module Main where

import Parse.Token
import Grammar
import GroupedGrammar
import Types
import ConfigFromArgs
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)

import Utils.Logging

import qualified System.Environment as Env (getArgs, getProgName)
import Control.Monad
import Control.Monad.Except
--import Control.Monad.Writer
import Data.List
--import Data.Maybe(fromMaybe)


main :: IO ()
main =
	unpackTransformationMonad $
	do
		config <-
			configFromArgs =<< lift Env.getArgs
		progName <-
			liftIO Env.getProgName

		let outputCommands = cfg_output config :: [OutputSpec]
		output <-
			calcOutput progName config getContents outputCommands
		liftIO $ putStrLn output

calcOutput ::
	String -> Config -> IO String
	-> [OutputSpec]
	-> TransformationMonadT IO String
calcOutput progName config input outputCommands =
	do
		let
			inParseFormat =
				let inFormat = formatState_format $ cfg_inputFormat config
				in parseFormatFromGrammarFormat $ inFormat
		str <- liftIO input
		fmap concat $
			flip mapM outputCommands $
			\cmd -> 
			{-
			outputAction :: OutputSpec -> ExceptT String IO String
			outputAction cmd =
			-}
				case cmd of
					OutputHelp ->
						return $ usageString progName
					OutputOptions ->
						return $ show $ config
					OutputTokenStream ->
						do
							tokens <- ExceptT $ return $ tokensFromStr inParseFormat str
							return $ tokStreamToText tokens
					OutputGrammar info ->
						let format = formatState_format $ outputGrammar_format info
						in
							ExceptT . return $
							fmap (toTextAs format) $
							grammarFromTokens 
							=<<
							tokensFromStr inParseFormat str
					OutputGroupedGrammar info ->
						let format = formatState_format $ outputGrammar_format info
						in
							(
								fmap (toTextAs format) .
								applyTransformations (outputGrammar_transformations info) .
								toProdAndSymbolsTagged prodTag_empty .
								toTaggedGrammar
							)
							=<<
							either throwError return (
								groupedGrammarFromTokens =<<
								tokensFromStr inParseFormat str
							)

applyTransformations ::
	MonadLog m =>
	[Transformation] -> GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag] -> TransformationMonadT m (GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag])
applyTransformations t =
	(foldl (>=>) return $ map applyTransformation t)

unpackTransformationMonad :: TransformationMonadT IO () -> IO ()
unpackTransformationMonad e =
	do
		eitherErr <- runExceptT e
		case eitherErr of
			Left err ->
				putStrLn $ err
				--exit
			Right x -> return x

tokStreamToText s =
	intercalate " " $
	foldl conc [] $
	s
	where
		conc l next =
			l ++
			case tokenType next of
				SepTokenType -> [pretty next] ++ ["\n"]
				_ -> [pretty next]

{-
failOrVal :: Monad m => String -> Either String a -> m a
failOrVal msg x =
	case x of
		Left err -> fail $ msg ++ err
		Right val -> return val
-}
