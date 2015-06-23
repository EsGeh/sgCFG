module Main where

import Parse.Token
import Grammar
import GroupedGrammar
import Types
import ConfigFromArgs
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)

import System.Environment (getArgs)
import Control.Monad
import Data.List

progName = "sgCFG"


main :: IO ()
main = do
	mConfig <- liftM configFromArgs getArgs
	config <- case mConfig of
		Nothing -> fail "error parsing arguments"
		Just config -> return config
	str <- getContents
	-- parse input:
	let
		inParseFormat =
			let inFormat = formatState_format $ cfg_inputFormat config
			in parseFormatFromGrammarFormat $ inFormat
		errOrTokens = tokensFromStr inParseFormat str
		errOrGrammar = grammarFromTokens =<< errOrTokens
		errOrGroupedGrammar = groupedGrammarFromTokens =<< errOrTokens
	-- calculate output:
	let
		outputCommands = cfg_output config :: [OutputSpec]
		outputAction :: OutputSpec -> IO ()
		outputAction cmd =
			case cmd of
				OutputHelp ->
					putStrLn $ usageString progName
				OutputTokenStream ->
					putStrLn . tokStreamToText =<< failOrVal "error parsing token stream" errOrTokens
				OutputOptions ->
					putStrLn . show $ config
				OutputGrammar info ->
					let format = formatState_format $ outputGrammar_format info
					in
						putStrLn . toTextAs format =<< failOrVal "error parsing grammar" errOrGrammar
				OutputGroupedGrammar info ->
					let
						format = formatState_format $ outputGrammar_format info
						asTree = outputGrammar_asTree info
					in
						(
							putStrLn .
							if not asTree
								then toTextAs format
								else toTextAsTree format
						)
						=<<
						return . maybe (error "could not apply transformations") id . applyTransformations (outputGrammar_transformations info) . toTaggedGrammar
						=<<
						failOrVal "error parsing grammar" errOrGroupedGrammar
	sequence_ $ map outputAction outputCommands

applyTransformations :: [Transformation] -> GroupedGrammarTagged -> Maybe GroupedGrammarTagged
applyTransformations t =
	(foldl (>=>) return $ map applyTransformation t)

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

failOrVal :: Monad m => String -> Either String a -> m a
failOrVal msg x =
	case x of
		Left err -> fail $ msg ++ err
		Right val -> return val
