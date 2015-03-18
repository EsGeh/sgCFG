module Main where

import Parse.Token
import Grammar
import Types
import ConfigFromArgs
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)

import System.Environment (getArgs)
import Control.Monad
import Data.List


main = do
	mConfig <- liftM configFromArgs getArgs
	config <- case mConfig of
		Nothing -> fail "error parsing arguments"
		Just config -> return config
	putStrLn $ show config
	str <- getContents
	let errOrGrammar =
		fromTextAs (cfg_inputFormat config) str
		:: Either String Grammar
	case errOrGrammar of
		Left err ->
			do
				putStrLn $ "error parsing grammar:"
				putStrLn $ err
				case tokenStream config str of
					Left _ -> return ()
					Right stream -> do
						putStrLn $ "tokenStream: "
						putStrLn $ tokStreamToText stream
		Right g -> do
			putStrLn $ toTextAs (cfg_outputFormat config) g
			{-
			case tokenStream config str of
				Left _ -> return ()
				Right stream -> do
					putStrLn $ "tokenStream: "
					putStrLn $ tokStreamToText stream
			--putStrLn $ "successfully parsed grammar!"
			-}
	where
		tokenStream config str = tokensFromStr (parseFormatFromGrammarFormat $ cfg_inputFormat config) str

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
