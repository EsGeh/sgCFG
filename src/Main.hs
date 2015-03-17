module Main where

import Grammar
import Types
import ConfigFromArgs

import System.Environment (getArgs)
import Control.Monad


main = do
	mConfig <- liftM configFromArgs getArgs
	config <- case mConfig of
		Nothing -> fail "error parsing arguments"
		Just config -> return config
	str <- getContents
	let errOrGrammar =
		fromTextAs (cfg_inputFormat config) str
		:: Either String Grammar
	case errOrGrammar of
		Left err -> putStrLn $ "error parsing grammar: " ++ err
		Right g ->
			putStrLn $ toTextAs (cfg_outputFormat config) g
			--putStrLn $ "successfully parsed grammar!"
