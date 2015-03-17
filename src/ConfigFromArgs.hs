module ConfigFromArgs where

import Grammar

import qualified System.Console.GetOpt as Opt
import Control.Monad
import Text.Read

import Control.Applicative


data Config
	= Config {
		cfg_inputFormat :: GrammarFormat,
		cfg_outputFormat :: GrammarFormat
	}

data PartialCfg
	= PartialCfg {
		cfgPartial_inputFormat :: Maybe GrammarFormat,
		cfgPartial_outputFormat :: Maybe GrammarFormat
	}
emptyOptions = PartialCfg Nothing Nothing

configFromPartialCfg cfg =
	Config
		<$> cfgPartial_inputFormat cfg
		<*> cfgPartial_outputFormat cfg

configFromArgs :: [String] -> Maybe Config
configFromArgs =
	configFromPartialCfg <=< argsToPartialCfg

argsToPartialCfg :: [String] -> Maybe PartialCfg
argsToPartialCfg args =
	case Opt.getOpt Opt.RequireOrder optDescrList args of
		(options, nonOptions, []) ->
			let
				f = foldl (\f g -> f >=> g) return options -- :: [PartialCfg -> Maybe PartialCfg] -> PartialCfg -> Maybe PartialCfg
			in
				f $ emptyOptions
		(_,_, errMessages) -> Nothing --errMessages
 
optDescrList :: [Opt.OptDescr (PartialCfg -> Maybe PartialCfg)]
optDescrList =
	[ Opt.Option [] ["input-format"] (Opt.ReqArg inputF "FORMAT") "one of default, bnf, bnfe"
	, Opt.Option [] ["output-format"] (Opt.ReqArg outputF "FORMAT") "one of default, bnf, bnfe"
	--, Opt.Option [] ["prod-sign"] (Opt.ReqArg prodSign "") "prod-sign"
	]

{-
prodSign :: String -> PartialCfg -> Maybe PartialCfg
prodSign arg cfg =
-}

inputF :: String -> PartialCfg -> Maybe PartialCfg
inputF arg cfg =
	do
		format <- parseFormat arg
		return $ cfg{ cfgPartial_inputFormat = Just format }

outputF :: String -> PartialCfg -> Maybe PartialCfg
outputF arg cfg =
	do
		format <- parseFormat arg
		return $ cfg{ cfgPartial_outputFormat = Just format }

parseFormat str =
	case str of
		"default"-> return $ Default
		"bnf" -> return $ BNF
		"bnfe" -> return $ BNFE
		_ -> Nothing
