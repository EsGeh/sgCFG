module ConfigFromArgs where

import Grammar
import GrammarFormat

import qualified System.Console.GetOpt as Opt
import Control.Monad
import Text.Read

import Control.Applicative

data Config
	= Config {
		cfg_inputFormat :: GrammarFormat,
		cfg_outputFormat :: GrammarFormat
	}
	deriving (Show)
cfgMapToInputFormat f cfg = cfg{ cfg_inputFormat = f (cfg_inputFormat cfg) }
cfgMapToOutputFormat f cfg = cfg{ cfg_outputFormat = f (cfg_outputFormat cfg) }

data PartialCfg
	= PartialCfg {
		cfgPartial_inputFormat :: Maybe DefaultFormat,
		cfgPartial_outputFormat :: Maybe DefaultFormat,
		cfgPartial_inProdSign :: [String],
		cfgPartial_outProdSign :: [String],
		cfgPartial_inLineComment :: [String]
	}
	deriving (Show)
partialCfgMapToInProdSign f cfg = cfg{ cfgPartial_inProdSign = f (cfgPartial_inProdSign cfg) }
partialCfgMapToOutProdSign f cfg = cfg{ cfgPartial_outProdSign = f (cfgPartial_outProdSign cfg) }
partialCfgMapToInLineComment f cfg = cfg{ cfgPartial_inLineComment = f (cfgPartial_inLineComment cfg) }
emptyOptions = PartialCfg Nothing Nothing [] [] []

configFromPartialCfg :: PartialCfg -> Maybe Config
configFromPartialCfg cfg =
	liftM (writeOutProdSign $ cfgPartial_outProdSign cfg) $
	liftM (writeInProdSign $ cfgPartial_inProdSign cfg) $
	liftM (writeInLineComment $ cfgPartial_inLineComment cfg) $
	Config
		<$> (liftM defaultFormat $ cfgPartial_inputFormat cfg)
		<*> (liftM defaultFormat $ cfgPartial_outputFormat cfg)
	where
		writeOutProdSign l cfg =
			mapIfSet l (cfgMapToOutputFormat $ gFormatMapToProdSign $ const l) cfg
		writeInProdSign l cfg = 
			mapIfSet l (cfgMapToInputFormat $ gFormatMapToProdSign $ const l) cfg
		writeInLineComment l cfg =
			mapIfSet l (cfgMapToInputFormat $ gFormatMapToLineComment $ const l) cfg
		mapIfSet list f cfg =
			case list of
				[] -> cfg
				_ -> f cfg

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
	, Opt.Option [] ["in-prod-sign"] (Opt.ReqArg inProdSign "") "prod-sign"
	, Opt.Option [] ["out-prod-sign"] (Opt.ReqArg outProdSign "") "prod-sign"
	, Opt.Option [] ["in-line-comment"] (Opt.ReqArg inLineComment "") "symbol to start one line comment"
	]

inProdSign :: String -> PartialCfg -> Maybe PartialCfg
inProdSign arg cfg =
	return $ (partialCfgMapToInProdSign $ (arg:)) cfg

outProdSign :: String -> PartialCfg -> Maybe PartialCfg
outProdSign arg cfg =
	return $ (partialCfgMapToOutProdSign $ (arg:)) cfg

inLineComment :: String -> PartialCfg -> Maybe PartialCfg
inLineComment arg cfg =
	return $ (partialCfgMapToInLineComment $ (arg:)) cfg

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
