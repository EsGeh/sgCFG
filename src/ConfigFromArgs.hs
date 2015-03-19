module ConfigFromArgs where

import Grammar
import GrammarFormat
import Utils (mapFst, mapSnd)

import qualified System.Console.GetOpt as Opt
import Control.Monad
import Text.Read

import Control.Applicative

configFromArgs args =
	case Opt.getOpt Opt.RequireOrder optDescrList args of
		(options, nonOptions, []) ->
			let
				f = foldl (\f g -> f >=> g) return options -- :: [PartialCfg -> Maybe PartialCfg] -> PartialCfg -> Maybe PartialCfg
			in
				f $ defConfig
		(_,_, errMessages) -> Nothing --errMessages

usageString progName =
	unwords $
	[ Opt.usageInfo header optDescrList
	, unlines $
	  [ "where"
	  , "FORMAT: one of default, bnf, bnfe"
	  , "CHANGE_FORMAT: <param>=<str>"
	  , "  param can be one of: or, arrow, lineComment"
	  ]
	]
	where
		header =
			unlines $
			[ concat [ "usage: ", progName , " OPTIONS" ]
			, "OPTIONS:"
			]

data Config
	= Config {
		cfg_inputFormat :: (GrammarFormat, [FormatParam]),
		cfg_output :: [OutputSpec]
		--cfg_outputFormat :: GrammarFormat
	}
	deriving (Show)
cfgMapToInputFormat f cfg = cfg{ cfg_inputFormat = f (cfg_inputFormat cfg) }
cfgMapToOutput f cfg = cfg{ cfg_output = f (cfg_output cfg) }
defConfig =
	Config {
		cfg_inputFormat = (defaultFormat Default, []),
		cfg_output = []
	}

data OutputSpec
	= OutputHelp
	| OutputTokenStream
	| OutputOptions
	| OutputGrammar (GrammarFormat, [FormatParam])
	| OutputGroupedGrammar (GrammarFormat, [FormatParam])
	deriving (Show)

data FormatParam
	= Or
	| Arrow
	| LineComment
	deriving (Eq, Show)

formatParamFromStr str =
	case str of
		"or" -> return Or
		"arrow" -> return Arrow
		"lineComment" -> return LineComment
		_ -> Nothing

applyFormatChange param str (format, overwrittenParams) =
	case param of
		Or ->
			changeF gFormatMapToOr
		Arrow ->
			changeF gFormatMapToArrow 
		LineComment ->
			changeF gFormatMapToLineComment
	where
		changeF f =
			case param `elem` overwrittenParams of
				False -> (f (const [str]) format, param:overwrittenParams)
				True -> (f (str:) format, overwrittenParams)

optDescrList :: [Opt.OptDescr (Config -> Maybe Config)]
optDescrList =
	[ Opt.Option ['h'] ["help"] (Opt.NoArg (\cfg -> return $ cfgMapToOutput (OutputHelp:) cfg)) "print help"
	, Opt.Option ['i'] ["input-format", "if"] (Opt.ReqArg inputF "FORMAT") "input format (append \"--change-input-format\" to modify)"
	, Opt.Option [] ["change-input-format", "cif"] (Opt.ReqArg changeInputFormat "CHANGE_FORMAT") "change input format"
	, Opt.Option [] ["output-options"]
		(Opt.NoArg $ return . cfgMapToOutput (OutputOptions:))
		"output options"
	, Opt.Option ['t'] ["output-tokens"]
		(Opt.NoArg outputTokens)
		"output the input stream as stream of tokens"
	, Opt.Option ['o'] ["output"]
		(Opt.ReqArg outputF "FORMAT")
		"input format (append \"--change-output-format\" to modify)"
	, Opt.Option ['g'] ["output-grouped"]
		(Opt.ReqArg outputGrouped "FORMAT")
		"input format (append \"--change-output-format\" to modify)"
	, Opt.Option [] ["change-output-format", "cof"] (Opt.ReqArg changeOutputFormat "CHANGE_FORMAT") "change output format"
	]

inputF :: String -> Config -> Maybe Config
inputF arg cfg =
	do
		format <- liftM defaultFormat (parseFormat arg)
		return $ cfg{ cfg_inputFormat = (format, []) }

outputTokens :: Config -> Maybe Config
outputTokens cfg =
	return $ cfgMapToOutput ((OutputTokenStream):) cfg

outputF :: String -> Config -> Maybe Config
outputF arg cfg =
	do
		format <- liftM defaultFormat (parseFormat arg)
		return $ cfgMapToOutput ((OutputGrammar (format,[])):) cfg
		--return $ cfg{ cfg_inputFormat = format }

outputGrouped :: String -> Config -> Maybe Config
outputGrouped arg cfg =
	do
		format <- liftM defaultFormat (parseFormat arg)
		return $ cfgMapToOutput ((OutputGroupedGrammar (format, [])):) cfg
		--return $ cfg{ cfg_inputFormat = format }

changeInputFormat :: String -> Config -> Maybe Config
changeInputFormat arg cfg =
	case mapSnd (drop 1) $ span (/='=') arg of
		(keyStr, val) -> do
			key <- formatParamFromStr keyStr
			return $ cfgMapToInputFormat (applyFormatChange key val) cfg
		--_ -> Nothing

changeOutputFormat :: String -> Config -> Maybe Config
changeOutputFormat arg cfg =
	case mapSnd (drop 1) $ span (/='=') arg of
		(keyStr, val) -> do
			key <- formatParamFromStr keyStr
			let outSpecs = cfg_output cfg
			newSpecs <- changeF key val outSpecs
			return $ cfg{ cfg_output = newSpecs }
			where
				changeF :: FormatParam -> String -> [OutputSpec] -> Maybe [OutputSpec]
				changeF key val outputCommands =
					case outputCommands of
						(spec:rest) -> 
							case spec of
								OutputGrammar format -> 
									return $ (OutputGrammar (applyFormatChange key val format): rest)
								OutputGroupedGrammar format -> 
									return $ (OutputGroupedGrammar (applyFormatChange key val format): rest)
						_ -> Nothing

parseFormat str =
	case str of
		"default"-> return $ Default
		"bnf" -> return $ BNF
		"bnfe" -> return $ BNFE
		_ -> Nothing

mapIfSet list f cfg =
	case list of
		[] -> cfg
		_ -> f cfg
