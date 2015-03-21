module ConfigFromArgs(
	module ConfigFromArgs,
	module Config.Types
) where

import Config.Types
import Grammar
import GrammarFormat
import Types
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

optDescrList :: [Opt.OptDescr (Config -> Maybe Config)]
optDescrList =
	[ Opt.Option ['h'] ["help"] (Opt.NoArg (\cfg -> return $ cfgMapToOutput (OutputHelp:) cfg)) "print help"
	, Opt.Option ['i'] ["input-format", "if"] (Opt.ReqArg inputF "FORMAT") "input format (append \"--change-input-format\" to modify)"
	, Opt.Option [] ["change-input-format", "cif"] (Opt.ReqArg changeInputFormat "CHANGE_FORMAT") "change input format"
	, Opt.Option [] ["output-options"]
		(Opt.NoArg $ return . cfgMapToOutput (OutputOptions:))
		"output options"
	, Opt.Option [] ["output-tokens"]
		(Opt.NoArg outputTokens)
		"output the input stream as stream of tokens"
	, Opt.Option ['o'] ["output"]
		(Opt.ReqArg outputF "FORMAT")
		"input format (append \"--change-output-format\" to modify)"
	, Opt.Option ['g'] ["output-grouped"]
		(Opt.ReqArg outputGrouped "FORMAT")
		"input format (append \"--change-output-format\" to modify)"
	, Opt.Option [] ["change-output-format", "cof"] (Opt.ReqArg changeOutputFormat "CHANGE_FORMAT") "change output format"
	, Opt.Option ['t'] ["transformation"] (Opt.ReqArg transformation "GRAMMAR_TRANSFORMATION") "apply a transformation on grammar"
	]

inputF :: String -> Config -> Maybe Config
inputF arg cfg =
	do
		format <- liftM defaultFormat (either (const Nothing) Just $ fromPretty arg)
		return $ cfg{ cfg_inputFormat = FormatState format [] }

outputTokens :: Config -> Maybe Config
outputTokens cfg =
	return $ cfgMapToOutput ((OutputTokenStream):) cfg

outputF :: String -> Config -> Maybe Config
outputF arg cfg =
	do
		format <- either (const Nothing) Just $ fromPretty arg
		return $
			cfgMapToOutput ((OutputGrammar $ defOutputGrammarInfo format):) cfg
		--return $ cfgMapToOutput ((OutputGrammar (format,[])):) cfg
		--return $ cfg{ cfg_inputFormat = format }

outputGrouped :: String -> Config -> Maybe Config
outputGrouped arg cfg =
	do
		format <- either (const Nothing) Just $ fromPretty arg
		return $
			cfgMapToOutput ((OutputGroupedGrammar $ defOutputGrammarInfo format):) cfg
		--return $ cfgMapToOutput ((OutputGroupedGrammar (format, [])):) cfg
		--return $ cfg{ cfg_inputFormat = format }

changeInputFormat :: String -> Config -> Maybe Config
changeInputFormat arg cfg = do
	(key, val) <- either (const Nothing) Just $ parseKeyValue arg
	return $ cfgMapToInputFormat (applyFormatChange key val) cfg

parseKeyValue str =
	case mapSnd (drop 1) $ span (/='=') str of
		(keyStr, val) -> do
			key <- fromPretty keyStr
			return $ (key, val)

transformation arg cfg =
	do
		cfgMapToOutputM (changeF arg) cfg
	where
			changeF :: String -> [OutputSpec] -> Maybe [OutputSpec]
			changeF str outputCommands =
				flip mapToHeadMaybe outputCommands $ \spec ->
					case spec of
						OutputGroupedGrammar info -> 
							return . OutputGroupedGrammar
							=<<
							outputGrammarInfo_mapToTransformationsM (changeTransformations str) info
							where
								changeTransformations str list = do
									transformation <- either (const Nothing) Just $ fromPretty str
									return $ (transformation:list)
						_ -> Nothing

changeOutputFormat :: String -> Config -> Maybe Config
changeOutputFormat arg cfg =
	do 
		(key, val) <- either (const Nothing) Just $ parseKeyValue arg
		cfgMapToOutputM (changeF key val) cfg
		where
			changeF :: FormatParam -> String -> [OutputSpec] -> Maybe [OutputSpec]
			changeF key val outputCommands =
				flip mapToHeadMaybe outputCommands $ \spec ->
					case spec of
						OutputGrammar info  -> 
							return $ (
								(OutputGrammar $ outputGrammarInfo_mapToFormat (applyFormatChange key val) info)
							)
						OutputGroupedGrammar info -> 
							return $ (
								(OutputGroupedGrammar $ outputGrammarInfo_mapToFormat (applyFormatChange key val) info)
							)
						_ -> Nothing
			{-
			changeF :: FormatParam -> String -> [OutputSpec] -> Maybe [OutputSpec]
			changeF key val outputCommands =
				case outputCommands of
					(spec:rest) -> 
						case spec of
							OutputGrammar info  -> 
								return $ (
									(OutputGrammar $ outputGrammarInfo_mapToFormat (applyFormatChange key val) info)
									:
									rest
								)
							OutputGroupedGrammar info -> 
								return $ (
									(OutputGroupedGrammar $ outputGrammarInfo_mapToFormat (applyFormatChange key val) info)
									:
									rest
								)
								--return $ ((OutputGroupedGrammar $ (formatState_mapToFormat $ applyFormatChange key val) info): rest)
								--return $ (OutputGroupedGrammar (applyFormatChange key val format): rest)
					_ -> Nothing
			-}

applyFormatChange :: FormatParam -> String -> FormatState -> FormatState
applyFormatChange param str outputInfo =
	let 
	in
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
					False -> FormatState (f (const [str]) format) (param:overwrittenParams)
					True -> FormatState (f (str:) format) overwrittenParams
			format = formatState_format outputInfo
			overwrittenParams = formatState_paramsChanged outputInfo
{-
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
-}
