{-# LANGUAGE RankNTypes #-}
module ConfigFromArgs(
	module ConfigFromArgs,
	module Config.Types
) where

import Config.Types
--import Grammar
import GrammarFormat
import Types
import Utils (mapSnd)

import qualified System.Console.GetOpt as Opt
import Control.Monad
--import Text.Read

--import Control.Applicative

configFromArgs :: [String] -> Maybe Config
configFromArgs args =
	case Opt.getOpt Opt.RequireOrder optDescrList args of
		(options, _, []) ->
			let
				f = foldl (\f g -> f >=> g) return options -- :: [PartialCfg -> Maybe PartialCfg] -> PartialCfg -> Maybe PartialCfg
			in
				f $ defConfig
		(_,_,_) -> Nothing --errMessages

usageString :: String -> String
usageString progName =
	unwords $
	[ Opt.usageInfo header optDescrList
	, unlines $
	  [ "where"
	  , "FORMAT: one of default, bnf, bnfe"
	  , "CHANGE_FORMAT: <param>=<str>"
	  , "  param can be one of: left, right, var, terminal, or, arrow, lineComment"
		, "GRAMMAR_TRANSFORMATION: can be one of:"
		, "  annotate=<val> where <val> one of loops, first"
		, "  leftFactor=NAMING_SCHEME"
		, "  elimLeftRec_dragon=NAMING_SCHEME"
		, "  subGrammar"
		, "  unused"
		, "NAMING_SCHEME can be any string <x>, or \"%v%n\""
		, "  algorithms which introduce new variables name them <prefix><suffix>."
		, "  * if NAMING_SCHEME == %v%n: the <prefix> is chosen on the left hand side of the rule"
		, "  * else: <prefix> = NAMING_SCHEME"
		, "  The <suffix> is an number increasing to avoid name clashes"
	  ]
	]
	where
		header =
			unlines $
			[ concat [ "usage: ", progName , " OPTIONS" ]
			, "OPTIONS"
			, "needed: -i, -o."
			, "appending -cif, cof changes input/output formats"
			, "if \"-o grouped\" is in the list, -t can be appended to apply transformations"
			]

optDescrList :: [Opt.OptDescr (Config -> Maybe Config)]
optDescrList =
	[ defineOption ['h'] ["help"] "print help" $
		Opt.NoArg $ \cfg -> return $ cfgMapToOutput (OutputHelp:) cfg
	, defineOption ['i'] ["input-format", "if"] "input format (append \"--change-input-format\" to modify)" $
		Opt.ReqArg inputF "FORMAT"
	, defineOption [] ["change-input-format", "cif"] "change input format" $
		Opt.ReqArg changeInputFormat "CHANGE_FORMAT"
	, defineOption [] ["output-options"] "output options" $
		Opt.NoArg $ return . cfgMapToOutput (OutputOptions:)
	, defineOption [] ["output-tokens"] "output the input stream as stream of tokens" $
		Opt.NoArg outputTokens
	, defineOption ['o'] ["output"] "output format (append \"--change-output-format\" to modify)" $
		Opt.ReqArg outputF "FORMAT"
	, defineOption ['g'] ["output-grouped"] "output format (append \"--change-output-format\" to modify)" $
		Opt.ReqArg outputGrouped "FORMAT"
	, defineOption [] ["change-output-format", "cof"] "change output format" $
		Opt.ReqArg changeOutputFormat "CHANGE_FORMAT"
	, defineOption [] ["tree"] "output grammar as tree" $
		Opt.NoArg outputTree
	, defineOption ['t'] ["transformation"] "apply a transformation on grammar" $
		Opt.ReqArg transformation "GRAMMAR_TRANSFORMATION"
	]
	where
		defineOption shortOpt longOpt descr transformation =
			Opt.Option shortOpt longOpt transformation descr

outputTree =
	cfgMapToOutputM $ mapToHeadMaybe $ \spec ->
		case spec of
			OutputGroupedGrammar info ->
				return $ OutputGroupedGrammar $ info{ outputGrammar_asTree = True }
			_ -> Nothing

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

outputGrouped :: String -> Config -> Maybe Config
outputGrouped arg cfg =
	do
		format <- either (const Nothing) Just $ fromPretty arg
		return $
			cfgMapToOutput ((OutputGroupedGrammar $ defOutputGrammarInfo format):) cfg

changeInputFormat :: String -> Config -> Maybe Config
changeInputFormat arg cfg = do
	(key, val) <- either (const Nothing) Just $ parseKeyValue arg
	cfgMapToInputFormatM (applyFormatChange key val) cfg

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
									return $ (list++[transformation])
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
							--return $
								liftM OutputGrammar $
									outputGrammarInfo_mapToFormatM (applyFormatChange key val) info
						OutputGroupedGrammar info -> 
							--return $
								liftM OutputGroupedGrammar $
									outputGrammarInfo_mapToFormatM (applyFormatChange key val) info
						_ -> Nothing

applyFormatChange :: FormatParam -> String -> FormatState -> Maybe FormatState
applyFormatChange param str outputInfo
	| param `elem` [LeftSideFormatParam, RightSideFormatParam, VarFormatParam, TerminalFormatParam] =
		let mapToField =
			case param of
				LeftSideFormatParam -> gFormatMapToLeftSide
				RightSideFormatParam -> gFormatMapToRightSide
				VarFormatParam -> gFormatMapToVar
				TerminalFormatParam -> gFormatMapToTerminal
				_ -> error "internal error"
		in
			surroundBy_fromText str
			>>=
			return .
			(\surroundByInfo -> changeF $
				\_ -> mapToField $ const $ Just surroundByInfo
			)
	| otherwise =
		let mapToField =
			case param of
				OrFormatParam -> gFormatMapToOr
				ArrowFormatParam -> gFormatMapToArrow
				LineCommentFormatParam -> gFormatMapToLineComment
				_ -> error "internal error"
		in
			return $
			changeF (\alreadySet -> mapToField (if alreadySet then (str:) else (const [str])))
	where
		changeF ::
			(Bool -> GrammarFormat -> GrammarFormat)
			-> FormatState
		changeF f =
			let alreadySet = param `elem` overwrittenParams
			in
				FormatState
					(f alreadySet format)
					(if alreadySet then overwrittenParams else param:overwrittenParams)
			{-
			case param `elem` overwrittenParams of
				False -> FormatState (f (const [str]) format) (param:overwrittenParams)
				True -> FormatState (f (str:) format) overwrittenParams
			-}
		format = formatState_format outputInfo
		overwrittenParams = formatState_paramsChanged outputInfo
