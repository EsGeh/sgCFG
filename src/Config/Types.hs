module Config.Types where

import GrammarTypes
import GrammarFormat
import GroupedGrammar.Transformations
import Types
import Utils

import Control.Monad.Identity


data Config
	= Config {
		cfg_inputFormat :: FormatState,
		cfg_output :: [OutputSpec]
		--cfg_outputFormat :: GrammarFormat
	}
	deriving (Show)
cfgMapToInputFormatM f cfg = do
	new <- f (cfg_inputFormat cfg)
	return $ cfg{ cfg_inputFormat = new }
--cfgMapToInputFormat f cfg = cfg{ cfg_inputFormat = f (cfg_inputFormat cfg) }
cfgMapToOutputM f cfg = do
	new <- f (cfg_output cfg)
	return $ cfg{ cfg_output = new }
--cfgMapToOutput f cfg = cfg{ cfg_output = f (cfg_output cfg) }

cfgMapToInputFormat f = runIdentity . cfgMapToInputFormatM (return . f)
cfgMapToOutput f = runIdentity . cfgMapToOutputM (return . f)
defConfig =
	Config {
		cfg_inputFormat = FormatState (defaultFormat Default) [],
		cfg_output = []
	}

data OutputSpec
	= OutputHelp
	| OutputTokenStream
	| OutputOptions
	| OutputGrammar OutputGrammarInfo
	| OutputGroupedGrammar OutputGrammarInfo
	deriving (Show)

data OutputGrammarInfo
	= OutputGrammarInfo {
		outputGrammar_format :: FormatState,
		outputGrammar_transformations :: [Transformation],
		outputGrammar_asTree :: Bool
	}
	deriving (Show)
defOutputGrammarInfo f = OutputGrammarInfo (defFormatState f) [] False
outputGrammarInfo_mapToFormatM f x = do
	new <- f $ outputGrammar_format x
	return $ x{ outputGrammar_format = new }
outputGrammarInfo_mapToTransformationsM f x = do
	new <- f $ outputGrammar_transformations x
	return $ x{ outputGrammar_transformations = new }
outputGrammarInfo_mapToFormat f = runIdentity . outputGrammarInfo_mapToFormatM (return . f)
outputGrammarInfo_mapToTransformations f = runIdentity . outputGrammarInfo_mapToTransformationsM (return . f)
{-
outputGrammarInfo_mapToFormat f x = x{ outputGrammar_format = f (outputGrammar_format x) }
outputGrammarInfo_mapToTransformations f x = x{ outputGrammar_transformations = f (outputGrammar_transformations x) }
-}

data FormatState
	= FormatState {
		formatState_format :: GrammarFormat,
		formatState_paramsChanged :: [FormatParam]
	}
	deriving (Show)
defFormatState f = FormatState (defaultFormat f) []
formatState_mapToFormat f x = x{ formatState_format = f (formatState_format x) }
formatState_mapToParamsChanged f x = x{ formatState_paramsChanged = f (formatState_paramsChanged x) }

data FormatParam
	= Or
	| Arrow
	| LineComment
	deriving (Eq, Show)

instance FromPretty Transformation where
	fromPretty str =
		case mapSnd (drop 1) $ span (/='=') str of
			("annotate", "loops") ->
				return $ Annotate $ AnnotateWithLoops
			("subGrammar", var) ->
				return $ SubGrammar $ Var var
			("unused", []) ->
				return $ UnusedRules
			_ -> Left $ "fromPretty error for Transformation"

instance FromPretty DefaultFormat where
	fromPretty str =
		case str of
			"default"-> return $ Default
			"bnf" -> return $ BNF
			"bnfe" -> return $ BNFE
			_ -> Left $ concat $ ["unknown DefaultFormat", "\"", str, "\""]

instance FromPretty FormatParam where
	fromPretty str =
		case str of
			"or" -> return Or
			"arrow" -> return Arrow
			"lineComment" -> return LineComment
			_ -> Left $ concat $ ["unknown FormatParam", "\"", str, "\""]

mapToHeadMaybe f list =
	case list of
		[] -> Nothing
		(x:xs) -> do
			new <- f x
			return $ new:xs

mapToHeadM f list =
	case list of
		[] -> return []
		(x:xs) -> do
			new <- f x
			return $ new:xs

mapToHead f = runIdentity . mapToHeadM (return . f)