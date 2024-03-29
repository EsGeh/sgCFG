{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Config.Types where

import Grammar.Types
import GrammarFormat
import GroupedGrammar
import Types
import Utils

import Control.Monad.Identity
import Text.Read


data Config
	= Config {
		cfg_inputFormat :: FormatState,
		cfg_output :: [OutputSpec]
	}
	deriving (Show)

cfgMapToInputFormatM ::
	Monad m =>
	(FormatState -> m FormatState) -> Config -> m Config
cfgMapToInputFormatM f cfg = do
	new <- f (cfg_inputFormat cfg)
	return $ cfg{ cfg_inputFormat = new }

cfgMapToOutputM ::
	Monad m =>
	([OutputSpec] -> m [OutputSpec])
	-> Config -> m Config
cfgMapToOutputM f cfg = do
	new <- f (cfg_output cfg)
	return $ cfg{ cfg_output = new }

cfgMapToInputFormat ::
	(FormatState -> FormatState)
	-> Config -> Config
cfgMapToInputFormat f = runIdentity . cfgMapToInputFormatM (return . f)

cfgMapToOutput ::
	([OutputSpec] -> [OutputSpec])
	-> Config -> Config
cfgMapToOutput f = runIdentity . cfgMapToOutputM (return . f)

defConfig :: Config
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

defOutputGrammarInfo :: DefaultFormat -> OutputGrammarInfo
defOutputGrammarInfo f = OutputGrammarInfo (defFormatState f) [] False

outputGrammarInfo_mapToFormatM ::
	Monad m =>
	(FormatState -> m FormatState)
	-> OutputGrammarInfo -> m OutputGrammarInfo
outputGrammarInfo_mapToFormatM f x = do
	new <- f $ outputGrammar_format x
	return $ x{ outputGrammar_format = new }

outputGrammarInfo_mapToTransformationsM ::
	Monad m =>
	([Transformation] -> m [Transformation])
	-> OutputGrammarInfo -> m OutputGrammarInfo
outputGrammarInfo_mapToTransformationsM f x = do
	new <- f $ outputGrammar_transformations x
	return $ x{ outputGrammar_transformations = new }

outputGrammarInfo_mapToFormat ::
	(FormatState -> FormatState)
	-> OutputGrammarInfo -> OutputGrammarInfo
outputGrammarInfo_mapToFormat f = runIdentity . outputGrammarInfo_mapToFormatM (return . f)

outputGrammarInfo_mapToTransformations ::
	([Transformation] -> [Transformation])
	-> OutputGrammarInfo -> OutputGrammarInfo
outputGrammarInfo_mapToTransformations f = runIdentity . outputGrammarInfo_mapToTransformationsM (return . f)


data FormatState
	= FormatState {
		formatState_format :: GrammarFormat,
		-- list of parameters that have been touched by the user:
		formatState_paramsChanged :: [FormatParam]
	}
	deriving (Show)

defFormatState :: DefaultFormat -> FormatState
defFormatState f = FormatState (defaultFormat f) []

formatState_mapToFormat ::
	(GrammarFormat -> GrammarFormat)
	-> FormatState -> FormatState
formatState_mapToFormat f x = x{ formatState_format = f (formatState_format x) }

formatState_mapToParamsChanged ::
	([FormatParam] -> [FormatParam])
	-> FormatState -> FormatState
formatState_mapToParamsChanged f x = x{ formatState_paramsChanged = f (formatState_paramsChanged x) }


data FormatParam
	= LeftSideFormatParam
	| RightSideFormatParam
	| VarFormatParam
	| TerminalFormatParam
	| OrFormatParam
	| ArrowFormatParam
	| LineCommentFormatParam
	deriving (Eq, Show)

instance FromPretty Transformation where
	-- fromPretty :: String -> Either ParseError Transformation
	fromPretty str =
		case parseTransformationDescr str of
			("annotate", ["loops"]) ->
				return $ Annotate $ AnnotateWithLoops
			("annotate", ["first"]) ->
				return $ Annotate $ AnnotateWithFirstSet
			("leftFactor", [varScheme]) ->
				fmap LeftFactor $ fromPretty varScheme
			("leftFactor_full", [whileCond, negate', regex, varScheme]) ->
				LeftFactor_Full
					<$> fromPretty whileCond
					<*> (
						VarCondition <$>
						isEqualOrEmpty "not" negate'
						<*> return regex
					)
					<*> fromPretty varScheme
			("elimLeftRec", [varScheme]) ->
				fmap ElimLeftRecur $ fromPretty varScheme
			("elimLeftRec_noEpsilon", [varScheme]) ->
				fmap ElimLeftRecurNoEpsilon $ fromPretty varScheme
			("elimLeftRec_full", [negate',regex,varScheme]) ->
				ElimLeftRecur_Full <$>
				(
					VarCondition <$>
					isEqualOrEmpty "not" negate'
					<*> return regex
				)
				<*>
				fromPretty varScheme
			("elimLeftRec_noEpsilon_full", [negate',regex,varScheme]) ->
				ElimLeftRecurNoEpsilon_Full <$>
				(
					VarCondition <$>
					isEqualOrEmpty "not" negate'
					<*> return regex
				)
				<*>
				fromPretty varScheme
			("breakRules", [maxLength, varScheme]) ->
				fmap (uncurry BreakRules) $ liftM2 (,) (return $ read maxLength) (fromPretty varScheme)
			("unfold", [negate',regex]) ->
				do
					doNegate <- isEqualOrEmpty "not" negate'
					return $ Unfold $
						VarCondition {
							varCond_negate = doNegate,
							varCond_regex = regex
						}
			("elimEpsilon",[]) ->
				return $ ElimEpsilon
			("insert", [posStr,prodsStr]) ->
				do
					pos <- case posStr of
						"start" -> return $ GrammarPosBeginning
						"end" -> return $ GrammarPosEnding
						_ -> throwError $
							concat $ 
							[ "unknown position "
							, posStr
							]
					prods <-
						fmap fromGrammar $
						fromTextAs (defaultFormat Default) prodsStr
					return $
						InsertProductions $
						InsertProductionsParams {
							insertProdsParams_position = pos,
							insertProdsParams_productions = prods
						}
			("delete", [negate',regex]) ->
				do
					doNegate <- isEqualOrEmpty "not" negate'
					return $
						DeleteProductions $
						VarCondition {
							varCond_negate = doNegate,
							varCond_regex = regex
						}
			("removeDoubleProds", []) ->
				return $ RemoveDoubleProds
			("addActionSymbols",[counterInit]) ->
				AddActionSymbols <$>
					readEither counterInit
					--(fromMaybe (Left $ "addActionSymbols expects an integer") $ readMaybe counterInit)
			("subGrammar", [var]) ->
				return $ SubGrammar $ Var var
			("unused", []) ->
				return $ UnusedRules
			("findDeadEnds", []) ->
				return $ FindDeadEnds
			x -> Left $
				concat $
				[ "fromPretty error for Transformation, got "
				, show x
				]

isEqualOrEmpty :: MonadError String m => String -> String -> m Bool
isEqualOrEmpty pattern str =
	case str of
		str' | str'==pattern -> return $ True
		"" -> return $ False
		_ -> throwError $
			concat $
			[ "isEqualOrEmpty failed with "
			, pattern
			, " and "
			, str
			]

parseTransformationDescr :: String -> (String, [String])
parseTransformationDescr =
	mapSnd (
		(\x -> case x of { [""] -> []; _ -> x })
		.
		splitBy ',' . init . drop 1
	)
	.
	span (/='(')

splitBy :: forall a .
	Eq a => a -> [a] -> [[a]]
splitBy c str = 
	fst $
	splitBy' [[]] str
	where
		splitBy' :: [[a]] -> [a] -> ([[a]], [[a]])
		splitBy' processed remaining =
			case remaining of
				(x:xs) | x == c ->
					splitBy'
						(processed ++ [[]])
						xs
				(x:xs) ->
					splitBy'
						(
							case processed of
								[] -> [[x]]
								_ -> init processed ++ [last processed ++ [x]]
						)
						xs
				_ ->
					(processed, [])

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
			"left" -> return LeftSideFormatParam
			"right" -> return RightSideFormatParam
			"var" -> return VarFormatParam
			"terminal" -> return TerminalFormatParam
			"or" -> return OrFormatParam
			"arrow" -> return ArrowFormatParam
			"lineComment" -> return LineCommentFormatParam
			_ -> Left $ concat $ ["unknown FormatParam", "\"", str, "\""]

mapToHeadMaybe :: (a -> a) -> [a] -> Maybe [a]
mapToHeadMaybe f list =
	case list of
		[] -> Nothing
		(x:xs) -> do
			return $ (f x):xs

mapToHeadOrError :: MonadError p m => p -> (a -> a) -> [a] -> m [a]
mapToHeadOrError errMsg f list =
	case list of
		[] -> throwError $ errMsg
		(x:xs) -> do
			return $ (f x):xs
