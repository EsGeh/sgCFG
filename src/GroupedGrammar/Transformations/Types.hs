{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.Types where

import GroupedGrammar.Types
import Grammar.Types
import Types
import Utils.Graph

import Text.Regex.TDFA
import qualified Data.Map as M
import Text.Read


data Transformation
	= Annotate AnnotateInfo
	| ElimLeftRecur VarScheme
	| ElimLeftRecurNoEpsilon VarScheme
	| ElimLeftRecur_Full VarCondition VarScheme
	| ElimLeftRecurNoEpsilon_Full VarCondition VarScheme
	| LeftFactor VarScheme
	| LeftFactor_Full FullLeftFactorIterateWhile VarCondition VarScheme
	| BreakRules Int VarScheme
	| Unfold VarCondition
	| ElimEpsilon
	| InsertProductions InsertProductionsParams
	| DeleteProductions VarCondition
	| AddActionSymbols Int
	| RemoveDoubleProds
	| SubGrammar SubGrammarInfo
	| UnusedRules
	| FindDeadEnds
	deriving (Show)

type SubGrammarInfo = Var
data AnnotateInfo
	= AnnotateWithLoops
	| AnnotateWithFirstSet
	deriving (Show)

{- |algorithms introducing new variables may take a parameter of this type
-}
data VarScheme
	= Const String -- constName + number
	| FromVar -- originalName + number
	deriving Show

data VarCondition = VarCondition {
	varCond_negate :: Bool,
	varCond_regex :: String
}
	deriving (Show)

data FullLeftFactorIterateWhile
	= IterateWhileDecreasing
	| IterateNTimes Int
	deriving( Show )

data InsertProductionsParams = InsertProductionsParams {
	insertProdsParams_productions :: [GroupedProduction],
	insertProdsParams_position :: GrammarPosition
}
	deriving (Show)

data GrammarPosition
	= GrammarPosBeginning
	| GrammarPosEnding
	deriving (Show)

varCondFromDescr :: VarCondition -> (Var -> Bool)
varCondFromDescr descr =
	let
		neg = varCond_negate descr
		regex = varCond_regex descr
	in
		(if neg then not else id)
		.
		(=~regex)
		.
		var_name

data GroupedGrammar_SeparateProdTags productionTag symbolTag =
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar :: GrammarGen (GroupedProductionTagged symbolTag),
		ggSeparateProdTags_ruleAnnotations :: M.Map Var productionTag
	}


ggSeparateProdTags_mapToGrammar ::
	(GrammarGen (GroupedProductionTagged symbolTag1) -> GrammarGen (GroupedProductionTagged symbolTag2))
	-> GroupedGrammar_SeparateProdTags productionTag symbolTag1
	-> GroupedGrammar_SeparateProdTags productionTag symbolTag2
ggSeparateProdTags_mapToGrammar =
	fromMonadicLens ggSeparateProdTags_mapToGrammarM

ggSeparateProdTags_mapToRuleAnnotations ::
	(M.Map Var productionTag1 -> M.Map Var productionTag2)
	-> GroupedGrammar_SeparateProdTags productionTag1 symbolTag
	-> GroupedGrammar_SeparateProdTags productionTag2 symbolTag
ggSeparateProdTags_mapToRuleAnnotations =
	fromMonadicLens ggSeparateProdTags_mapToRuleAnnotationsM

ggSeparateProdTags_mapToGrammarM :: Monad m =>
	(GrammarGen (GroupedProductionTagged symbolTag1) -> m (GrammarGen (GroupedProductionTagged symbolTag2)))
 	-> GroupedGrammar_SeparateProdTags productionTag symbolTag1
	-> m (GroupedGrammar_SeparateProdTags productionTag symbolTag2)
ggSeparateProdTags_mapToGrammarM f g =
	do
		new <- f (ggSeparateProdTags_grammar g)
		return $ g{ ggSeparateProdTags_grammar = new }

ggSeparateProdTags_mapToRuleAnnotationsM :: Monad m =>
	(M.Map Var productionTag1 -> m (M.Map Var productionTag2))
	-> GroupedGrammar_SeparateProdTags productionTag1 symbolTag
	-> m (GroupedGrammar_SeparateProdTags productionTag2 symbolTag)
ggSeparateProdTags_mapToRuleAnnotationsM f g =
	do
		new <- f (ggSeparateProdTags_ruleAnnotations g)
		return $ g{ ggSeparateProdTags_ruleAnnotations = new }

type GrammarGraph symbolTag =
	Graph Symbol (GroupedProductionTagged symbolTag)

graphFromGroupedGrammar ::
	Ord key =>
	(prod -> key)
	-> (prod -> [key])
	-> GrammarGen prod -> Graph key prod
graphFromGroupedGrammar keyFromProd calcDest g =
	let list = map f $ fromGrammar g
	in
		graphFromEdges list
	where
		--f :: GroupedProduction -> (GroupedProduction, Symbol, [Symbol])
		f prod =
			( prod -- node
			, keyFromProd prod -- key
			, calcDest prod) -- [key]

toSeparateProdTags ::
	GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag -> GroupedGrammar_SeparateProdTags prodTag symbolTag
toSeparateProdTags g =
	GroupedGrammar_SeparateProdTags{
		ggSeparateProdTags_grammar = fmap value g,
		ggSeparateProdTags_ruleAnnotations = ruleAnnotations g
	}
	where
		ruleAnnotations :: (GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag) -> M.Map Var prodTag
		ruleAnnotations =
			M.fromList
			.
			(fmap $ \p -> (prod_left $ value p, tag p))
			.
			fromGrammar

fromSeparateProdTags ::
	forall prodTag symbolTag .
	prodTag -> GroupedGrammar_SeparateProdTags prodTag symbolTag -> GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag
fromSeparateProdTags defProdTag g =
	let
		grammar = ggSeparateProdTags_grammar g
	in
		fmap annotateProd grammar
	where
		annotateProd :: GroupedProductionTagged symbolTag -> GroupedProduction_ProdAndSymbolsTagged prodTag symbolTag  
		annotateProd prod =
			let annotations = ggSeparateProdTags_ruleAnnotations g
			in
				case M.lookup (prod_left prod) annotations of
					Nothing -> tagged defProdTag prod
					Just ann -> tagged ann prod

deriving instance
	(Eq productionTag, Eq symbolTag)
	=> Eq (GroupedGrammar_SeparateProdTags productionTag symbolTag)

instance Pretty Transformation where
	pretty t =
		case t of
			Annotate info ->
				concat $
				["annotate(", pretty info, ")"]
			ElimLeftRecur varScheme ->
				concat $
				["elimLeftRec(", pretty varScheme, ")"]
			ElimLeftRecurNoEpsilon varScheme ->
				concat $
				["elimLeftRec_noEpsilon(", pretty varScheme, ")"]
			ElimLeftRecur_Full varCondition varScheme ->
				concat $
				["elimLeftRec_full(", pretty varCondition, ",", pretty varScheme, ")"]
			ElimLeftRecurNoEpsilon_Full varCondition varScheme ->
				concat $
				["elimLeftRec_noEpsilon_full(", pretty varCondition, ",", pretty varScheme, ")"]
			LeftFactor varScheme ->
				concat $
				["leftFactor(", pretty varScheme, ")"]
			LeftFactor_Full iterateWhile varCondition varScheme ->
				concat $
				["leftFactor_full(", pretty iterateWhile, pretty varCondition, ",", pretty varScheme, ")"]
			BreakRules maxLength varScheme ->
				concat $
				["breakRules(", show maxLength, ",", pretty varScheme, ")"]
			Unfold varCondition ->
				concat $
				["unfold(", pretty varCondition, ")"]
			ElimEpsilon ->
				concat $
				["elimEpsilon()"]
			InsertProductions params ->
				concat $
				[ "insert("
				,
					case insertProdsParams_position params of
						GrammarPosBeginning -> "start"
						GrammarPosEnding -> "end"
				, ","
				, "?"
				--, toTextAs (defaultFormat Default) $ Grammar $ insertProdsParams_productions params
				, ")"
				]
			DeleteProductions varCondition ->
				concat $
				["delete(", pretty varCondition, ")"]
			RemoveDoubleProds ->
				"removeDoubleProds()"
			AddActionSymbols startIndex ->
				concat $
				["addActionSymbols(", show startIndex, ")"]
			SubGrammar subGrammarInfo ->
				concat $
				["subGrammar(", pretty subGrammarInfo, ")"]
			UnusedRules ->
				concat $
				["unused()"]
			FindDeadEnds ->
				concat $
				["findDeadEnds()"]

instance Pretty AnnotateInfo where
	pretty i =
		case i of
			AnnotateWithLoops -> "loops"
			AnnotateWithFirstSet -> "first"

instance Pretty VarCondition where
	pretty x =
		concat $
		[ if varCond_negate x then "not" else ""
		, ","
		, varCond_regex x
		]

instance Pretty VarScheme where
	pretty s =
		case s of
			Const str -> str
			FromVar -> "%v%n"

instance FromPretty VarScheme where
	fromPretty str =
		case str of
			"%v%n" -> Right $ FromVar
			"" -> Left $ "fromPretty error reading VarScheme"
			_ -> Right $ Const str

instance Pretty FullLeftFactorIterateWhile where
	pretty x =
		case x of
			IterateWhileDecreasing -> "whileDecreasing"
			IterateNTimes count -> show count

instance FromPretty FullLeftFactorIterateWhile where
	fromPretty str =
		case str of
			"whileDecreasing" ->
				return $ IterateWhileDecreasing
			_ ->
				maybe (Left "fromPretty error reading FullLeftFactorIterateWhile") Right $
				IterateNTimes <$> readMaybe str
