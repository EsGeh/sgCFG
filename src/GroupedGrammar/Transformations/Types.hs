{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.Types where

import GroupedGrammar.Types
import Grammar.Types
import Types
--import GrammarFormat
--import Types
import Utils.Graph

import Text.Regex.TDFA
import qualified Data.Map as M
--import qualified Data.Set as S
--import Data.List


data Transformation
	= Annotate AnnotateInfo
	| ElimLeftRecur VarScheme
	| ElimLeftRecurNoEpsilon VarScheme
	| ElimLeftRecur_Full VarCondition VarScheme
	| ElimLeftRecurNoEpsilon_Full VarCondition VarScheme
	| LeftFactor VarScheme
	| BreakRules Int VarScheme
	| Unfold VarCondition
	| ElimEpsilon
	| InsertProductions InsertProductionsParams
	| DeleteProductions VarCondition
	| AddActionSymbols Int
	| SubGrammar SubGrammarInfo
	| UnusedRules
	| FindDeadEnds
	deriving (Show)

{-
data SelectCondition
	= SelectByLeft VarCondition
	-- | SelectByRight RightCondition
	| SelectReachable
	| SelectNonReachable
-}

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

{-
data UnfoldParams = UnfoldParams {
	unfoldParams_repeatUntilNotChanging :: Bool,
	unfoldParams_varCondDescr :: VarCondition
}
	deriving (Show)
-}

data VarCondition = VarCondition {
	varCond_negate :: Bool,
	varCond_regex :: String
}
	deriving (Show)

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
		negate = varCond_negate descr
		regex = varCond_regex descr
	in
		(if negate then not else id)
		.
		(=~regex)
		.
		var_name

{-
prodCondFromVarCondDescr :: VarCondition -> (ProductionGen Var right -> Bool)
prodCondFromVarCondDescr descr =
	varCondFromDescr descr
	. prod_left
-}

instance FromPretty VarScheme where
	fromPretty str =
		case str of
			"%v%n" -> Right $ FromVar
			"" -> Left $ "fromPretty error reading VarScheme"
			str -> Right $ Const str

data GroupedGrammar_SeparateProdTags productionTag symbolTag =
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar :: GrammarGen (GroupedProductionTagged symbolTag),
		ggSeparateProdTags_ruleAnnotations :: M.Map Var productionTag
	}
ggSeparateProdTags_mapToGrammar f g =
	g{ ggSeparateProdTags_grammar = f (ggSeparateProdTags_grammar g) }
ggSeparateProdTags_mapToRuleAnnotations f g =
	g{ ggSeparateProdTags_ruleAnnotations = f (ggSeparateProdTags_ruleAnnotations g) }

type GrammarGraph symbolTag =
	Graph Symbol (GroupedProductionTagged symbolTag)
	--Graph Symbol (GroupedProductionGen Var Symbol)
--type GrammarNode = Maybe Symbol
-- Nothing means "or"

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

toTaggedGrammar :: GroupedGrammar -> GroupedGrammarTagged [symbolTag]
toTaggedGrammar =
	fmap $ toTaggedProduction

fromTaggedGrammar :: GroupedGrammarTagged symbolTag -> GroupedGrammar
fromTaggedGrammar =
	fmap $ fromTaggedProduction

toTaggedProduction =
	prod_mapToRight $ map $ map $ Tagged []

fromTaggedProduction = 
	prod_mapToRight $ map $ map $ value


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
