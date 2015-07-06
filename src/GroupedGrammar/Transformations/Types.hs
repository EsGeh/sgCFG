{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module GroupedGrammar.Transformations.Types where

import GroupedGrammar.Internals
import GrammarTypes
import GrammarFormat
import Types
import Utils.Graph

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List


data Transformation
	= Annotate AnnotateInfo
	| SubGrammar SubGrammarInfo
	| UnusedRules
	deriving (Show)

type SubGrammarInfo = Var
data AnnotateInfo
	= AnnotateWithLoops
	| AnnotateWithFirstSet
	deriving (Show)

data GroupedGrammar_SeparateProdTags productionTag symbolTag =
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar :: GrammarGen (GroupedProductionTagged symbolTag),
		ggSeparateProdTags_ruleAnnotations :: M.Map Var productionTag
	}
ggSeparateProdTags_mapToGrammar f g =
	g{ ggSeparateProdTags_grammar = f (ggSeparateProdTags_grammar g) }
ggSeparateProdTags_mapToRuleAnnotations f g =
	g{ ggSeparateProdTags_ruleAnnotations = f (ggSeparateProdTags_ruleAnnotations g) }

data SymbolTag
	= ProductionRef Var
	deriving( Eq, Ord )

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

fromSeparateProdTags :: GroupedGrammar_SeparateProdTags prodTag symbolTag -> GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag
fromSeparateProdTags g =
	let
		grammar = ggSeparateProdTags_grammar g
	in
		fmap annotateProd grammar
	where
		annotateProd prod =
			let annotations = ggSeparateProdTags_ruleAnnotations g
			in
				case M.lookup (prod_left prod) annotations of
					Nothing -> error "fromSeparateProdTags error"
					Just ann -> tagged ann prod

instance ToTextAs GrammarFormat SymbolTag where
	toTextAs _ _ = "!"

instance ToTextAs GrammarFormat ProductionTag where
	toTextAs format x =
		maybe "" id $
			(prodTag_firstSet x) >>= \set -> return $
				concat $
					[ "FIRST = { "
					, intercalate ", " $ fmap (toTextAs format) $ S.toList $ set
					, "}"
					]

deriving instance
	(Eq productionTag, Eq symbolTag)
	=> Eq (GroupedGrammar_SeparateProdTags productionTag symbolTag)
