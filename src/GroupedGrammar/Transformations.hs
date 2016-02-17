{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations(
	Transformation(..), AnnotateInfo(..),
	UnfoldParams(..), VariableConditionDescr(..),
	SymbolTag(..),
	toProdAndSymbolsTagged, toTaggedGrammar,
	graphFromGroupedGrammar,
	applyTransformation
) where

import GroupedGrammar.Transformations.FirstSet
import GroupedGrammar.Transformations.ElimLeftRecur
import GroupedGrammar.Transformations.LeftFactor
import GroupedGrammar.Transformations.FindLoops
import GroupedGrammar.Transformations.BreakRules
import GroupedGrammar.Transformations.Unfold

import GroupedGrammar.Transformations.Types
import GroupedGrammar.Types
import Grammar.Types
import Utils.Graph

import qualified Data.Tree as Tree
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad


toProdAndSymbolsTagged ::
	productionTag ->
		GroupedGrammarTagged symbolTag -> GroupedGrammar_ProdAndSymbolsTagged productionTag symbolTag
toProdAndSymbolsTagged defTag =
	fmap $ tagged defTag

applyTransformation ::
	Transformation ->
	GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag]-> Maybe (GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag])
applyTransformation t g' =
		case t of
			Annotate info ->
				case info of
					AnnotateWithLoops -> 
						flip (applyTransformationImpl prodTag_empty) g' $ \g prodTags graph ->
							do
								startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
								newGrammar <- findLoops startSym graph
								return $
									GroupedGrammar_SeparateProdTags {
										ggSeparateProdTags_grammar = g `intersectTaggedGrammars` newGrammar,
										ggSeparateProdTags_ruleAnnotations = prodTags
									}
					AnnotateWithFirstSet ->
						(Just . fromSeparateProdTags prodTag_empty . conv annotateWithFirstSets . toSeparateProdTags) g'
						where
							conv :: 
								Eq symbolTag
								=> (GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag)
								-> GroupedGrammar_SeparateProdTags ProductionTag symbolTag -> GroupedGrammar_SeparateProdTags ProductionTag symbolTag
							conv f g =
								let originalProdTags = ggSeparateProdTags_ruleAnnotations g
								in
									rejoinWithOriginalProdTags originalProdTags $
									f $
									(ggSeparateProdTags_mapToRuleAnnotations $ M.mapMaybe $ const $ Just $ S.empty) g
									where
										rejoinWithOriginalProdTags origAnn =
											ggSeparateProdTags_mapToRuleAnnotations $
											M.mapMaybeWithKey $
											\var set ->
												let oldAnn = maybe (error "rejoinWithOriginalProdTags error") id $ M.lookup var origAnn
												in
													Just $
													(prodTag_mapToFirstSet $ const $ Just set) $
													oldAnn
			LeftFactor varScheme ->
				flip (applyTransformationImpl prodTag_empty) g' (leftFactor varScheme)
			ElimLeftRecur varScheme ->
				flip (applyTransformationImpl prodTag_empty) g' $ elimLeftRecur varScheme
			ElimLeftRecurNoEpsilon varScheme ->
				flip (applyTransformationImpl prodTag_empty) g' $ elimLeftRecurNoEpsilon varScheme
			BreakRules maxLength varScheme ->
				flip (applyTransformationImpl prodTag_empty) g' $ breakRules varScheme maxLength
			Unfold params ->
				flip (applyTransformationImpl prodTag_empty) g' $ unfold params
			SubGrammar var ->
				flip (applyTransformationImpl prodTag_empty) g' $
				\g prodTags graph ->
					do
						subGrammar <- groupedGrammarSub [var] graph
						return $
							GroupedGrammar_SeparateProdTags {
								ggSeparateProdTags_grammar = subGrammar `intersectTaggedGrammars` g,
								ggSeparateProdTags_ruleAnnotations = prodTags
							}
			UnusedRules ->
				flip (applyTransformationImpl prodTag_empty) g' $
				\g prodTags graph ->
					let
						used =
							maybe (Grammar []) id $
							do
								startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
								(groupedGrammarSub [startSym] graph)
					in
						return $
							GroupedGrammar_SeparateProdTags {
								ggSeparateProdTags_grammar =
									Grammar $
									fromGrammar g
									\\
									fromGrammar used,
								ggSeparateProdTags_ruleAnnotations = prodTags
							}

groupedGrammarSub :: [Var] -> GrammarGraph symbolTag -> Maybe (GroupedGrammarTagged symbolTag)
groupedGrammarSub vars graph =
	liftM (
		Grammar .
		join .
		map (Tree.flatten . fmap snd)
	) $
	spanningForest (map Right vars) graph

type TransformationImplType prodTag symbolTag m =
	GroupedGrammarTagged symbolTag -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged symbolTag) -> m (GroupedGrammar_SeparateProdTags prodTag symbolTag)

applyTransformationImpl ::
	Monad m => 
	prodTag ->
	TransformationImplType prodTag symbolTag m
	-> GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag -> m (GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag)
applyTransformationImpl defProdTag f grammarWithAnn =
	let
		separateProdTags = toSeparateProdTags grammarWithAnn
		g = ggSeparateProdTags_grammar separateProdTags
		prodTags = ggSeparateProdTags_ruleAnnotations separateProdTags
		graph =
			graphFromGroupedGrammar
				(Right . prod_left) -- key from production
				(map value . join . prod_right) -- destinations from production
				$
				g
	in
		do
			res <- f g prodTags graph
			return $
				fromSeparateProdTags defProdTag $
				GroupedGrammar_SeparateProdTags {
					ggSeparateProdTags_grammar = ggSeparateProdTags_grammar res,
					ggSeparateProdTags_ruleAnnotations = ggSeparateProdTags_ruleAnnotations res
				}

{- |for every prod P = (A -> [[..]..[..]]) in G1
	find similar prod P' in G2
	add P to the result, concat tags from P with P'
-}
intersectTaggedGrammars ::
	GroupedGrammarTagged [SymbolTag]
	-> GroupedGrammarTagged [SymbolTag]
	-> GroupedGrammarTagged [SymbolTag]
intersectTaggedGrammars g1 g2 =
	Grammar $
	let
		productions1 = fromGrammar g1
		productions2 = fromGrammar g2
	in
		productions1
		`merge`
		productions2
		where
			--merge :: [GroupedProductionTagged] -> [GroupedProductionTagged] -> [GroupedProductionTagged]
			merge p1 p2 =
				case p1 of
					(x:xs) ->
						maybe (merge xs p2) (\y -> mergeProductions x y : merge xs p2) $
							find ((==fromTaggedProduction x) . fromTaggedProduction) p2
					_ -> []
			--mergeProductions :: GroupedProductionTagged -> GroupedProductionTagged -> GroupedProductionTagged
			mergeProductions p1 p2 = 
				Production
					(prod_left p1) $
					zipWith
						(zipWith $ \a b -> tagged_mapToTag (++ tag b) a)
						(prod_right p1)
						(prod_right p2)
