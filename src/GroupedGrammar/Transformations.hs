{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations(
	TransformationMonadT, TransformationMonad,
	-- doLog, withNoLogging,
	Transformation(..), AnnotateInfo(..),
	VarCondition(..),
	InsertProductionsParams(..),
	GrammarPosition(..),
	SymbolTag(..),
	toProdAndSymbolsTagged, toTaggedGrammar,
	graphFromGroupedGrammar,
	applyTransformation
) where

import GroupedGrammar.Transformations.FirstSet
import GroupedGrammar.Transformations.ElimLeftRecur
import GroupedGrammar.Transformations.LeftFactor
import GroupedGrammar.Transformations.FindLoops
import GroupedGrammar.Transformations.FindDeadEnds
import GroupedGrammar.Transformations.BreakProds
import GroupedGrammar.Transformations.Unfold
import GroupedGrammar.Transformations.ElimEpsilon
import GroupedGrammar.Transformations.AddProds
import GroupedGrammar.Transformations.DeleteProds
import GroupedGrammar.Transformations.AddActionSymbols
import GroupedGrammar.Transformations.RemoveDoubleProds

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Transformations.Types
import GroupedGrammar.Types
import Grammar.Types
import Utils.Graph

import Types

import qualified Data.Tree as Tree
import Data.List
import Data.Maybe
import Control.Monad.Identity


type TransformationMonadT m a =
	ExceptT String m a

type TransformationMonad a = TransformationMonadT Identity a

applyTransformation ::
	MonadLog m =>
	Transformation
	-> GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag]
	-> TransformationMonadT m (GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag])
applyTransformation t g' =
	((doLog $ concat $ ["applying ", pretty t]) >>) $
	flip (applyTransformationImpl prodTag_empty) g' $ 
		case t of
			Annotate info ->
				case info of
					AnnotateWithLoops -> 
						\g prodTags graph ->
							do
								startSym <-
									maybe (throwError "no start symbol found") return $
									fmap prod_left $ listToMaybe $ fromGrammar g
								newGrammar <-
									maybe (throwError "no start symbol found") return $
									findLoops startSym graph
								return $
									GroupedGrammar_SeparateProdTags {
										ggSeparateProdTags_grammar = g `intersectTaggedGrammars` newGrammar,
										ggSeparateProdTags_ruleAnnotations = prodTags
									}
					AnnotateWithFirstSet ->
						\g _ _ ->
							return $
								GroupedGrammar_SeparateProdTags {
									ggSeparateProdTags_grammar = g,
									ggSeparateProdTags_ruleAnnotations =
										annotateWithFirstSets (fromTaggedGrammar g)
								}
			LeftFactor varScheme ->
				leftFactor varScheme
			LeftFactor_Full varCond varScheme ->
				leftFactor_full (varCondFromDescr varCond) varScheme
			ElimLeftRecur varScheme ->
				elimLeftRecur varScheme
			ElimLeftRecurNoEpsilon varScheme ->
				elimLeftRecurNoEpsilon varScheme
			ElimLeftRecur_Full varCond varScheme ->
				elimLeftRecur_full (varCondFromDescr varCond) varScheme
			ElimLeftRecurNoEpsilon_Full varCond varScheme ->
				elimLeftRecurNoEpsilon_full (varCondFromDescr varCond) varScheme
			BreakRules maxLength varScheme ->
				breakProds varScheme maxLength
			Unfold varCondDescr ->
				unfold $ varCondFromDescr varCondDescr
			ElimEpsilon ->
				elimEpsilon
			InsertProductions params ->
				insertProds params
			DeleteProductions condDescr ->
				deleteProds $ varCondFromDescr condDescr
			RemoveDoubleProds ->
				removeDoubleProds
			AddActionSymbols counterInit ->
				addActionSymbols counterInit
			SubGrammar var ->
				\g prodTags graph ->
					do
						subGrammar <- groupedGrammarSub [var] graph
						return $
							GroupedGrammar_SeparateProdTags {
								ggSeparateProdTags_grammar = subGrammar `intersectTaggedGrammars` g,
								ggSeparateProdTags_ruleAnnotations = prodTags
							}
			FindDeadEnds ->
				findDeadEnds
			UnusedRules ->
				\g prodTags graph ->
					do
						used <-
							do
								startSym <-
									maybe (throwError "no start symbol found") return $
									fmap prod_left $ listToMaybe $ fromGrammar g
								(groupedGrammarSub [startSym] graph)
						return $
							GroupedGrammar_SeparateProdTags {
								ggSeparateProdTags_grammar =
									Grammar $
									fromGrammar g
									\\
									fromGrammar used,
								ggSeparateProdTags_ruleAnnotations = prodTags
							}

groupedGrammarSub ::
	MonadError String m =>
	[Var] -> GrammarGraph symbolTag
	-> m (GroupedGrammarTagged symbolTag)
groupedGrammarSub vars graph =
	maybe (throwError $ "error while calculating subGrammar") return $
	fmap (
		Grammar .
		join .
		map (Tree.flatten . fmap snd)
	) $
	spanningForest (map Right vars) graph

applyTransformationImpl ::
	Monad m => 
	prodTag ->
	TransformationImplTypeM prodTag symbolTag m
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
