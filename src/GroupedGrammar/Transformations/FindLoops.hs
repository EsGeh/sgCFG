{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.FindLoops where

import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types
import Grammar.Types
import Utils.Graph
import qualified Data.Tree as Tree

import Data.Maybe


findLoops ::
	Var -> GrammarGraph [SymbolTag] ->
	Maybe (GroupedGrammarTagged [SymbolTag])
findLoops startSymbol graph = do
	forest <-
		return . map (fmap snd)
		=<<
		spanningForest [Right startSymbol] graph
		:: Maybe [Tree.Tree (GroupedProductionTagged [SymbolTag])]
	case map (findLoopsInTree graph) forest of
		[tree] ->
			return $
				Grammar $
				map (uncurry f) $
				Tree.flatten $
				tree
			where
				f :: GroupedProductionTagged [SymbolTag]-> [Var] -> GroupedProductionGen Var (Tagged [SymbolTag] Symbol)
				f prod backEdgeVars =
					(prod_mapToRight $ map $ map tagMaybe) prod
					where
						tagMaybe ::
							Tagged [SymbolTag] Symbol -> Tagged [SymbolTag] Symbol
						tagMaybe sym =
							case value sym of
								Right var
									| var `elem` backEdgeVars ->
										tagged_mapToTag (ProductionRef var:) sym
								_ ->
									sym
									--Tagged [] sym
		_ -> Nothing

type SeedType = (Tree.Tree (GroupedProductionTagged [SymbolTag]), [GroupedProductionTagged [SymbolTag]])

findLoopsInTree :: GrammarGraph [SymbolTag] -> Tree.Tree (GroupedProductionTagged [SymbolTag]) -> Tree.Tree (GroupedProductionTagged [SymbolTag], [Var])
findLoopsInTree graph tree =
	Tree.unfoldTree unfoldF (tree, [])
		:: Tree.Tree (GroupedProductionTagged [SymbolTag], [Var])
	where
		unfoldF :: SeedType -> ((GroupedProductionTagged [SymbolTag], [Var]), [SeedType])
		unfoldF (tree, path) = (newNode, newSeeds) 
			where
				newNode :: (GroupedProductionTagged [SymbolTag], [Var])
				newNode =
					(label, allBackEdges)
					where
						allBackEdges :: [Var]
						allBackEdges =
							filter cond $ map prod_left (label:path)
						cond var =
							fromMaybe False $
							isReachable graph
								(Right $ prod_left label)
								(Right var)
				newSeeds =
					map calcSeeds $ Tree.subForest tree
					where
						calcSeeds subTree = (subTree, label:path)
				label = Tree.rootLabel tree
