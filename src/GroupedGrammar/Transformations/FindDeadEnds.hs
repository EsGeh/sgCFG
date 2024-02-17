{-# LANGUAGE FlexibleContexts #-}
module GroupedGrammar.Transformations.FindDeadEnds where

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Types
import Grammar.Types

import qualified Utils.Graph as Graph

import qualified Data.Set as Set
import qualified Data.Either as Either


findDeadEnds ::
	(MonadLog m) =>
	TransformationImplTypeM prodTag [SymbolTag] m
findDeadEnds grammar _ graph =
	return $
	flip applyAlgorithmUsingProductions grammar $
	\prods ->
		map (\var -> Production var []) $
		Set.toList $
		allVars prods Set.\\ Set.fromList (Either.rights $ Graph.graph_nodeKeys graph)

allVars :: [GroupedProduction] -> Set.Set Var
allVars prods =
	foldr Set.union Set.empty $
	flip map prods $
	\prod ->
		Set.singleton (prod_left prod)
		`Set.union`
		(
		Set.fromList $
		Either.rights $
		concat $ prod_right prod
		)
