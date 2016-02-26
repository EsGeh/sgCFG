module GroupedGrammar.Transformations.FindDeadEnds where

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Transformations.Types
--import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types

import qualified Utils.Graph as Graph

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Either as Either

findDeadEnds ::
	GroupedGrammarTagged [SymbolTag]
	-> M.Map Var prodTag
	-> Graph.Graph Symbol (GroupedProductionTagged [SymbolTag])
	-> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
findDeadEnds grammar m graph =
	(\f -> applyAlgorithmUsingProductions f grammar m graph) $
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
