{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.FirstSet where

import GroupedGrammar.Types
import GroupedGrammar.Conversions
--import GroupedGrammar.Transformations.Utils
import Grammar.Types
import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe


--annotateWithFirstSets ::
annotateWithFirstSets grammar =
	fmap (ProductionTag . Just) $
	(
	repeatTillNotChanging $
	step (concatMap productionsFromGroupedProd $ fromGrammar $ grammar)
	) $
	(initFirstSets $ fromGrammar $ grammar)

{-
annotateWithFirstSets ::
	TransformationImplTypeM ProductionTag [SymbolTag] Maybe
annotateWithFirstSets ggTagged _ _ =
	return $
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar = ggTagged,
		ggSeparateProdTags_ruleAnnotations =
			fmap (ProductionTag . Just) $
			(
			repeatTillNotChanging $
			step (concatMap productionsFromGroupedProd $ fromGrammar $ fromTaggedGrammar ggTagged)
			) $
			(initFirstSets $ fromGrammar $ fromTaggedGrammar ggTagged)
	}
-}

initFirstSets :: [GroupedProduction] -> M.Map Var FirstSet
initFirstSets prods =
	M.fromList $
	map (\p -> (prod_left p, S.empty))
	prods

step :: [Production] -> M.Map Var FirstSet -> M.Map Var FirstSet
step prods =
	foldl (.) id (map updateFirstSet prods)
	where
		updateFirstSet :: Production -> M.Map Var FirstSet -> M.Map Var FirstSet
		updateFirstSet prod firstMap =
			let
				left = prod_left prod
				right = prod_right prod
			in
				(
					\newFirsts ->
						M.adjust (S.union newFirsts) left firstMap
				) $
				calcNewFirstSet (flip M.lookup firstMap) right

calcNewFirstSet :: (Var -> Maybe FirstSet) -> [Symbol] -> FirstSet
calcNewFirstSet lookup right =
	case right of
		((Left t):xs) ->
			(
				if t == epsilon
				then
					\x -> x `S.union` calcNewFirstSet lookup xs
				else
				id
			) $
			S.singleton t
		((Right var):xs) ->
			let
				directFirst =
					fromMaybe (S.singleton epsilon) $
					lookup var
			in
				if epsilon `S.member` directFirst
					then
						directFirst `S.union` calcNewFirstSet lookup xs
					else
						directFirst
		[] ->
			S.singleton epsilon
