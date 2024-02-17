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


annotateWithFirstSets ::
	GrammarGen GroupedProduction -> M.Map Var ProductionTag
annotateWithFirstSets grammar =
	fmap (ProductionTag . Just) $
	(
	repeatTillNotChanging $
	step (concatMap productionsFromGroupedProd $ fromGrammar $ grammar)
	) $
	(initFirstSets $ fromGrammar $ grammar)

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
calcNewFirstSet lookupFunc right =
	case right of
		((Left t):xs) ->
			(
				if t == epsilon
				then
					\x -> x `S.union` calcNewFirstSet lookupFunc xs
				else
				id
			) $
			S.singleton t
		((Right var):xs) ->
			let
				directFirst =
					fromMaybe (S.singleton epsilon) $
					lookupFunc var
			in
				if epsilon `S.member` directFirst
					then
						directFirst `S.union` calcNewFirstSet lookupFunc xs
					else
						directFirst
		[] ->
			S.singleton epsilon
