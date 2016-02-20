module GroupedGrammar.Transformations.Unfold where

import GroupedGrammar.Transformations.Utils
import Grammar.Types
import GroupedGrammar.Types
import Utils

import Data.List
import Data.Maybe
import Control.Monad


unfold varCond =
	let
		cond = varCond . prod_left
	in
		applyAlgorithmUsingProductions $
		\prods ->
			maybe prods (
				fromZipper
				.
				repeatTillNotChanging (
					(
						\state ->
							fromMaybe state $
							nextSelection cond state
					)
					.
					step
				)
			) $
			selectProd cond prods
		{-
		(if pleaseRepeatUntilNotChanging then repeatTillNotChanging else id) $
		\prods -> 
			maybe prods (
				\(preceding,prod,rest) ->
					(replaceAll prod $ preceding)
					++
					[prod]
					++
					(replaceAll prod $ rest)
			) $
			selectProd (varCondFromDescr varCondDescr) $
			prods
		-}

fromZipper (preceding, selected, rest) =
	preceding ++ [selected] ++ rest

step :: Zipper GroupedProduction -> Zipper GroupedProduction
step (preceding,prod,rest) =
	(replaceAll prod $ preceding
	,
	prod
	,
	replaceAll prod $ rest)

nextSelection :: (GroupedProduction -> Bool) -> Zipper GroupedProduction -> Maybe (Zipper GroupedProduction)
nextSelection cond (lastPreceding, lastSelected, lastRest) =
	flip fmap (selectProd cond lastRest) $
		\(preceding, selected, rest) ->
			(lastPreceding ++ [lastSelected] ++ preceding, selected, rest)

type Zipper a = ([a], a, [a]) -- (preceding, selected, rest)

replaceAll :: GroupedProduction -> [GroupedProduction] -> [GroupedProduction]
replaceAll prod prods =
	let
		var = prod_left prod
		rightSides = prod_right prod
	in
		(
			prod_mapToRight $
			\currentRightSides ->
				join $
				(replaceAllByCond' (== Right var)) rightSides <$> currentRightSides
		)
		<$>
		prods

replaceAllByCond' :: (a -> Bool) -> [[a]] -> [a] -> [[a]]
replaceAllByCond' cond inserts l =
	case find cond l of
		Nothing -> [l]
		_ -> replaceAllByCond cond <$> inserts <*> [l]
