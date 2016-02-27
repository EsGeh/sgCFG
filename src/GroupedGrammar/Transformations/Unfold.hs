module GroupedGrammar.Transformations.Unfold where

import GroupedGrammar.Transformations.Utils
import Grammar.Types
import GroupedGrammar.Types
import Utils

import Data.List
import Data.Maybe


unfold varCond grammar _ _ =
	let
		cond = varCond . prod_left
	in
		return $
		flip applyAlgorithmUsingProductions grammar $
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

step :: Zipper GroupedProduction -> Zipper GroupedProduction
step (preceding,prod,rest) =
	(replaceAll prod $ preceding
	,
	prod
	,
	replaceAll prod $ rest)

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
