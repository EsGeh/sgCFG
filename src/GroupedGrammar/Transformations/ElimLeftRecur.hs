{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.ElimLeftRecur where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Utils
import Grammar.Types
import Types
import Utils

import Data.List
import Data.Maybe
import Control.Monad

--import Debug.Trace


elimLeftRecur_full =
	elimFull $
	elimImmediateLeftRecursion

elimLeftRecurNoEpsilon_full =
	elimFull $
	elimImmediateLeftRecursion_noEpsilon

elimFull immediate varCond varScheme =
	applyAlgorithmUsingProductionsM varScheme $
		processAllOnceM $
		\(processed,remaining) current ->
			(
				fmap (\p -> toGroupedProductions $ maybeUnfold varCond (processed++remaining) =<< productionsFromGroupedProd =<< p)
				. immediate
			) $
			elimIndirectLeftRecursion processed current

{- |
	`maybeUnfold cond (processed, remaining) prod` does the following:
	be prod = A -> X1,..., Xn.
	if cond X1 then:
		look for rules like
			X1 -> ...
		unfold them into prod.
-}
maybeUnfold ::
	(Var -> Bool)
	-> [GroupedProduction]
	-> Production -> [Production]
maybeUnfold cond allOtherProds production =
	flip prod_mapToRightM production $
	\right ->
		case right of
			(Right var):rest | cond var ->
				let
					productionsToInsert =
						(productionsFromGroupedProd =<<) $
						filter ((== var) . prod_left) $
						allOtherProds
					in
						(++rest) <$> (map prod_right productionsToInsert)
			_ -> [right]
	{-
	if
		maybe False (
			either (const False) cond -- if first symbol is terminal => False
		) $
		listToMaybe $ -- first symbol
		prod_right $ production
	then
		flip prod_mapToRightM production $
			\right -> replaceAll_byIndex 0 <$> replaceBy <*> [right]
		{-
		let
			replaceBy :: [[Symbol]]
			replaceBy =
				map prod_right $
				(productionsFromGroupedProd =<<) $
				filter ((==prod_left production) . prod_left) $
				allOtherProds
		in
		-}
			{-
			trace (
				concat $
				["replacing in "
				, pretty production
				, ": "
				, Utils.unlines $ map pretty replaceBy
				]
			) $
			-}
	else
		[production]
	-}

elimLeftRecur varScheme =
	applyAlgorithmUsingProductionsM varScheme $
		processAllOnceM $
			elimLeftRecurStep $ elimImmediateLeftRecursion

elimLeftRecurNoEpsilon varScheme =
	applyAlgorithmUsingProductionsM varScheme $
		processAllOnceM $
			elimLeftRecurStep $ elimImmediateLeftRecursion_noEpsilon

elimLeftRecurStep ::
	(GroupedProduction -> VarNameMonad [GroupedProduction])
	-> ProcessedAndRemaining GroupedProduction -> GroupedProduction -> VarNameMonad [GroupedProduction]
elimLeftRecurStep immediateStep (processed,_) currentProd =
	immediateStep $
	elimIndirectLeftRecursion processed currentProd

elimIndirectLeftRecursion ::
	[GroupedProduction] -> GroupedProduction -> GroupedProduction
elimIndirectLeftRecursion processed currentProd =
	let
		left = prod_left currentProd :: Var
		rightSides = prod_right currentProd :: [[Symbol]]
	in
		Production left $
			join $
			map insertRules rightSides
		where
			insertRules :: [Symbol] -> [[Symbol]]
			insertRules right =
				case right of
					(Right var : rightRest) ->
						let mSelectProcessedRule =
							find ((==var) . prod_left) $ processed
						in
							case mSelectProcessedRule of
								Just selectedRule ->
									map (++rightRest) $ prod_right selectedRule
								Nothing ->
									[right]
					_ ->
						[right]

elimImmediateLeftRecursion ::
	GroupedProduction -> VarNameMonad [GroupedProduction]
elimImmediateLeftRecursion =
	elimImmediateLeftRecursionImpl $
		splitProduction

elimImmediateLeftRecursion_noEpsilon ::
	GroupedProduction -> VarNameMonad [GroupedProduction]
elimImmediateLeftRecursion_noEpsilon =
	elimImmediateLeftRecursionImpl $
		splitProductionNoEpsilon

elimImmediateLeftRecursionImpl ::
	(Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction])
	-> GroupedProduction -> VarNameMonad [GroupedProduction]
elimImmediateLeftRecursionImpl splitProduction' prod =
	let
		left = prod_left prod
		rightSides = prod_right prod :: [[Symbol]]
	in
			case partition isLeftRecursive rightSides of
				([],_) ->
					return $ [prod]
				partition -> -- (recursiveProductions, otherProductions)
					fmap
						(
							uncurry (splitProduction' left) $
							mapFst (map $ drop 1) $
							partition
						) $
					getSimilarVar left
		where
			isLeftRecursive rightProductionSide =
				case rightProductionSide of
					(Right var : _) | var == prod_left prod -> True
					_ -> False

splitProduction :: Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction]
splitProduction left recursiveProductionRests otherProductions newVarName =
	[ Production left $
		(++) <$> otherProductions <*> [[Right newVarName]]
	, Production newVarName $
		((++) <$> recursiveProductionRests <*> [[Right newVarName]])
		++
		[[Left epsilon]]
	]

splitProductionNoEpsilon :: Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction]
splitProductionNoEpsilon left recursiveProductionRests otherProductions newVarName =
	[ Production left $
		otherProductions
		++
		((++) <$> otherProductions <*> [[Right newVarName]])
	, Production newVarName $
		((++) <$> recursiveProductionRests <*> [[Right newVarName]])
	]
