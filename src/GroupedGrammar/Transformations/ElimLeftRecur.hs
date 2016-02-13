{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.ElimLeftRecur where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Utils
import Grammar.Types
import Utils

import Data.List
import Control.Monad


elimLeftRecur varScheme =
	applyAlgorithmUsingProductions varScheme $
		processAllReftRec $ elimLeftRecurStep

processAllReftRec ::
	forall a m . Monad m =>
	([a] ->  a -> m [a]) -> [a] -> m [a]
processAllReftRec f l =
	liftM fst $
	processAllReftRec' f ([], l)
	where
		processAllReftRec' :: ([a] -> a -> m [a]) -> ([a],[a]) -> m ([a],[a])
		processAllReftRec' f partition@(processed, remaining) =
			case remaining of
				[] -> return $ partition
				(x:xs) ->
					do
						newElems <- f processed x :: m [a]
						processAllReftRec' f (processed ++ newElems, xs)


elimLeftRecurStep ::
	[GroupedProduction] -> GroupedProduction -> VarNameMonad [GroupedProduction]
elimLeftRecurStep processed currentProd =
	elimImmediateLeftRecursion_Dragon $
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

elimImmediateLeftRecursion_Dragon ::
	GroupedProduction -> VarNameMonad [GroupedProduction]
elimImmediateLeftRecursion_Dragon prod =
	let
		left = prod_left prod
		rightSides = prod_right prod :: [[Symbol]]
	in
			case partition isLeftRecursive rightSides of
				([],_) ->
					return $ [prod]
				partition -> -- (recursiveProductions, otherProductions)
					liftM
						(
							uncurry splitProduction $
							mapFst (map $ drop 1) $
							partition
						) $
					getSimilarVar left
		where
			isLeftRecursive rightProductionSide =
				case rightProductionSide of
					(Right var : _) | var == prod_left prod -> True
					_ -> False
			splitProduction :: [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction]
			splitProduction recursiveProductionRests otherProductions newVarName =
				[ Production (prod_left prod) $
					(++) <$> otherProductions <*> [[Right newVarName]]
				, Production newVarName $
					((++) <$> recursiveProductionRests <*> [[Right newVarName]]) ++ [[Left epsilon]]
				]
