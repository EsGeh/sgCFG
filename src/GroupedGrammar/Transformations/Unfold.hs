module GroupedGrammar.Transformations.Unfold where

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Transformations.Types
import Grammar.Types
import GroupedGrammar.Types
import Utils

import Control.Monad
import Data.Maybe
import Data.List

import Text.Regex.TDFA

unfold unfoldParams =
	let
		pleaseRepeatUntilNotChanging = unfoldParams_repeatUntilNotChanging unfoldParams
		varCondDescr = unfoldParams_varCondDescr unfoldParams
	in
		applyAlgorithmUsingProductions $
		(if pleaseRepeatUntilNotChanging then repeatTillNotChanging else id) $
		\prods -> 
			fromMaybe prods $
			liftM (
				\(preceding,prod,rest) ->
					(replaceAll prod $ preceding)
					++
					[prod]
					++
					(replaceAll prod $ rest)
			) $
			findRuleToInsert (varCondFromDescr varCondDescr) $
			prods

findRuleToInsert :: (GroupedProduction -> Bool) -> [GroupedProduction] -> Maybe ([GroupedProduction], GroupedProduction, [GroupedProduction])
findRuleToInsert cond prods =
	case break cond prods of
		(preceding, prod:rest) ->
			return $ (preceding, prod, rest)
		_ -> Nothing

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

varCondFromDescr :: VariableConditionDescr -> (ProductionGen Var right -> Bool)
varCondFromDescr descr =
	let
		negate = varCondDescr_negate descr
		regex = varCondDescr_regex descr
	in
		(if negate then not else id)
		.
		(=~regex)
		.
		var_name . prod_left
