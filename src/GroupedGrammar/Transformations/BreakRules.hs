{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.BreakRules where

import GroupedGrammar.Transformations.VarNameMonad
--import GroupedGrammar.Types
import GroupedGrammar.Conversions
import GroupedGrammar.Transformations.Utils
import Grammar.Types

import Control.Monad


breakRules varScheme maxLength =
	applyAlgorithmUsingProductionsM varScheme $
	asWithNormalProductionsM $ 
		processAllExtM $ breakRulesStep maxLength

breakRulesStep :: Int -> (ProcessedAndRemaining Production) -> Production -> VarNameMonad ([Production], [Production])
breakRulesStep maxLength _ currentProd =
	let
		breakedRule = breakRuleIfTooLong maxLength currentProd
	in
		{- first production is ok, the others are to be
			processed again:
		-}
		liftM (splitAt 1) $ breakedRule

breakRuleIfTooLong :: Int -> Production -> VarNameMonad [Production]
breakRuleIfTooLong maxLength prod =
	if (length $ prod_right prod) > maxLength
	then
		liftM (breakRule maxLength prod) $
		getSimilarVar $ prod_left prod
	else
		return [prod]

breakRule :: Int -> Production -> Var -> [Production]
breakRule maxLength prod newVarName =
	let
		(rightSideHead, rest) = splitAt (maxLength-1) $ prod_right $ prod
	in
		[ Production (prod_left prod) $ rightSideHead ++ [Right newVarName]
		, Production newVarName $ rest
		]
