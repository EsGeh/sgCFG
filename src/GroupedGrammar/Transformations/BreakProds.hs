{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.BreakProds where

import GroupedGrammar.Transformations.VarNameMonad
--import GroupedGrammar.Types
import GroupedGrammar.Conversions
import GroupedGrammar.Transformations.Utils
import Grammar.Types


breakProds varScheme maxLength =
	applyAlgorithmUsingProductionsM varScheme $
	asWithNormalProductionsM $ 
		processAllM $ breakProdsStep maxLength

breakProdsStep :: Int -> (ProcessedAndRemaining Production) -> Production -> VarNameMonad ([Production], [Production])
breakProdsStep maxLength _ currentProd =
	let
		breakedRule = breakProdIfTooLong maxLength currentProd
	in
		{- first production is ok, the others are to be
			processed again:
		-}
		fmap (splitAt 1) $ breakedRule

breakProdIfTooLong :: Int -> Production -> VarNameMonad [Production]
breakProdIfTooLong maxLength prod =
	if (length $ prod_right prod) > maxLength
	then
		fmap (breakProd maxLength prod) $
		getSimilarVar $ prod_left prod
	else
		return [prod]

breakProd :: Int -> Production -> Var -> [Production]
breakProd maxLength prod newVarName =
	let
		(rightSideHead, rest) = splitAt (maxLength-1) $ prod_right $ prod
	in
		[ Production (prod_left prod) $ rightSideHead ++ [Right newVarName]
		, Production newVarName $ rest
		]
