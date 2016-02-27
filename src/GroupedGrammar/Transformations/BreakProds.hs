{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module GroupedGrammar.Transformations.BreakProds where

import GroupedGrammar.Transformations.Utils
--import GroupedGrammar.Types
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types


breakProds ::
	(MonadLog m, MonadError String m) =>
	VarScheme
	-> Int
	-> TransformationImplTypeM prodTag [SymbolTag] m
breakProds varScheme maxLength grammar _ _ =
	runVarNameMonadT
		varScheme
		(grammar_mapToProductions (map groupedProd_removeSymbolTags) $ grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
	asWithNormalProductionsM $ 
		processAllM $ breakProdsStep maxLength

breakProdsStep ::
	Monad m =>
	Int ->
	(ProcessedAndRemaining Production)
	-> Production -> VarNameMonadT m ([Production], [Production])
breakProdsStep maxLength _ currentProd =
	let
		breakedRule = breakProdIfTooLong maxLength currentProd
	in
		{- first production is ok, the others are to be
			processed again:
		-}
		fmap (splitAt 1) $ breakedRule

breakProdIfTooLong ::
	Monad m =>
	Int -> Production -> VarNameMonadT m [Production]
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
