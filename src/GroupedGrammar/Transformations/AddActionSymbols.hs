module GroupedGrammar.Transformations.AddActionSymbols where

import GroupedGrammar.Transformations.Types
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions
import Grammar.Types

import Control.Monad.State

--addActionSymbols :: Int
addActionSymbols counterInit =
	applyAlgorithmUsingProductions $
	asWithNormalProductions $
	flip evalState counterInit
	.
	(
		processAllOnceM $
		\_ prod ->
			mapM (prod_mapToRightM $ addActionSym) $ [prod]
			--return $ [prod]
	)

addActionSym :: [Symbol] -> State Int [Symbol]
addActionSym rightSide =
	do
		ret <- get >>= return . addActionSym'
		modify (+1)
		return $ ret
	where
		addActionSym' index =
			rightSide ++ [varSymbol $ Var $ "action_" ++ show index]
