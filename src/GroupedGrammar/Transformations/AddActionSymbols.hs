module GroupedGrammar.Transformations.AddActionSymbols where

import Grammar.Types
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions

import Control.Monad.State


addActionSymbols ::
	Monad m =>
	Int
	-> GroupedGrammarTagged [SymbolTag]
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
addActionSymbols counterInit grammar _ _ =
	return $
	flip applyAlgorithmUsingProductions grammar $
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
