{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module GroupedGrammar.Transformations.Utils where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Types
import GroupedGrammar.Types
import Grammar.Types
import Utils.Graph

import Control.Monad

import qualified Data.Map as M


applyAlgorithmUsingProductions ::
	VarScheme ->
	([ProductionGen Var [[Symbol]]] -> VarNameMonad [ProductionGen Var [[Symbol]]])
	-> GroupedGrammarTagged [SymbolTag] -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged [SymbolTag]) -> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
applyAlgorithmUsingProductions scheme algorithm grammar _ _ =
	return $
	ggSeparateProdTags_mapToGrammar calcGrammar $
	GroupedGrammar_SeparateProdTags grammar M.empty
	where
		calcGrammar :: forall tag . GroupedGrammarTagged [tag] -> GroupedGrammarTagged [tag]
		calcGrammar =
			grammar_mapToProductions $
			withUntaggedProductions $
			(
				flip (runVarNameMonad scheme) (fromTaggedGrammar grammar)
				.
				algorithm 
			)
			where
				withUntaggedProductions f =
					map (prod_mapToRight $ map $ map $ tagged [])
					.
					f
					.
					map (prod_mapToRight $ map $ map $ value)

processAll f =
	processAllExt $
	\processed currentElem -> liftM (,[]) $ f processed currentElem

processAllExt ::
	forall a m . Monad m =>
	([a] ->  a -> m ([a],[a])) -> [a] -> m [a]
processAllExt f l =
	liftM fst $
	processAllExt' f ([], l)
	where
		processAllExt' :: ([a] -> a -> m ([a], [a])) -> ([a],[a]) -> m ([a],[a])
		processAllExt' f partition@(processed, remaining) =
			case remaining of
				[] -> return $ partition
				(x:xs) ->
					do
						(newElemsFinished, newElemsToBeProcessedAgain) <- f processed x
						processAllExt' f (processed ++ newElemsFinished, newElemsToBeProcessedAgain ++ xs)
