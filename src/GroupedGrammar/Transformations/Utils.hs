{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module GroupedGrammar.Transformations.Utils where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Types
import GroupedGrammar.Types
import Grammar.Types
import Utils.Graph

import Control.Monad
import Control.Monad.Identity

import qualified Data.Map as M


applyAlgorithmUsingProductions ::
	([ProductionGen Var [[Symbol]]] -> [ProductionGen Var [[Symbol]]])
	-> GroupedGrammarTagged [SymbolTag]
	-> M.Map Var prodTag0
	-> Graph Symbol (GroupedProductionTagged [SymbolTag])
	-> Maybe (GroupedGrammar_SeparateProdTags prodTag0 [SymbolTag])
applyAlgorithmUsingProductions f =
	applyAlgorithmUsingProductionsM'
		runIdentity
		(return . f)

applyAlgorithmUsingProductionsM scheme f grammar =
	applyAlgorithmUsingProductionsM'
		(flip (runVarNameMonad scheme) (fromTaggedGrammar grammar))
		f
		grammar


applyAlgorithmUsingProductionsM' ::
	Monad m =>
	(forall a . m a -> a) ->
	([ProductionGen Var [[Symbol]]] -> m [ProductionGen Var [[Symbol]]])
	-> GroupedGrammarTagged [SymbolTag] -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged [SymbolTag]) -> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
applyAlgorithmUsingProductionsM' runMonad algorithm grammar _ _ =
	return $
	ggSeparateProdTags_mapToGrammar calcGrammar $
	GroupedGrammar_SeparateProdTags grammar M.empty
	where
		calcGrammar :: forall tag . GroupedGrammarTagged [tag] -> GroupedGrammarTagged [tag]
		calcGrammar =
			grammar_mapToProductions $
			withUntaggedProductions $
			(
				runMonad
				--flip (runVarNameMonad scheme) (fromTaggedGrammar grammar)
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

{-
applyAlgorithmUsingProductionsM' ::
	VarScheme ->
	([ProductionGen Var [[Symbol]]] -> VarNameMonad [ProductionGen Var [[Symbol]]])
	-> GroupedGrammarTagged [SymbolTag] -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged [SymbolTag]) -> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
applyAlgorithmUsingProductionsM' scheme algorithm grammar _ _ =
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
-}

type ProcessedAndRemaining a = ([a],[a])

processAllExt ::
	(ProcessedAndRemaining a ->  a -> ProcessedAndRemaining a) -> [a] -> [a]
processAllExt f =
	runIdentity .
	processAllExtM (\processedAndRemaining current-> return $ f processedAndRemaining current)

processAllM ::
	Monad m =>
	(ProcessedAndRemaining a -> a -> m [a])
	-> [a] -> m [a]
processAllM f =
	processAllExtM $
	\processedAndRemaining currentElem -> liftM (,[]) $ f processedAndRemaining currentElem

processAllExtM ::
	forall a m . Monad m =>
	(ProcessedAndRemaining a ->  a -> m (ProcessedAndRemaining a)) -> [a] -> m [a]
processAllExtM f l =
	liftM fst $
	processAllExtM' f ([], l)
	where
		processAllExtM' :: (ProcessedAndRemaining a -> a -> m (ProcessedAndRemaining a)) -> ProcessedAndRemaining a -> m (ProcessedAndRemaining a)
		processAllExtM' f partition@(processed, remaining) =
			case remaining of
				[] -> return $ partition
				(x:xs) ->
					do
						(newElemsFinished, newElemsToBeProcessedAgain) <- f (processed,remaining) x
						processAllExtM' f (processed ++ newElemsFinished, newElemsToBeProcessedAgain ++ xs)
