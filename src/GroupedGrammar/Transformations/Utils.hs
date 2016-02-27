{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module GroupedGrammar.Transformations.Utils(
	module GroupedGrammar.Transformations.Utils,
	module GroupedGrammar.Transformations.VarNameMonad,
	module Utils.Logging,
	module Control.Monad.Except
) where

import GroupedGrammar.Transformations.Types
import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types
import Utils.Graph
import Utils.Logging

import Control.Monad.Identity
import Control.Monad.Except

import qualified Data.Map as M


type TransformationImplType prodTag symbolTag =
	GroupedGrammarTagged symbolTag -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged symbolTag) -> GroupedGrammar_SeparateProdTags prodTag symbolTag

type TransformationImplTypeM prodTag symbolTag m =
	GroupedGrammarTagged symbolTag -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged symbolTag) -> m (GroupedGrammar_SeparateProdTags prodTag symbolTag)

type Zipper a = ([a], a, [a]) -- (preceding, selected, rest)

fromZipper (preceding, selected, rest) =
	preceding ++ [selected] ++ rest

selectProd :: (prod -> Bool) -> [prod] -> Maybe (Zipper prod)
selectProd cond prods =
	case break cond prods of
		(preceding, prod:rest) ->
			return $ (preceding, prod, rest)
		_ -> Nothing

-- | move zipper to next element which fulfills the condition
nextSelection :: (prod -> Bool) -> Zipper prod -> Maybe (Zipper prod)
nextSelection cond (lastPreceding, lastSelected, lastRest) =
	flip fmap (selectProd cond lastRest) $
		\(preceding, selected, rest) ->
			(lastPreceding ++ [lastSelected] ++ preceding, selected, rest)

applyAlgorithmUsingProductions f =
	runIdentity .
	applyAlgorithmUsingProductionsM (return . f)

applyAlgorithmUsingProductionsM ::
	forall m prodTag .
	Monad m =>
	([ProductionGen Var [[Symbol]]] -> m [ProductionGen Var [[Symbol]]])
	-> GroupedGrammarTagged [SymbolTag]
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
applyAlgorithmUsingProductionsM algorithm grammar =
	ggSeparateProdTags_mapToGrammarM calcGrammar $
	GroupedGrammar_SeparateProdTags grammar M.empty
	where
		calcGrammar :: forall tag . GroupedGrammarTagged [tag] -> m (GroupedGrammarTagged [tag])
		calcGrammar =
			grammar_mapToProductionsM $
			withUntaggedProductionsM $
				algorithm 
			where
				withUntaggedProductionsM ::
					([ProductionGen Var [[Symbol]]] -> m [ProductionGen Var [[Symbol]]])
					-> [GroupedProductionTagged [tag]] -> m [GroupedProductionTagged [tag]]
				withUntaggedProductionsM f =
					fmap (map $ groupedProd_addSymbolTags [])
					.
					f
					.
					map groupedProd_removeSymbolTags

{-
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
	-> GroupedGrammarTagged [SymbolTag]
	-> M.Map Var prodTag
	-> Graph Symbol (GroupedProductionTagged [SymbolTag])
	-> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
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
-}

type ProcessedAndRemaining a = ([a],[a])

processAll ::
	(ProcessedAndRemaining a ->  a -> ProcessedAndRemaining a) -> [a] -> [a]
processAll f =
	runIdentity .
	processAllM (\processedAndRemaining current-> return $ f processedAndRemaining current)

processAllOnceM ::
	Monad m =>
	(ProcessedAndRemaining a -> a -> m [a])
	-> [a] -> m [a]
processAllOnceM f =
	processAllM $
	\processedAndRemaining currentElem -> fmap (,[]) $ f processedAndRemaining currentElem

processAllM ::
	forall a m . Monad m =>
	(ProcessedAndRemaining a ->  a -> m (ProcessedAndRemaining a)) -> [a] -> m [a]
processAllM f l =
	fmap fst $
	processAllM' f ([], l)
	where
		processAllM' :: (ProcessedAndRemaining a -> a -> m (ProcessedAndRemaining a)) -> ProcessedAndRemaining a -> m (ProcessedAndRemaining a)
		processAllM' f partition@(processed, remaining) =
			case remaining of
				[] -> return $ partition
				(x:xs) ->
					do
						(newElemsFinished, newElemsToBeProcessedAgain) <- f (processed,remaining) x
						processAllM' f (processed ++ newElemsFinished, newElemsToBeProcessedAgain ++ xs)

{- |
	`maybeUnfold cond allOtherProductions prod` does the following:
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
