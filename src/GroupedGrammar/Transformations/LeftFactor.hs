{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Transformations.Types
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types
import Utils
import Types

import Control.Monad.State
import qualified Data.Set as S
import Data.Maybe

--import qualified Debug.Trace as Trace


leftFactor ::
	(MonadLog m, MonadError String m) =>
	VarScheme
	-> TransformationImplTypeM prodTag [SymbolTag] m
leftFactor varScheme grammar _ _ =
	runVarNameMonadT varScheme (grammar_mapToProductions (map $ groupedProd_removeSymbolTags) $ grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
	--processAll_LeftFactoringM $
	processAllOnceM $
		\_ x ->
			fmap (uncurry (++)) $
			untilExtM (==) leftFactoringStep
			([],[x])

leftFactoringStep ::
	MonadLog m => 
	([GroupedProduction], [GroupedProduction])
	-> VarNameMonadT m ([GroupedProduction], [GroupedProduction])
leftFactoringStep (oldBlock, newBlock) =
	do
		(newProds, nextBlock) <-
			fmap (
				mapSnd join
				.
				unzip
			) $
			mapM leftFactorProd newBlock
		return $
			(oldBlock ++ newProds, nextBlock)

data FullLeftFState = FullLeftFState {
	fullLeftFState_it :: Int,
	fullLeftFState_stopAtTheseProds :: StopAtSet
}
type StopAtSet = S.Set GroupedProduction

fullLeftFState_mapToIt f s = s{ fullLeftFState_it = f (fullLeftFState_it s) }
fullLeftFState_mapToStopAt f s = s{ fullLeftFState_stopAtTheseProds = f (fullLeftFState_stopAtTheseProds s) }

fullLeftFState_default = FullLeftFState 0 S.empty

getIt :: MonadState FullLeftFState m => m Int
getIt = fmap fullLeftFState_it $ get
incIt :: MonadState FullLeftFState m => m ()
incIt = modify $ fullLeftFState_mapToIt (+1)
getStopAt :: MonadState FullLeftFState m => m StopAtSet
getStopAt = fmap fullLeftFState_stopAtTheseProds $ get
modifyStopAt :: MonadState FullLeftFState m =>(StopAtSet -> StopAtSet) -> m ()
modifyStopAt f = modify $ fullLeftFState_mapToStopAt f

unfoldCondFromDescr condDescr iteration unfolded prod =
	case condDescr of
		IterateWhileDecreasing ->
			(sum $ map lengthSum unfolded) < lengthSum prod
			where
				lengthSum :: GroupedProduction -> Int
				lengthSum = length . join . prod_right
		IterateNTimes maxRec ->
			iteration < maxRec

leftFactor_full ::
	forall m prodTag .
	MonadLog m =>
	FullLeftFactorIterateWhile
	-> (Var -> Bool) -> VarScheme
	-> TransformationImplTypeM prodTag [SymbolTag] m
leftFactor_full unfoldCondDescr varCond varScheme grammar _ _ =
	runVarNameMonadT varScheme (grammar_mapToProductions (map $ groupedProd_removeSymbolTags) $ grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
	flip evalStateT fullLeftFState_default .
	(untilExtM (==) $ transformProds)
	where
		transformProds ::
			[ProductionGen Var [[Symbol]]]
			-> StateT FullLeftFState (VarNameMonadT m) [GroupedProduction]
		transformProds =
				\prods ->
					do
						it_index <- getIt
						lift $ doLog $ "iteration " ++ show it_index
						newProds <- lift $
							flip processAllOnceM prods $
							transformProd (unfoldCondFromDescr unfoldCondDescr it_index)
						incIt
						return newProds
		transformProd ::
			([GroupedProduction] -> GroupedProduction -> Bool)
			-> ProcessedAndRemaining GroupedProduction
			-> GroupedProduction
			-> VarNameMonadT m [GroupedProduction]
		transformProd unfoldCond _ prod =
			--((lift $ doLog $ unlines $ ["leftFactorFull starting with", pretty $ prod_left prod]) >>) $
			do
				(newProd, continuations) <-
					((leftFactorProd prod) >>=) $ mapSndM $
					(fmap join .) $
					mapM $
						\cont ->
							let unfolded = maybeUnfoldGrouped varCond allProds cont
							in
								if unfoldCond unfolded prod
								then
									return $ unfolded
								else
									do
										doLog $ "skipping unfolding of prod:" ++ pretty cont
										return $ [cont]
				unless (null continuations) $
				-- when (newProd/=prod) $
					doLog $
						unlines $
						[ "splitting"
						, "\t" ++ pretty (prod_left prod)
						, "into"
						, "\t" ++ pretty (prod_left newProd)
						, "and"
						]
						++
						map (("\t"++) . pretty . prod_left) continuations
				return $ newProd : continuations
			where
				allProds =
					fromGrammar $ fromTaggedGrammar $ grammar


type Node = (GroupedProduction, [GroupedProduction])
type Path = [Node]

instance MonadLog m => MonadLog (VarNameMonadT m) where
	doLog = lift . doLog

{-
leftFactorAndUnfold ::
	Monad m => 
	(Var -> Bool)
	-> [GroupedProduction]
	-> GroupedProduction
	-> VarNameMonadT m (GroupedProduction, [GroupedProduction])
leftFactorAndUnfold cond allOtherProds input =
		--((lift $ doLog $ unlines $ ["leftFactorAndUnfold"] ++ [pretty input]) >>) $
		fmap (mapSnd $ join . (map $ maybeUnfoldGrouped cond allOtherProds)) $
		leftFactorProd $
		input
-}

maybeUnfoldGrouped ::
	(Var -> Bool)
	-> [GroupedProduction]
	-> GroupedProduction -> [GroupedProduction]
maybeUnfoldGrouped cond allOtherProds =
		toGroupedProductions
		.
		((
			\prod -> case maybeUnfold cond allOtherProds prod of
				[] -> [prod]
				newProds -> newProds
		)
		<=<
		productionsFromGroupedProd
		)

leftFactorProd ::
	forall m .
	Monad m =>
	GroupedProduction
	-> VarNameMonadT m (GroupedProduction, [GroupedProduction]) -- new production and a list of "continuations"
leftFactorProd prod =
	fmap (temp $ prod_left prod) $
	mapM (
		calcNewProd 
		.
		(mapSnd $ (map $ \x -> if null x then [Left epsilon] else x))
	)$
	groupByPrefix $
	prod_right prod
	where
		calcNewProd :: ([Symbol], [[Symbol]]) -> VarNameMonadT m ([Symbol], Maybe GroupedProduction)
		calcNewProd rules =
			case rules of
				(pref, rests) | (all $ all (==Left epsilon)) rests ->
					return $
					( pref
					, Nothing
					)
				(pref, rests) ->
					do
						newVar <- getSimilarVar (prod_left prod)
						return $
							( pref ++ [Right newVar]
							, Just $ Production newVar rests
							)
		temp ::
			Var
			-> [([Symbol], Maybe GroupedProduction)]
			-> (GroupedProduction, [GroupedProduction])
		temp left input =
			( Production left $ map fst input
			, catMaybes $ map snd $ input
			)

{-
joinProductions ::
	[GroupedProduction] -> [GroupedProduction]
joinProductions =
	map join_ . groupBy (\a b -> prod_left a == prod_left b) . sortOn prod_left
	where
		join_ productions =
			case productions of
				[] -> error "joinProductions error"
				hd:_ -> Production (prod_left hd) $ concatMap prod_right productions

logIfChanged :: MonadLog m => GroupedProduction -> [GroupedProduction] -> m [GroupedProduction]
logIfChanged old new =
	do
		when (length new /= 1) $ 
			doLog (
				concat $
				[ "expanded:\n\t"
				, pretty old
				, "\nto\n"
				, unlines $ map (("\t" ++) . pretty) new
				]
			)
		return $ new
-}
