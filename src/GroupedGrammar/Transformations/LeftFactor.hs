{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types
import Utils
import Types

import Data.List hiding( unlines )
import Control.Monad
import Prelude hiding( unlines )

import qualified Debug.Trace as Trace

leftFactor_full varCond varScheme =
	applyAlgorithmUsingProductionsM varScheme $
	processAll_LeftFactoringM $
	\ctxt x ->
		fmap (\p -> toGroupedProductions $ maybeUnfold varCond (uncurry (++) $ ctxt) =<< productionsFromGroupedProd =<< p) $
		leftFactoringStep x

leftFactor varScheme =
	applyAlgorithmUsingProductionsM varScheme $
	processAll_LeftFactoringM $
	\_ x -> leftFactoringStep x

processAll_LeftFactoringM ::
	(Eq a, Monad m) =>
	(ProcessedAndRemaining a -> a -> m [a])
	-> [a] -> m [a]
processAll_LeftFactoringM f =
	processAllM $
	\ctxt x -> f ctxt x >>=
		\new ->
			return $
			case new of
				new
					| new == [x] ->
						(new, []) --
					| otherwise ->
						([], new) -- process again...

leftFactoringStep :: GroupedProduction -> VarNameMonad [GroupedProduction]
leftFactoringStep prod =
	--fmap (traceIfChanged prod) $
	fmap (joinProductions . join) $
	mapM (
		calcNewProd
		. (mapSnd $ map $ \x -> if null x then [Left epsilon] else x)
	) $
	groupByPrefix $
	(prod_right prod :: [[Symbol]])
	where
		calcNewProd :: ([Symbol], [[Symbol]]) -> VarNameMonad [ProductionGen Var [[Symbol]]]
		calcNewProd rules =
			case rules of
				(pref, rests) | (all $ all (==Left epsilon)) rests ->
				-- (pref, [[]]) ->
					return $ return $ Production (prod_left prod) [pref]
				(pref, rests) ->
					do
						newVar <- getSimilarVar $ prod_left prod
						return $
							Production (prod_left prod) [pref ++ [Right newVar]]
							: [Production newVar rests]

joinProductions ::
	[ProductionGen Var [[Symbol]]] -> [GroupedProduction]
joinProductions =
	map join_ . groupBy (\a b -> prod_left a == prod_left b) . sortOn prod_left
	where
		join_ productions =
			case productions of
				[] -> error "joinProductions error"
				hd:_ -> Production (prod_left hd) $ concatMap prod_right productions

traceIfChanged :: GroupedProduction -> [GroupedProduction] -> [GroupedProduction]
traceIfChanged old new =
	(
		--if [prod_mapToRight (groupByPrefix) old] /= new
		if length new /= 1
		--if [old] /= new
		then
			Trace.trace (
				concat $
				[ "expanded:\n\""
				, pretty old
				, "\"\nto \n\""
				, unlines $ map pretty new
				, "\""
				]
			)
		else
			id
	)
	new
