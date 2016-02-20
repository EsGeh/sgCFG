{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Utils
import Grammar.Types
import Utils

import Data.List
import Control.Monad

--import Debug.Trace

leftFactor varScheme =
	applyAlgorithmUsingProductionsM varScheme $
	processAll_LeftFactoring leftFactoringStep


processAll_LeftFactoring ::
	forall a m . Monad m =>
	(a -> m [a]) -> [a] -> m [a]
processAll_LeftFactoring f l =
	case l of
		[] -> return []
		(x:xs) ->
			do
				newElems <- f x :: m [a]
				case newElems of
					[_] ->
							fmap ([x] ++) $ processAll_LeftFactoring f xs
					_ ->
						--liftM (newElems ++) $ (processAll_LeftFactoring f $ xs)
						processAll_LeftFactoring f $ newElems ++ xs

leftFactoringStep :: GroupedProduction -> VarNameMonad [GroupedProduction]
leftFactoringStep prod =
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

{-
ignoreTags ::
	forall tag .
	(GroupedProduction -> GroupedProduction)
	-> GroupedProductionTagged tag -> GroupedProductionTagged tag
ignoreTags f prod =
	let
		right = prod_right $ prod
		tagMap = (map $ map $ tag) $ right :: [[tag]]
	in
		(prod_mapToRight $ to tagMap) $ f $ (prod_mapToRight from) $ prod
		where
			from = (map $ map $ value)
			to tagMap right' = zipWith (zipWith tagged) tagMap right'
-}
