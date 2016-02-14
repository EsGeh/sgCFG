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
	applyAlgorithmUsingProductions varScheme $ processAll_LeftFactoring leftFactoringStep

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
							liftM ([x] ++) $ processAll_LeftFactoring f xs
					_ ->
						--liftM (newElems ++) $ (processAll_LeftFactoring f $ xs)
						processAll_LeftFactoring f $ newElems ++ xs

leftFactoringStep :: GroupedProduction -> VarNameMonad [GroupedProduction]
leftFactoringStep prod =
		liftM (joinProductions . join) $
		mapM calcNewProd $
		(map $ mapSnd $ map $ \x -> if x == [] then [Left epsilon] else x) $
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
var = Right . Var

joinProductions ::
	[ProductionGen Var [[Symbol]]] -> [GroupedProduction]
joinProductions =
	map join_ . groupBy (\a b -> prod_left a == prod_left b) . sortOn prod_left
	where
		join_ productions =
			case productions of
				[] -> error "joinProductions error"
				hd:_ -> Production (prod_left hd) $ concat $ map prod_right productions

-- if two or more lists have the same prefix (/= epsilon), they are returned as a group.
{- e.g.
	groupByPrefix ["abcdef", "abc", "abcHURZ", "xyz"] == [("xyz",[]), ("abc",["HURZ", "", "def"]) ]
-}
groupByPrefix ::
	(Eq a, Ord a) => [[a]] -> [([a], [[a]])]
groupByPrefix l =
	foldl conc init $
	sortBy lexic l
	where
		conc :: (Eq a, Ord a) => [([a],[[a]])] -> [a] -> [([a],[[a]])] 
		conc res l2 =
				case res of
					((pref, rests):xs) ->
						case longestCommonPrefix' pref l2 of
							(pref', _, newRest2) | pref' /= [] ->
								(pref'
								, newRest2
								  : map (drop (length pref') pref ++) rests
								)
								: xs
							_ -> (l2,[[]]):res
					_ -> [(l2, [[]])]

		init = []

lexic a b =
	case (a,b) of
		(x:xs, y:ys) | x == y -> lexic xs ys
		(x:_, y:_) -> compare x y
		([],[]) -> EQ
		([],_) -> LT
		(_,[]) -> GT

longestCommonPrefix' :: Eq a => [a] -> [a] -> ([a],[a],[a])
longestCommonPrefix' l1 l2 =
	case (l1, l2) of
		(x:xs, y:ys) | x == y ->
			let (pref, rest1, rest2) = longestCommonPrefix' xs ys
			in
				(x : pref, rest1, rest2)
		_ -> ([], l1, l2)

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
