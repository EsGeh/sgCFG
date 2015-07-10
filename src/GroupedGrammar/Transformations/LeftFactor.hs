{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Internals
import GroupedGrammar.Transformations.Types
import GrammarTypes
import Utils.Graph

import qualified Data.Map as M
import Data.List
import Control.Monad

--import Debug.Trace


leftFactor ::
	GroupedGrammarTagged [SymbolTag] -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged [SymbolTag]) -> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
leftFactor grammar _ _ =
	return $
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar = newGrammar grammar,
		ggSeparateProdTags_ruleAnnotations = M.empty
	}
	where
		newGrammar :: forall tag . GroupedGrammarTagged [tag] -> GroupedGrammarTagged [tag]
		newGrammar grammar =
			Grammar $
			map (prod_mapToRight $ map $ map $ tagged []) $
			f $
			map (prod_mapToRight $ map $ map $ value) $
			fromGrammar grammar

			where
				f :: [ProductionGen Var [[Symbol]]] -> [ProductionGen Var [[Symbol]]]
				f =
					flip runVarNameMonad (fmap (prod_mapToRight $ map $ map $ value) grammar)
					.
					g 

				g :: [ProductionGen Var [[Symbol]]] -> VarNameMonad [ProductionGen Var [[Symbol]]]
				g productions =
					case productions of
						[] ->  return []
						(prod:otherProds) ->
							do
								newProds <- splitProduction prod
								case newProds of
									[_] -> -- the production hadn't been needed to split:
											liftM ([prod] ++) $ g otherProds
									_ ->
										g $ newProds ++ otherProds

splitProduction :: GroupedProduction -> VarNameMonad [GroupedProduction]
splitProduction prod =
	let
		right = prod_right prod :: [[Symbol]]
	in
		liftM (joinProductions . join) $
		mapM calcNewProd $
		groupByPrefix right
		where
			calcNewProd :: ([Symbol], [[Symbol]]) -> VarNameMonad [ProductionGen Var [[Symbol]]]
			calcNewProd rules =
				--trace ("calcNewProd with" ++ show rules) $
				case rules of
					(pref, [[]]) ->
						return $ return $ Production (prod_left prod) [pref]
					(pref, rests) ->
						do
							newVar <- getNewVar
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

-- if two or more lists have the same prefix, they are returned as a group.
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
