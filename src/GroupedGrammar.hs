{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GroupedGrammar(
	module GroupedGrammar.Internals,
	groupedGrammarSub,
	--spanningForest,
	graphFromGroupedGrammar,
	groupedGrammarFromTokens,
	groupedGrammarFromStr
) where

import GrammarTypes
import GrammarFormat
import GroupedGrammar.Internals
import GroupedGrammar.Parse
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Parse.Token
import Types
import Utils
import Utils.Graph

import qualified Data.Tree as Tree
import qualified Text.Parsec as P
import Control.Monad
import qualified Data.Either as Either
import Data.List
import qualified Data.Foldable as Fold
import Control.Applicative

groupedGrammarFromStr descr =
	groupedGrammarFromTokens
	<=<
	tokensFromStr descr

groupedGrammarFromTokens =
	mapLeft show . P.parse parseGroupedGrammar ""

type SeedType = (Tree.Tree GroupedProduction, [GroupedProduction])

findLoops ::
	Var -> GrammarGraph ->
	Maybe (GrammarGen (GroupedProductionGen Var (Tagged (Maybe Var) Symbol)))
findLoops startSymbol graph = do
	forest <- return . map (fmap snd) =<< spanningForest [Right startSymbol] graph
	case map (findLoopsInTree graph) forest of
		[tree] ->
			return $
				Grammar $
				map (uncurry f) $
				Tree.flatten $
				tree
			where
				f :: GroupedProduction -> [Var] -> GroupedProductionGen Var (Tagged (Maybe Var) Symbol)
				f prod backEdgeVars =
					(groupedProdMapToRight $ map $ map tagMaybe) prod
					where
						tagMaybe sym =
							case sym of
								Right var | var `elem` backEdgeVars ->
									Tagged (Just var) sym 
								_ -> Tagged Nothing sym
		_ -> Nothing

findLoopsInTree :: GrammarGraph -> Tree.Tree GroupedProduction -> Tree.Tree (GroupedProduction, [Var])
findLoopsInTree graph tree =
	Tree.unfoldTree f (tree, [])
		:: Tree.Tree (GroupedProduction, [Var])
	where
		f :: SeedType -> ((GroupedProduction, [Var]), [SeedType])
		f (tree, path) = (newNode, newSeeds) 
			where
				newNode :: (GroupedProduction, [Var])
				newNode =
					(label, allBackEdges)
					where
						allBackEdges :: [Var]
						allBackEdges =
							filter cond $ map groupedProd_left path
						cond var =
							maybe False id $
							isReachable graph
								(Right $ groupedProd_left label)
								(Right var)
				newSeeds =
					map calcSeeds $ Tree.subForest tree
					where
						calcSeeds subTree = (subTree, label:path)
				label = Tree.rootLabel tree

test =
	findLoops (Var "a") $
	graphFromGroupedGrammar $
	testGrammar
	{-
	where
		showNode (prod, loops) =
			toTextAs (defaultFormat Default) prod
			++ (show $ map pretty loops)
	-}

testGrammar =
	either (const $ error "") id $
	fromTextAs (defaultFormat Default) "a -> b | c\nb -> a\nc -> \"d\""

groupedGrammarSub :: [Var] -> GrammarGraph -> Maybe GroupedGrammar
groupedGrammarSub vars graph =
	liftM (
		Grammar .
		join .
		map (Tree.flatten . fmap snd)
	) $
	spanningForest (map Right vars) graph

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

type GrammarGraph = Graph Symbol GroupedProduction
--type GrammarNode = Maybe Symbol
-- Nothing means "or"

graphFromGroupedGrammar :: GroupedGrammar -> GrammarGraph
graphFromGroupedGrammar g =
	let list = map f $ fromGrammar g
	in
		graphFromEdges list
	where
		f :: GroupedProduction -> (GroupedProduction, Symbol, [Symbol])
		f prod =
			( prod -- node
			, Right $ groupedProd_left prod -- key
			, join $ groupedProd_right prod) -- [key]

{-
graphFromGroupedGrammar :: GroupedGrammar -> Graph Var GroupedProduction 
graphFromGroupedGrammar g =
	let list = map f $ fromGroupedGrammar g
	in
		uncurry3 Graph $ G.graphFromEdges list
	where
		f :: GroupedProduction -> (GroupedProduction, Var, [Var])
		f prod =
			( prod -- node
			, groupedProd_left prod -- key
			, join $ (map Either.rights) $ groupedProd_right prod)
			--, intercalate [Nothing] $ map (map Just) $ groupedProd_right prod)
-}

instance FromTextAs GrammarFormat GroupedGrammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ groupedGrammarFromStr parseFormat str
