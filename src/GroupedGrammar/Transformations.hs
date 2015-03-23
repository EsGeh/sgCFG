{-# LANGUAGE MultiParamTypeClasses #-}
module GroupedGrammar.Transformations where

import GroupedGrammar.Internals
import Types
import GrammarTypes
import GrammarFormat
import Utils.Graph

import qualified Data.Tree as Tree
import Data.List
import Data.Maybe
import Control.Monad


applyTransformation :: Transformation -> GroupedGrammarTagged -> Maybe GroupedGrammarTagged
applyTransformation t g =
	let
		graph = graphFromGroupedGrammar $ fromTaggedGrammar g
	in
		case t of
			Annotate info ->
				case info of
					AnnotateWithLoops -> do
						startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
						newGrammar <- findLoops startSym graph
						return $ g `intersectTaggedGrammars` newGrammar
			SubGrammar var ->
				do
					subGrammar <- groupedGrammarSub [var] graph
					return $
						toTaggedGrammar subGrammar `intersectTaggedGrammars` g
			UnusedRules ->
				 maybe (Just $ Grammar []) Just $
				do
					startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
					used <- (groupedGrammarSub [startSym] graph)
					return $
						toTaggedGrammar $
						Grammar $
						fromGrammar (fromTaggedGrammar g)
						\\
						fromGrammar used

data Transformation
	= Annotate AnnotateInfo
	| SubGrammar SubGrammarInfo
	| UnusedRules
	deriving (Show)

type SubGrammarInfo = Var
data AnnotateInfo
	= AnnotateWithLoops
	deriving (Show)

type GroupedGrammarTagged =
	GrammarGen GroupedProductionTagged
type GroupedProductionTagged =
	GroupedProductionGen Var (Tagged [SymbolTag] Symbol)

data SymbolTag
	= ProductionRef Var

instance ToTextAs GrammarFormat SymbolTag where
	toTextAs _ t = "!"

type GrammarGraph =
	Graph Symbol (GroupedProductionGen Var Symbol)
--type GrammarNode = Maybe Symbol
-- Nothing means "or"

toTaggedGrammar :: GroupedGrammar -> GroupedGrammarTagged
toTaggedGrammar =
	fmap $ toTaggedProduction

fromTaggedGrammar :: GroupedGrammarTagged -> GroupedGrammar
fromTaggedGrammar =
	fmap $ fromTaggedProduction

toTaggedProduction =
	prod_mapToRight $ map $ map $ Tagged []

fromTaggedProduction = 
	prod_mapToRight $ map $ map $ value

type SeedType = (Tree.Tree GroupedProduction, [GroupedProduction])

intersectTaggedGrammars g1 g2 =
	Grammar $
	let
		productions1 = fromGrammar g1
		productions2 = fromGrammar g2
	in
		productions1
		`merge`
		productions2
		where
			compareLeft a b = fromTaggedProduction a `compare` fromTaggedProduction  b
			merge :: [GroupedProductionTagged] -> [GroupedProductionTagged] -> [GroupedProductionTagged]
			merge p1 p2 =
				case p1 of
					(x:xs) ->
						maybe (merge xs p2) (\y -> mergeProductions x y : merge xs p2) $
							find ((==fromTaggedProduction x) . fromTaggedProduction) p2
					_ -> []
			mergeProductions :: GroupedProductionTagged -> GroupedProductionTagged -> GroupedProductionTagged
			mergeProductions p1 p2 = 
				Production
					(prod_left p1) $
					zipWith
						(zipWith $ \a b -> tagged_mapToTag (++ tag b) a)
						(prod_right p1)
						(prod_right p2)

{-
intersectTaggedGrammars g1 g2 =
	Grammar $
	let
		productions1 = fromGrammar g1
		productions2 = fromGrammar g2
	in
		sortBy compareLeft productions1
		`merge`
		sortBy compareLeft productions2
		where
			compareLeft a b = fromTaggedProduction a `compare` fromTaggedProduction  b
			merge :: [GroupedProductionTagged] -> [GroupedProductionTagged] -> [GroupedProductionTagged]
			merge p1 p2 =
				case p1 of
					(x:xs) ->
						case 
							span (
								(< fromTaggedProduction x) .
								fromTaggedProduction
							) $
							p2
						of
							(_, y:ys) | fromTaggedProduction y == fromTaggedProduction x ->
								mergeProductions x y: merge xs ys
							(_, ys) -> merge xs ys
					_ -> []
			mergeProductions :: GroupedProductionTagged -> GroupedProductionTagged -> GroupedProductionTagged
			mergeProductions p1 p2 = 
				Production
					(prod_left p1) $
					zipWith
						(zipWith $ \a b -> tagged_mapToTag (++ tag b) a)
						(prod_right p1)
						(prod_right p2)
-}

--graphFromGroupedGrammar :: GroupedGrammar -> GrammarGraph
graphFromGroupedGrammar g =
	let list = map f $ fromGrammar g
	in
		graphFromEdges list
	where
		--f :: GroupedProduction -> (GroupedProduction, Symbol, [Symbol])
		f prod =
			( prod -- node
			, Right $ prod_left prod -- key
			, join $ prod_right prod) -- [key]

findLoops ::
	Var -> GrammarGraph ->
	Maybe GroupedGrammarTagged
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
				f :: GroupedProduction -> [Var] -> GroupedProductionGen Var (Tagged [SymbolTag] Symbol)
				f prod backEdgeVars =
					(prod_mapToRight $ map $ map tagMaybe) prod
					where
						tagMaybe sym =
							case sym of
								Right var | var `elem` backEdgeVars ->
									Tagged [ProductionRef var] sym 
								_ -> Tagged [] sym
		_ -> Nothing

findLoopsInTree :: GrammarGraph -> Tree.Tree GroupedProduction -> Tree.Tree (GroupedProduction, [Var])
findLoopsInTree graph tree =
	Tree.unfoldTree unfoldF (tree, [])
		:: Tree.Tree (GroupedProduction, [Var])
	where
		unfoldF :: SeedType -> ((GroupedProduction, [Var]), [SeedType])
		unfoldF (tree, path) = (newNode, newSeeds) 
			where
				newNode :: (GroupedProduction, [Var])
				newNode =
					(label, allBackEdges)
					where
						allBackEdges :: [Var]
						allBackEdges =
							filter cond $ map prod_left path
						cond var =
							maybe False id $
							isReachable graph
								(Right $ prod_left label)
								(Right var)
				newSeeds =
					map calcSeeds $ Tree.subForest tree
					where
						calcSeeds subTree = (subTree, label:path)
				label = Tree.rootLabel tree

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
			, prod_left prod -- key
			, join $ (map Either.rights) $ prod_right prod)
			--, intercalate [Nothing] $ map (map Just) $ prod_right prod)
-}
