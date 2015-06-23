{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils.Graph where

import Utils

import qualified Data.Graph as G
import qualified Data.Tree as Tree
import Control.Monad
--import Control.Applicative


data Graph key node
	= Graph {
		graph_graph :: G.Graph,
		graph_nodeFromVertex :: G.Vertex -> (node, key, [key]),
		graph_vertexFromKey :: key -> Maybe G.Vertex
	}

graphFromEdges =
	uncurry3 Graph . G.graphFromEdges

-- (dfs)
spanningForest :: [key] -> Graph key node -> Maybe [Tree.Tree (key, node)]
spanningForest vars graph =
	liftM (
		map (fmap $ lookupVertex graph) .
		G.dfs (graph_graph graph)
	) $ 
	mapM (graph_vertexFromKey graph) $ vars

lookupVertex graph vertex =
	let (node, key, _) = graph_nodeFromVertex graph vertex
	in
		(key, node)

-- all nodes reachable from a given node
hull graph key =
	liftM (
		map (lookupVertex graph) .
		G.reachable (graph_graph graph)
	) $
	graph_vertexFromKey graph key

isReachable graph k1 k2 =
	G.path (graph_graph graph) <$>
		(graph_vertexFromKey graph $ k1) <*>
		(graph_vertexFromKey graph $ k2)

{-
--spanningForest :: [Var] -> GrammarGraph -> Maybe [Tree.Tree GroupedProduction]
spanningForest vars graph =
		do
			keyVertices <-
				mapM (graph_vertexFromKey graph . Right) $ vars
				:: Maybe [G.Vertex]
			let spanningForest = G.dfs (graph_graph graph) keyVertices :: [Tree.Tree G.Vertex]
			return $
				map (fmap $ lookupVertex) $
				spanningForest
		where
			lookupVertex vertex =
				let (node, key, destinations) = graph_nodeFromVertex graph vertex
				in
					case key of
						(Right var) -> node
						_ -> error "spanning forest error"
-}
