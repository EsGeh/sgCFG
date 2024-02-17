{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils.Graph where

import Utils

import qualified Data.Graph as G
import qualified Data.Tree as Tree


data Graph key node
	= Graph {
		graph_graph :: G.Graph,
		graph_nodeFromVertex :: G.Vertex -> (node, key, [key]),
		graph_vertexFromKey :: key -> Maybe G.Vertex
	}

graph_nodeKeys :: Graph key node -> [key]
graph_nodeKeys g =
	map (xtract . graph_nodeFromVertex g) $
	G.vertices $ graph_graph g
	where
		xtract (_, x, _) = x

graphFromEdges :: Ord key => [(node, key, [key])] -> Graph key node
graphFromEdges =
	uncurry3 Graph . G.graphFromEdges

-- (dfs)
spanningForest :: [key] -> Graph key node -> Maybe [Tree.Tree (key, node)]
spanningForest vars graph =
	fmap (
		map (fmap $ lookupVertex graph) .
		G.dfs (graph_graph graph)
	) $ 
	mapM (graph_vertexFromKey graph) $ vars

lookupVertex :: Graph key node -> Int -> (key, node)
lookupVertex graph vertex =
	let (node, key, _) = graph_nodeFromVertex graph vertex
	in
		(key, node)

-- all nodes reachable from a given node
hull :: Graph key node -> key -> Maybe [(key, node)]
hull graph key =
	fmap (
		map (lookupVertex graph) .
		G.reachable (graph_graph graph)
	) $
	graph_vertexFromKey graph key

isReachable :: Graph key node -> key -> key -> Maybe Bool
isReachable graph k1 k2 =
	G.path (graph_graph graph) <$>
		(graph_vertexFromKey graph $ k1) <*>
		(graph_vertexFromKey graph $ k2)
