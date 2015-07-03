{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations where

import GroupedGrammar.Internals
import Types
import GrammarTypes
import GrammarFormat
import Utils.Graph

import qualified Data.Tree as Tree
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad


type GroupedGrammarTagged symbolTag =
	GrammarGen (GroupedProductionTagged symbolTag)
type GroupedGrammar_ProdAndSymbolsTagged productionTag symbolTag =
	GrammarGen (GroupedProduction_ProdAndSymbolsTagged productionTag symbolTag)

type GroupedProductionTagged symbolTag =
	GroupedProductionGen Var (Tagged symbolTag Symbol)
type GroupedProduction_ProdAndSymbolsTagged productionTag symbolTag =
	Tagged productionTag (GroupedProductionTagged symbolTag)

data ProductionTag = ProductionTag {
		prodTag_firstSet :: Maybe FirstSet
	}
	deriving( Eq, Ord )

prodTag_empty = ProductionTag Nothing
prodTag_mapToFirstSet f t = t{ prodTag_firstSet = f (prodTag_firstSet t) }

type FirstSet = S.Set Terminal

toProdAndSymbolsTagged ::
	productionTag ->
		GroupedGrammarTagged symbolTag -> GroupedGrammar_ProdAndSymbolsTagged productionTag symbolTag
toProdAndSymbolsTagged defTag =
	fmap $ tagged defTag

applyTransformation ::
	Transformation -> GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag]-> Maybe (GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag])
applyTransformation t g' =
	let
		(g, prodTags) =
			let
				separateProdTags = toSeparateProdTags g'
			in
				( ggSeparateProdTags_grammar separateProdTags 
				, ggSeparateProdTags_ruleAnnotations separateProdTags :: M.Map Var ProductionTag)
		graph =
			graphFromGroupedGrammar
				(Right . prod_left) -- key from production
				(map value . join . prod_right) -- destinations from production
				$
					--fromTaggedGrammar
					g
					:: Graph Symbol (GroupedProductionTagged [SymbolTag])
					-- :: Graph Symbol (ProductionGen Var [[Symbol]])
	in
		case t of
			Annotate info ->
				case info of
					AnnotateWithLoops -> do
						startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
						newGrammar <- findLoops startSym graph
						return $
							fromSeparateProdTags $
							GroupedGrammar_SeparateProdTags {
								ggSeparateProdTags_grammar = g `intersectTaggedGrammars` newGrammar,
								ggSeparateProdTags_ruleAnnotations = prodTags
							}
					AnnotateWithFirstSet ->
						(Just . fromSeparateProdTags . conv annotateWithFirstSets . toSeparateProdTags) g'
						where
							conv :: 
								Eq symbolTag
								=> (GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag)
								-> GroupedGrammar_SeparateProdTags ProductionTag symbolTag -> GroupedGrammar_SeparateProdTags ProductionTag symbolTag
							conv f g =
								let originalProdTags = ggSeparateProdTags_ruleAnnotations g
								in
									rejoinWithOriginalProdTags originalProdTags $
									f $
									(ggSeparateProdTags_mapToRuleAnnotations $ M.mapMaybe $ const $ Just $ S.empty) g
									where
										rejoinWithOriginalProdTags origAnn =
											ggSeparateProdTags_mapToRuleAnnotations $
											M.mapMaybeWithKey $
											\var set ->
												let oldAnn = maybe (error "rejoinWithOriginalProdTags error") id $ M.lookup var origAnn
												in
													Just $
													(prodTag_mapToFirstSet $ const $ Just set) $
													oldAnn
			SubGrammar var ->
				do
					subGrammar <- groupedGrammarSub [var] graph
					return $
						fromSeparateProdTags $
						GroupedGrammar_SeparateProdTags {
							ggSeparateProdTags_grammar = {-toTaggedGrammar-} subGrammar `intersectTaggedGrammars` g,
							ggSeparateProdTags_ruleAnnotations = prodTags
						}
			UnusedRules ->
				maybe (Just $ toProdAndSymbolsTagged prodTag_empty $ Grammar []) Just $
				do
					startSym <- liftM prod_left $ listToMaybe $ fromGrammar g
					used <- (groupedGrammarSub [startSym] graph)
					return $
						fromSeparateProdTags $
						GroupedGrammar_SeparateProdTags {
							ggSeparateProdTags_grammar =
								Grammar $
								fromGrammar g
								\\
								fromGrammar used,
							ggSeparateProdTags_ruleAnnotations = prodTags
						}

annotateWithFirstSets ::
	Eq symbolTag
	=> GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag
annotateWithFirstSets g =
	firstTimeNotChanging g $
	iterate firstSetsStep g
	where
		firstTimeNotChanging :: Eq a => a -> [a] -> a
		firstTimeNotChanging def [] = def
		firstTimeNotChanging _ [a] = a
		firstTimeNotChanging _ (a:b:_) | a == b = a
		firstTimeNotChanging def (_:b:bs) = firstTimeNotChanging def $ b:bs

firstSetsStep :: forall symbolTag .
	Eq symbolTag
	=> GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag
firstSetsStep g =
	let
		grammar = ggSeparateProdTags_grammar g :: GroupedGrammarTagged symbolTag
		--ruleAnnotations = ggSeparateProdTags_ruleAnnotations g :: M.Map Var FirstSet
	in
		(ggSeparateProdTags_mapToRuleAnnotations $ updateAnn $ fromTaggedGrammar grammar) g
		where
			updateAnn :: GroupedGrammar -> M.Map Var FirstSet -> M.Map Var FirstSet
			updateAnn grammar ruleAnnotations =
				let
					rules = fromGrammar grammar
				in
					M.fromList $
					map
						(\prod ->
							let
								var = prod_left prod
								oldFirstSet = maybe (error "updateAnn: error looking up old first set") id $ M.lookup var ruleAnnotations
							in
								(var, calcNewFirst (flip M.lookup ruleAnnotations) prod oldFirstSet)
						)
						rules 
					where
						calcNewFirst :: (Var -> Maybe FirstSet) -> GroupedProduction -> FirstSet -> FirstSet
						calcNewFirst lookup prod oldFirstSet =
							let
								--left = prod_left prod
								right = prod_right prod
								func right' =
									case right' of
										((Left t):xs) ->
											if t /= epsilon
											then S.singleton t
											else S.singleton t `S.union` func xs
										((Right var):xs) ->
											let
												directFirst =
													maybe (S.singleton $ Terminal $ "FIRST(" ++ pretty var ++ ")") id $
													lookup var
											in
												if not $ epsilon `S.member` directFirst
												then directFirst
												else
													directFirst `S.union` func xs
										_ -> S.empty
							in
								foldl S.union oldFirstSet $
								map func right
								--S.fromList $ [Terminal "bla"]

{-
data CalcFirstParams =
	CalcFirstParams {
		calcFirst_prod :: GroupedProduction,
		calcFirst_lookup :: Var -> GroupedProduction
	}
-}

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


data Transformation
	= Annotate AnnotateInfo
	| SubGrammar SubGrammarInfo
	| UnusedRules
	deriving (Show)

type SubGrammarInfo = Var
data AnnotateInfo
	= AnnotateWithLoops
	| AnnotateWithFirstSet
	deriving (Show)

data GroupedGrammar_SeparateProdTags productionTag symbolTag =
	GroupedGrammar_SeparateProdTags {
		ggSeparateProdTags_grammar :: GrammarGen (GroupedProductionTagged symbolTag),
		ggSeparateProdTags_ruleAnnotations :: M.Map Var productionTag
	}
ggSeparateProdTags_mapToGrammar f g =
	g{ ggSeparateProdTags_grammar = f (ggSeparateProdTags_grammar g) }
ggSeparateProdTags_mapToRuleAnnotations f g =
	g{ ggSeparateProdTags_ruleAnnotations = f (ggSeparateProdTags_ruleAnnotations g) }

deriving instance
	(Eq productionTag, Eq symbolTag)
	=> Eq (GroupedGrammar_SeparateProdTags productionTag symbolTag)

toSeparateProdTags ::
	GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag -> GroupedGrammar_SeparateProdTags prodTag symbolTag
toSeparateProdTags g =
	GroupedGrammar_SeparateProdTags{
		ggSeparateProdTags_grammar = fmap value g,
		ggSeparateProdTags_ruleAnnotations = ruleAnnotations g
	}
	where
		ruleAnnotations :: (GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag) -> M.Map Var prodTag
		ruleAnnotations =
			M.fromList
			.
			(fmap $ \p -> (prod_left $ value p, tag p))
			.
			fromGrammar

fromSeparateProdTags :: GroupedGrammar_SeparateProdTags prodTag symbolTag -> GroupedGrammar_ProdAndSymbolsTagged prodTag symbolTag
fromSeparateProdTags g =
	let
		grammar = ggSeparateProdTags_grammar g
	in
		fmap annotateProd grammar
	where
		annotateProd prod =
			let annotations = ggSeparateProdTags_ruleAnnotations g
			in
				case M.lookup (prod_left prod) annotations of
					Nothing -> error "fromSeparateProdTags error"
					Just ann -> tagged ann prod

instance ToTextAs GrammarFormat ProductionTag where
	toTextAs format x =
		maybe "" id $
			(prodTag_firstSet x) >>= \set -> return $
				concat $
					[ "FIRST = { "
					, intercalate ", " $ fmap (toTextAs format) $ S.toList $ set
					, "}"
					]

data SymbolTag
	= ProductionRef Var
	deriving( Eq, Ord )

{-
instance Eq (Tagged [SymbolTag] Symbol) where
	a == b =
		value a == value b
-}

instance ToTextAs GrammarFormat SymbolTag where
	toTextAs _ _ = "!"

type GrammarGraph symbolTag =
	Graph Symbol (GroupedProductionTagged symbolTag)
	--Graph Symbol (GroupedProductionGen Var Symbol)
--type GrammarNode = Maybe Symbol
-- Nothing means "or"

toTaggedGrammar :: GroupedGrammar -> GroupedGrammarTagged [symbolTag]
toTaggedGrammar =
	fmap $ toTaggedProduction

fromTaggedGrammar :: GroupedGrammarTagged symbolTag -> GroupedGrammar
fromTaggedGrammar =
	fmap $ fromTaggedProduction

toTaggedProduction =
	prod_mapToRight $ map $ map $ Tagged []

fromTaggedProduction = 
	prod_mapToRight $ map $ map $ value

{-
firstSets :: GroupedGrammarTagged -> GroupedGrammarTagged
firstSets g =
	firstSet :: [ProductionGen Var [[Symbol]]] -> [Symbol] -> [Terminal]
	firstSet productions (sym:rest) =
		case sym of
			Left term ->
				if term == Terminal ""
					then
					else [term]
			Right var -> 
				maybe [] id $
				do
					p <- find ((==var) . prod_left) productions
					prod_right p
-}

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
			--compareLeft a b = fromTaggedProduction a `compare` fromTaggedProduction  b
			--merge :: [GroupedProductionTagged] -> [GroupedProductionTagged] -> [GroupedProductionTagged]
			merge p1 p2 =
				case p1 of
					(x:xs) ->
						maybe (merge xs p2) (\y -> mergeProductions x y : merge xs p2) $
							find ((==fromTaggedProduction x) . fromTaggedProduction) p2
					_ -> []
			--mergeProductions :: GroupedProductionTagged -> GroupedProductionTagged -> GroupedProductionTagged
			mergeProductions p1 p2 = 
				Production
					(prod_left p1) $
					zipWith
						(zipWith $ \a b -> tagged_mapToTag (++ tag b) a)
						(prod_right p1)
						(prod_right p2)

graphFromGroupedGrammar ::
	Ord key =>
	(prod -> key)
	-> (prod -> [key])
	-> GrammarGen prod -> Graph key prod
graphFromGroupedGrammar keyFromProd calcDest g =
	let list = map f $ fromGrammar g
	in
		graphFromEdges list
	where
		--f :: GroupedProduction -> (GroupedProduction, Symbol, [Symbol])
		f prod =
			( prod -- node
			, keyFromProd prod -- key
			, calcDest prod) -- [key]

findLoops ::
	Var -> GrammarGraph [SymbolTag] ->
	Maybe (GroupedGrammarTagged [SymbolTag])
findLoops startSymbol graph = do
	forest <-
		return . map (fmap snd)
		=<<
		spanningForest [Right startSymbol] graph
		:: Maybe [Tree.Tree (GroupedProductionTagged [SymbolTag])]
	case map (findLoopsInTree graph) forest of
		[tree] ->
			return $
				Grammar $
				map (uncurry f) $
				Tree.flatten $
				tree
			where
				f :: GroupedProductionTagged [SymbolTag]-> [Var] -> GroupedProductionGen Var (Tagged [SymbolTag] Symbol)
				f prod backEdgeVars =
					(prod_mapToRight $ map $ map tagMaybe) prod
					where
						tagMaybe ::
							Tagged [SymbolTag] Symbol -> Tagged [SymbolTag] Symbol
						tagMaybe sym =
							case value sym of
								Right var
									| var `elem` backEdgeVars ->
										tagged_mapToTag (ProductionRef var:) sym
										--Tagged [ProductionRef var] sym 
								_ ->
									sym
									--Tagged [] sym
		_ -> Nothing

type SeedType = (Tree.Tree (GroupedProductionTagged [SymbolTag]), [GroupedProductionTagged [SymbolTag]])

findLoopsInTree :: GrammarGraph [SymbolTag] -> Tree.Tree (GroupedProductionTagged [SymbolTag]) -> Tree.Tree (GroupedProductionTagged [SymbolTag], [Var])
findLoopsInTree graph tree =
	Tree.unfoldTree unfoldF (tree, [])
		:: Tree.Tree (GroupedProductionTagged [SymbolTag], [Var])
	where
		unfoldF :: SeedType -> ((GroupedProductionTagged [SymbolTag], [Var]), [SeedType])
		unfoldF (tree, path) = (newNode, newSeeds) 
			where
				newNode :: (GroupedProductionTagged [SymbolTag], [Var])
				newNode =
					(label, allBackEdges)
					where
						allBackEdges :: [Var]
						allBackEdges =
							filter cond $ map prod_left (label:path)
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

groupedGrammarSub :: [Var] -> GrammarGraph symbolTag -> Maybe (GroupedGrammarTagged symbolTag)
groupedGrammarSub vars graph =
	liftM (
		Grammar .
		join .
		map (Tree.flatten . fmap snd)
	) $
	spanningForest (map Right vars) graph

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

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
