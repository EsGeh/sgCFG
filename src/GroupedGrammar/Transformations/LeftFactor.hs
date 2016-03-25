{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types
import Utils
import Types

import Control.Monad.Writer
import Data.List --hiding( unlines )
import Data.Maybe
-- import Prelude hiding( unlines )

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

leftFactor_full varCond varScheme grammar _ _ =
	runVarNameMonadT varScheme (grammar_mapToProductions (map $ groupedProd_removeSymbolTags) $ grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
	processAllOnceM $
	\_ prod ->
		((lift $ doLog $ unlines $ ["leftFactorFull starting with", pretty prod]) >>) $
		execWriterT $
		untilM null (stepFull stopCondition step) =<<
		(fmap (\x -> [Node x Nothing]) $ lift $ step prod)
	where
		stopCondition node =
			length (pathFromNode node) > 2
		{-
		stopCondition (_, oldBlock) (_, newBlock) =
			do
				let
					emergencyCriterium =
						let
							lengthSum = 
								length . join . join . map prod_right
						in
							lengthSum oldBlock <= lengthSum newBlock
				when emergencyCriterium $ lift $ doLog $ "stopped because of emergency criterium"
				return $
					newBlock == []
					||
					emergencyCriterium
		-}
		allProds =
			fromGrammar $ fromTaggedGrammar $ grammar
		step :: 
			MonadLog m => 
			GroupedProduction
			-> VarNameMonadT m (GroupedProduction, [GroupedProduction])
		step =
			leftFactorAndUnfold varCond allProds

data Node prod = Node {
	node_data :: (prod, [prod]),
	node_parent:: Maybe (Node prod)
}

pathFromNode n =
	node_data n :
	(maybe [] pathFromNode (node_parent n))

instance MonadLog m => MonadLog (VarNameMonadT m) where
	doLog = lift . doLog

stepFull ::
	forall m prod .
	(MonadLog m, Pretty prod) =>
	(Node prod -> Bool)
	-> (prod -> m (prod, [prod]))
	-> [Node prod]
	-> WriterT [prod] m [Node prod]
stepFull stopCond f nodes' =
	let
		(nodes, abandonedNodes) =
			partition (not . stopCond) $ nodes'
	in
		do
			lift $ doLog $ concat ["leftFactoring step, ", show (length nodes'), "nodes"]
			{-
			lift $ doLog $
				unlines $
				["leftFactoring step, ", show (length nodes'), "nodes:"]
				++
				map (showState . node_data) nodes'
			-}
			unless (null abandonedNodes) $
				lift $ doLog $ unlines $ map showStopNode $ abandonedNodes
			ret <- fmap join $
				flip mapM nodes $
					\node ->
						do
							lift $ doLog $ "n th subNode"
							tell $ [fst $ node_data node]
							(lift . nextNodes f) node
			return $ ret
	where
		showStopNode node =
			unlines $
			[ "stopped full left factoring after the following steps:"
			, showNode node
			]
		showNode n =
			unlines $
			map (("step: -------------------------\n"++) . showState) $
			pathFromNode n
		showState (prod, continuations) =
			unlines $
			[ "\t" ++ pretty prod
			, "continuations:"
			]
			++
			(map ("\t"++) $ map pretty continuations)

nextNodes ::
	forall m prod .
	Monad m =>
	(prod -> m (prod, [prod]))
	-> Node prod
	-> m [Node prod]
nextNodes f node =
	let
		(_, conts) = node_data node
	in
		fmap (map $ \d -> Node d (Just node)) $
		mapM f conts

leftFactorAndUnfold ::
	MonadLog m => 
	(Var -> Bool)
	-> [GroupedProduction]
	-> GroupedProduction
	-> VarNameMonadT m (GroupedProduction, [GroupedProduction])
leftFactorAndUnfold cond allOtherProds input =
		--((lift $ doLog $ unlines $ ["leftFactorAndUnfold"] ++ [pretty input]) >>) $
		fmap (mapSnd $ maybeUnfold') $
		leftFactorProd $
		input
	where
		maybeUnfold' :: [GroupedProduction] -> [GroupedProduction]
		maybeUnfold' prods =
			join $
			flip mapM prods $
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
	MonadLog m =>
	GroupedProduction
	-> VarNameMonadT m (GroupedProduction, [GroupedProduction]) -- new production and a list of "continuations"
leftFactorProd prod =
	fmap (temp $ prod_left prod) $
	mapM (
		calcNewProd 
		.
		(mapSnd $ map $ \x -> if null x then [Left epsilon] else x)
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
