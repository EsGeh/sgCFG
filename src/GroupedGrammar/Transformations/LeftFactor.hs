{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module GroupedGrammar.Transformations.LeftFactor where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import Grammar.Types
import Utils
import Types

import Data.List --hiding( unlines )
import Data.Maybe
-- import Prelude hiding( unlines )

--import qualified Debug.Trace as Trace


leftFactor_full varCond varScheme grammar _ _ =
	runVarNameMonadT varScheme (grammar_mapToProductions (map $ groupedProd_removeSymbolTags) $ grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
	processAllOnceM $
	\_ x -> ((lift $ doLog $ unlines $ ["leftFactorFull starting with", pretty x]) >>) $
			fmap (uncurry (++)) $
			untilExtM_M stopCondition (leftFactorStep_full varCond allProds)
			([],[x])
	where
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
		allProds =
			fromGrammar $ fromTaggedGrammar $ grammar

leftFactorStep_full ::
	MonadLog m => 
	(Var -> Bool)
	-> [GroupedProduction]
	-> ([GroupedProduction], [GroupedProduction])
	-> VarNameMonadT m ([GroupedProduction], [GroupedProduction])
leftFactorStep_full cond allOtherProds input =
		((lift $ doLog $ unlines $ ["leftFactorStep_full"] ++ map pretty (snd input)) >>) $
		(fmap (mapSnd $ maybeUnfold') . leftFactoringStep) input
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
			mapM leftFactoringStep' newBlock
		return $
			(oldBlock ++ newProds, nextBlock)

leftFactoringStep' ::
	forall m .
	MonadLog m =>
	GroupedProduction -> VarNameMonadT m (GroupedProduction, [GroupedProduction])
leftFactoringStep' prod =
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
		temp left =
			mapSnd catMaybes
			.
			mapFst (Production left)
			.
			unzip

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
