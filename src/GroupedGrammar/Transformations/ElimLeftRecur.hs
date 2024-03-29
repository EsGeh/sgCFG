{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GroupedGrammar.Transformations.ElimLeftRecur where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Transformations.Types
import GroupedGrammar.Conversions
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Utils
import Grammar.Types
import Utils

import Data.List( find, partition )


elimLeftRecur_full ::
	Monad m =>
	(Var -> Bool)
	-> VarScheme
	-> GrammarGen (ProductionGen Var [[Tagged [SymbolTag] Symbol]])
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimLeftRecur_full =
	elimFull $
	elimImmediateLeftRecursion

elimLeftRecurNoEpsilon_full :: Monad m =>
	(Var -> Bool)
	-> VarScheme
	-> GrammarGen (ProductionGen Var [[Tagged [SymbolTag] Symbol]])
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimLeftRecurNoEpsilon_full =
	elimFull $
	elimImmediateLeftRecursion_noEpsilon

elimFull :: Monad m =>
	(GroupedProduction -> VarNameMonadT m [GroupedProduction])
	-> (Var -> Bool)
	-> VarScheme
	-> GrammarGen (ProductionGen Var [[Tagged [SymbolTag] Symbol]])
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimFull immediate varCond varScheme grammar _ _ =
	runVarNameMonadT
		varScheme
		(grammar_mapToProductions (map groupedProd_removeSymbolTags) grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
		processAllOnceM $
		\(processed,remaining) current ->
			(
				fmap (\p -> toGroupedProductions $ maybeUnfold varCond (processed++remaining) =<< productionsFromGroupedProd =<< p)
				. immediate
			) $
			elimIndirectLeftRecursion processed current

elimLeftRecur :: Monad m =>
	VarScheme
	-> GrammarGen (ProductionGen Var [[Tagged [SymbolTag] Symbol]])
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimLeftRecur varScheme grammar _ _=
	runVarNameMonadT
		varScheme
		(grammar_mapToProductions (map groupedProd_removeSymbolTags) grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
		processAllOnceM $
			elimLeftRecurStep $ elimImmediateLeftRecursion

elimLeftRecurNoEpsilon :: Monad m =>
	VarScheme
	-> GrammarGen (ProductionGen Var [[Tagged [SymbolTag] Symbol]])
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimLeftRecurNoEpsilon varScheme grammar _ _=
	runVarNameMonadT
		varScheme
		(grammar_mapToProductions (map groupedProd_removeSymbolTags) grammar) $
	flip applyAlgorithmUsingProductionsM grammar $
		processAllOnceM $
			elimLeftRecurStep $ elimImmediateLeftRecursion_noEpsilon

elimLeftRecurStep ::
	(GroupedProduction -> VarNameMonadT m [GroupedProduction])
	-> ProcessedAndRemaining GroupedProduction
	-> GroupedProduction -> VarNameMonadT m [GroupedProduction]
elimLeftRecurStep immediateStep (processed,_) currentProd =
	immediateStep $
	elimIndirectLeftRecursion processed currentProd

elimIndirectLeftRecursion ::
	[GroupedProduction] -> GroupedProduction -> GroupedProduction
elimIndirectLeftRecursion processed currentProd =
	let
		left = prod_left currentProd :: Var
		rightSides = prod_right currentProd :: [[Symbol]]
	in
		Production left $
			join $
			map insertRules rightSides
		where
			insertRules :: [Symbol] -> [[Symbol]]
			insertRules right =
				case right of
					(Right var : rightRest) ->
						let mSelectProcessedRule =
							find ((==var) . prod_left) $ processed
						in
							case mSelectProcessedRule of
								Just selectedRule ->
									map (++rightRest) $ prod_right selectedRule
								Nothing ->
									[right]
					_ ->
						[right]

elimImmediateLeftRecursion ::
	Monad m =>
	GroupedProduction -> VarNameMonadT m [GroupedProduction]
elimImmediateLeftRecursion =
	elimImmediateLeftRecursionImpl $
		splitProduction

elimImmediateLeftRecursion_noEpsilon ::
	Monad m =>
	GroupedProduction -> VarNameMonadT m [GroupedProduction]
elimImmediateLeftRecursion_noEpsilon =
	elimImmediateLeftRecursionImpl $
		splitProductionNoEpsilon

elimImmediateLeftRecursionImpl ::
	Monad m =>
	(Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction])
	-> GroupedProduction -> VarNameMonadT m [GroupedProduction]
elimImmediateLeftRecursionImpl splitProduction' prod =
	let
		left = prod_left prod
		rightSides = prod_right prod :: [[Symbol]]
	in
			case partition isLeftRecursive rightSides of
				([],_) ->
					return $ [prod]
				partition' -> -- (recursiveProductions, otherProductions)
					fmap
						(
							uncurry (splitProduction' left) $
							mapFst (map $ drop 1) $
							partition'
						) $
					getSimilarVar left
		where
			isLeftRecursive rightProductionSide =
				case rightProductionSide of
					(Right var : _) | var == prod_left prod -> True
					_ -> False

splitProduction :: Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction]
splitProduction left recursiveProductionRests otherProductions newVarName =
	[ Production left $
		(++) <$> otherProductions <*> [[Right newVarName]]
	, Production newVarName $
		((++) <$> recursiveProductionRests <*> [[Right newVarName]])
		++
		[[Left epsilon]]
	]

{-
	A ->
		A a1 | ... | A an
		| b1 | ... | bk
	=>
	A ->
		b1 | ... | bk
		| b1 Z | ... | bk Z
	Z ->
		a1 | ... | an
		| a1 Z | ... | an Z
-}
splitProductionNoEpsilon :: Var -> [[Symbol]] -> [[Symbol]] -> Var -> [GroupedProduction]
splitProductionNoEpsilon left recursiveProductionRests otherProductions newVarName =
	[ Production left $
		otherProductions
		++
		((++) <$> otherProductions <*> [[Right newVarName]])
	, Production newVarName $
		((++) <$> recursiveProductionRests <*> [[], [Right newVarName]])
	]
