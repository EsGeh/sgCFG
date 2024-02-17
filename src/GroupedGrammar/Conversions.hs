{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Conversions where

import GroupedGrammar.Types
import Grammar.Types

import Control.Monad
import Control.Monad.Identity


{- |
	this correctly groups productions which have equal left hand sides
-}
groupedGrammarRebundle :: GroupedGrammar -> GroupedGrammar
groupedGrammarRebundle =
		grammar_mapToProductions $
		toGroupedProductions .
		concatMap productionsFromGroupedProd

groupedGrammarNullProdsToEpsilonProds ::
	GrammarGen (ProductionGen left [[Either Terminal b]])
	-> GrammarGen (ProductionGen left [[Either Terminal b]])
groupedGrammarNullProdsToEpsilonProds =
	grammar_mapToProductions $
	map $ prod_mapToRight $
	\right ->
		case right of
			[] -> [[Left epsilon]]
			[[]] -> [[Left epsilon]]
			_ -> right


-- conversion: grouped <-> normal

grammarFromGroupedGrammar ::
	GrammarGen GroupedProduction -> GrammarGen Production
grammarFromGroupedGrammar ast =
	Grammar $
	concatMap productionsFromGroupedProd $
	fromGrammar ast

productionsFromGroupedProd :: GroupedProduction -> [Production]
productionsFromGroupedProd x = Production <$> [prod_left x] <*> prod_right x

asWithNormalProductions ::
	([Production] -> [Production])
	-> [GroupedProduction] -> [GroupedProduction]
asWithNormalProductions f =
	runIdentity
	.
	asWithNormalProductionsM (return . f)

asWithNormalProductionsM ::
	Monad m =>
	([Production] -> m [Production])
	-> [GroupedProduction] -> m [GroupedProduction]
asWithNormalProductionsM f =
	(return . toGroupedProductions)
	<=<
	(f . join . map productionsFromGroupedProd)

toGroupedProductions :: [Production] -> [GroupedProduction]
toGroupedProductions =
	map joinProductions
	.
	partitionBy prod_left
	where
		joinProductions prods =
			case prods of
				[] -> error "toGroupedProductions error!"
				(x:_) ->
					Production (prod_left x) $ map prod_right prods

partitionBy :: forall a b . (Ord b) => (a -> b) -> [a] -> [[a]]
partitionBy f l =
	fst $
	partitionBy' [] l
	where
		partitionBy' :: [[a]] -> [a] -> ([[a]], [a])
		partitionBy' processed unprocessed =
			case unprocessed of
				[] -> (processed, [])
				(x:xs) ->
					let
						(notEqual, rest) = span ((/=f x). f . head) processed
							:: ([[a]],[[a]])
					in
						case rest of
							[] -> partitionBy' (processed ++ [[x]]) xs
							(y:ys) ->
								partitionBy'
									(notEqual ++ [y ++ [x]] ++ ys) xs
