module GroupedGrammar.Transformations.ElimEpsilon where

import Grammar.Types
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types

import Utils
import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Conversions


elimEpsilon :: Monad m =>
	GroupedGrammarTagged [SymbolTag]
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimEpsilon grammar _ _ =
	let
		cond p =
			[Left epsilon] == prod_right p
	in
		return $
		flip applyAlgorithmUsingProductions grammar $
		asWithNormalProductions $
		repeatTillNotChanging $
		\prods ->
			maybe prods step $
			selectProd cond prods

step :: Zipper Production -> [Production]
step (preceding,prod,rest) =
	(replaceAll prod $ preceding) ++ (replaceAll prod $ rest)

replaceAll :: Production -> [Production] -> [Production]
replaceAll prod prods =
	let
		var = prod_left prod
	in
		join $
		flip map prods $
		prod_mapToRightM $
		\right ->
			appendIfChanged right $
			case replaceAllByCond (== Right var) [] right of
				[] -> [Left epsilon]
				other -> other

appendIfChanged ::
	Eq b => [Either Terminal b] -> [Either Terminal b]
	-> [[Either Terminal b]]
appendIfChanged old new =
	if new == old
	then [old]
	else
		if new == [Left epsilon]
		then
			[new]
		else
			[old, new]
