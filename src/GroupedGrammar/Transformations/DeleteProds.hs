module GroupedGrammar.Transformations.DeleteProds where

import GroupedGrammar.Transformations.Utils
import Grammar.Types

import Utils

deleteProds varCond =
	let
		cond = varCond . prod_left
	in
		applyAlgorithmUsingProductions $
		repeatTillNotChanging $
		\prods ->
			maybe prods deleteProd $
			selectProd cond $
			prods

deleteProd (preceding, _, remaining) =
	preceding ++ remaining
