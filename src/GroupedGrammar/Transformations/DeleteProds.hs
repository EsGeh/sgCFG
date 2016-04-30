module GroupedGrammar.Transformations.DeleteProds where

import GroupedGrammar.Transformations.Utils
import Grammar.Types

import Utils

deleteProds varCond grammar _ _ =
	return $
	let
		cond = varCond . prod_left
	in
		flip applyAlgorithmUsingProductions grammar $
		repeatTillNotChanging $
		\prods ->
			maybe prods deleteProd $
			selectProd cond $
			prods

deleteProd (preceding, _, remaining) =
	preceding ++ remaining
