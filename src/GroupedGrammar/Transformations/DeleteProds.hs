module GroupedGrammar.Transformations.DeleteProds where

import Grammar.Types
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types

import Utils
import GroupedGrammar.Transformations.Utils


deleteProds :: Monad m =>
	(Var -> Bool)
	-> GroupedGrammarTagged [SymbolTag]
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
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

deleteProd :: ([a], b, [a]) -> [a]
deleteProd (preceding, _, remaining) =
	preceding ++ remaining
