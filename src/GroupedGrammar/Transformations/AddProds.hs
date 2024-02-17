module GroupedGrammar.Transformations.AddProds where

import GroupedGrammar.Types

import GroupedGrammar.Transformations.Types
import GroupedGrammar.Transformations.Utils


insertProds :: Monad m =>
	InsertProductionsParams
	-> GroupedGrammarTagged [SymbolTag]
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
insertProds params grammar _ _ =
	return $
	flip applyAlgorithmUsingProductions grammar $
	case insertProdsParams_position params of
		GrammarPosBeginning -> \x -> (x++)
		GrammarPosEnding -> \x -> (++x)
	$
	insertProdsParams_productions params
