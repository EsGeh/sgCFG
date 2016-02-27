module GroupedGrammar.Transformations.AddProds where

import GroupedGrammar.Transformations.Types
import GroupedGrammar.Transformations.Utils

insertProds params grammar _ _ =
	return $
	flip applyAlgorithmUsingProductions grammar $
	case insertProdsParams_position params of
		GrammarPosBeginning -> \x -> (x++)
		GrammarPosEnding -> \x -> (++x)
	$
	insertProdsParams_productions params
