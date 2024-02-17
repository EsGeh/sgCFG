module GroupedGrammar.Transformations.RemoveDoubleProds where

import GroupedGrammar.Transformations.Utils
import GroupedGrammar.Transformations.Types( GroupedGrammar_SeparateProdTags )
import GroupedGrammar.Types
import Grammar.Types
import Utils
import Types


removeDoubleProds :: MonadLog m =>
	GroupedGrammarTagged [SymbolTag]
	-> p1
	-> p2
	-> m (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
removeDoubleProds grammar _ _ =
	flip applyAlgorithmUsingProductionsM grammar $
	repeatTillNotChangingM $ -- <- this is a hack, not very efficient...
	processM step

step ::
	MonadLog m =>
	ProcessedAndRemaining GroupedProduction -> GroupedProduction
	-> m (ProcessedAndRemaining GroupedProduction)
step input@(processed, remaining) prod =
	case
		selectProd ((== prod_right prod) . prod_right) processed
	of
		Just (_,equalProd,_) ->
			((doLog $ concat $
			["remouveDoubleProds step: replacing\n\t"
			, pretty prod
			, "\nby\n\t"
			, pretty equalProd
			]
			)>>) $
			return $
			mapFst replace $
			mapSnd replace $
			input
			where
				replace =
					map $
					prod_mapToRight $
					map $ map $ \sym ->
						if sym == Right (prod_left prod)
						then
							Right (prod_left equalProd)
						else
							sym
		Nothing ->
			return $ (processed ++ [prod], remaining)

processM ::
	Monad m =>
	(ProcessedAndRemaining a -> a -> m (ProcessedAndRemaining a))
	-> [a] -> m [a]
processM f l =
	fmap fst $
	processM' ([], l)
	where
		processM' partition =
			case partition of
				(_,[]) -> return $ partition
				(processed, x:remaining) ->
					do
						newState <- f (processed,remaining) x
						processM' newState
