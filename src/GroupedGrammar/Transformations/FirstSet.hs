{-# LANGUAGE ScopedTypeVariables #-}
module GroupedGrammar.Transformations.FirstSet where

import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types
import Grammar.Types
import Types
import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe


annotateWithFirstSets ::
	Eq symbolTag
	=> GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag
annotateWithFirstSets g =
	firstTimeNotChanging g $
	iterate firstSetsStep g

firstSetsStep :: forall symbolTag .
	Eq symbolTag
	=> GroupedGrammar_SeparateProdTags FirstSet symbolTag -> GroupedGrammar_SeparateProdTags FirstSet symbolTag
firstSetsStep g =
	let
		grammar = ggSeparateProdTags_grammar g :: GroupedGrammarTagged symbolTag
	in
		(ggSeparateProdTags_mapToRuleAnnotations $ updateAnn $ fromTaggedGrammar grammar) g
		where
			updateAnn :: GroupedGrammar -> M.Map Var FirstSet -> M.Map Var FirstSet
			updateAnn grammar ruleAnnotations =
				let
					rules = fromGrammar grammar
				in
					M.fromList $
					map
						(\prod ->
							let
								var = prod_left prod
								oldFirstSet = fromMaybe (error "updateAnn: error looking up old first set") $ M.lookup var ruleAnnotations
							in
								(var, calcNewFirst (flip M.lookup ruleAnnotations) prod oldFirstSet)
						)
						rules 
					where
						calcNewFirst :: (Var -> Maybe FirstSet) -> GroupedProduction -> FirstSet -> FirstSet
						calcNewFirst lookup prod oldFirstSet =
							let
								--left = prod_left prod
								right = prod_right prod
								func right' =
									case right' of
										((Left t):xs) ->
											if t /= epsilon
											then S.singleton t
											else S.singleton t `S.union` func xs
										((Right var):xs) ->
											let
												directFirst =
													fromMaybe (
														S.singleton $
														Terminal $
														pretty var
														--"FIRST(" ++ pretty var ++ ")"
													) $
													lookup var
											in
												if not $ epsilon `S.member` directFirst
												then directFirst
												else
													directFirst `S.union` func xs
										_ -> S.empty
							in
								foldl S.union oldFirstSet $
								map func right
