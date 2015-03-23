{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GroupedGrammar(
	module GroupedGrammar.Internals,
	module GroupedGrammar.Transformations,
	--groupedGrammarSub,
	--spanningForest,
	groupedGrammarFromTokens,
	groupedGrammarFromStr,
	--toTextAsTree
) where

import GroupedGrammar.Transformations
import GroupedGrammar.Internals
import GroupedGrammar.Parse
import GrammarTypes
import GrammarFormat
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Parse.Token
import Types
import Utils
import qualified Utils.Graph as Graph

import qualified Text.Parsec as P
import qualified Data.Tree as Tree
import Control.Monad
import qualified Data.Either as Either
import qualified Data.Foldable as Fold
import Control.Applicative
import qualified Data.Maybe as Maybe

{-
test =
	findLoops (Var "a") $
	graphFromGroupedGrammar $
	testGrammar

testGrammar =
	either (const $ error "") id $
	fromTextAs (defaultFormat Default) "a -> b | c\nb -> a\nc -> \"d\""
-}

{-
toTextAsTree format g =
	let
		graph =
			graphFromGroupedGrammar $ g
	in
		do
			startSym <-
				liftM prod_left $
				Maybe.listToMaybe $
				fromGrammar g
			tree <-
				Maybe.listToMaybe
				=<< Graph.spanningForest [startSym] graph
				:: Maybe (Tree.Tree GroupedProductionTagged)
			return $
				fmap
				( toTextAs format
				) $
				tree
-}

tptpSyntaxFormat =
	(defaultFormat BNF){
		grammarFormat_arrow = ["::=",":=="],
		grammarFormat_lineComment = ["%"]
	}

groupedGrammarFromStr descr =
	groupedGrammarFromTokens
	<=<
	tokensFromStr descr

groupedGrammarFromTokens =
	mapLeft show . P.parse parseGroupedGrammar ""

instance FromTextAs GrammarFormat GroupedGrammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ groupedGrammarFromStr parseFormat str
