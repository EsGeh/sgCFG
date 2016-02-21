{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
module GroupedGrammar(
	module Export,
	--groupedGrammarSub,
	--spanningForest,
	groupedGrammarFromTokens,
	groupedGrammarFromStr,
	toTextAsTree
) where

import GroupedGrammar.Transformations as Export
import GroupedGrammar.Types as Export
import GroupedGrammar.Conversions as Export
import GroupedGrammar.Parse
import Grammar.Types
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
import qualified Data.Maybe as Maybe

toTextAsTree :: GrammarFormat -> GroupedGrammar_ProdAndSymbolsTagged ProductionTag [SymbolTag] -> String
toTextAsTree format g =
	let
		graph =
			graphFromGroupedGrammar (prod_left . value) (Either.rights . map value . join . prod_right. value) $ g
			:: Graph.Graph Var (GroupedProduction_ProdAndSymbolsTagged ProductionTag [SymbolTag])
	in
		maybe "" Tree.drawTree $
		do
			startSym <-
				fmap (prod_left . value) $
				Maybe.listToMaybe $
				fromGrammar g
				:: Maybe Var
			tree <-
				fmap (fmap snd) $
				Maybe.listToMaybe
				=<< Graph.spanningForest [startSym] graph
				:: Maybe (Tree.Tree (GroupedProduction_ProdAndSymbolsTagged ProductionTag [SymbolTag]))
			return $ fmap (toTextAs format) tree

groupedGrammarFromStr descr =
	groupedGrammarFromTokens
	<=<
	tokensFromStr descr

groupedGrammarFromTokens =
	fmap groupedGrammarRebundle
	.
	mapLeft show . P.parse parseGroupedGrammar ""

instance FromTextAs GrammarFormat GroupedGrammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ groupedGrammarFromStr parseFormat str
