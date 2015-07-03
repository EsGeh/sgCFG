{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module GroupedGrammar.Internals where

import GrammarTypes
import Types
import GrammarFormat
--import Utils (unlines)

import Prelude hiding(unlines)
import Data.List hiding (unlines)
--import qualified Data.List as List
--import Control.Applicative
--import Control.Monad.Identity
--import Data.Traversable
--import Data.Functor
--import qualified Data.Foldable as Fold


type GroupedGrammar = GrammarGen GroupedProduction
type GroupedProduction = GroupedProductionGen Var Symbol
type GroupedProductionGen var symbol = ProductionGen var [[symbol]]
	--GrammarGen (Tagged [String] (ProductionGen Var [[TaggedSymbol]]))

instance
	(Pretty left, Pretty symbol) =>
	Pretty (ProductionGen left [[symbol]]) where
		pretty p =
			concat $
			[ pretty $ prod_left p
			, " -> "
			, intercalate " | " $
				map (unwords . map pretty) $
				prod_right p
			]

{-
instance
	(ToTextAs GrammarFormat left, ToTextAs GrammarFormat symbol) =>
	ToTextAs GrammarFormat (ProductionGen left [[symbol]]) where
		toTextAs format p =
			concat $
			[ toTextAs format $ prod_left p
			, " "
			, head $ grammarFormat_arrow format
			, " "
			, showRightProdSide $ prod_right p
			]
			where
				showRightProdSide x =
					intercalate (head $ grammarFormat_or format) $
					map (unwords . map (toTextAs format)) $
					x
-}

instance
	( ToTextAs GrammarFormat left
	, ToTextAs GrammarFormat symbol
	, ToTextAs GrammarFormat tag
	) =>
	ToTextAs GrammarFormat (ProductionGen left [[Tagged [tag] symbol]]) where
		toTextAs format p =
			concat $
			[ applySymbolFormat (grammarFormat_leftSide format) $ toTextAs format $ prod_left p
			, " "
			, head $ grammarFormat_arrow format
			, " "
			, applySymbolFormat (grammarFormat_rightSide format) $ showRightProdSide $ prod_right p
			]
			where
				showRightProdSide x =
					intercalate (head $ grammarFormat_or format) $
					map (unwords . map (toTextTaggedSymbol format)) $
					x

toTextTaggedSymbol ::
	(ToTextAs GrammarFormat sym, ToTextAs GrammarFormat tag) =>
	GrammarFormat -> Tagged [tag] sym
	-> String
toTextTaggedSymbol format x =
	let
		valueStr = toTextAs format (value x)
	in
		if length (tag x) == 0 then valueStr else 
			let
				tagStr = intercalate ", " $ map (toTextAs format) $ tag x
				annFormat = grammarFormat_taggedSymbol format
				tagStrSurrounded =
					applySymbolFormat (Just $ annotationFormat_surroundBy annFormat) $
					tagStr
				intersectStr = annotationFormat_intersectBy annFormat
			in
				intercalate intersectStr $
					if annotationFormat_prepend annFormat
						then [tagStrSurrounded, valueStr]
						else [valueStr, tagStrSurrounded]
{-
instance ToText GroupedGrammar where
	toText groupedGrammar =
		unlines $
		map (uncurry showEntry . (\x -> (prod_left x, prod_right x))) $ fromGroupedGrammar groupedGrammar
		where
			showEntry var rightRuleSides =
				concat $
				[ pretty var
				, " -> "
				, intercalate " | " $ map (unwords . map pretty) rightRuleSides
				]
-}
