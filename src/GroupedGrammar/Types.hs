{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module GroupedGrammar.Types where

import Grammar.Types
import Types
import GrammarFormat
--import Utils (unlines)

import Prelude hiding(unlines)
import Data.List hiding (unlines)
import qualified Data.Set as S


---------------------------------------------------
-- type synonyms for grouped grammars and productions

type GroupedGrammar = GrammarGen GroupedProduction
type GroupedProduction = GroupedProductionGen Var Symbol
type GroupedProductionGen var symbol = ProductionGen var [[symbol]]
	--GrammarGen (Tagged [String] (ProductionGen Var [[TaggedSymbol]]))

---------------------------------------------------
-- type synonyms for "tagged" grammars and productions:

type GroupedGrammarTagged symbolTag =
	GrammarGen (GroupedProductionTagged symbolTag)
type GroupedGrammar_ProdAndSymbolsTagged productionTag symbolTag =
	GrammarGen (GroupedProduction_ProdAndSymbolsTagged productionTag symbolTag)

type GroupedProductionTagged symbolTag =
	GroupedProductionGen Var (Tagged symbolTag Symbol)
type GroupedProduction_ProdAndSymbolsTagged productionTag symbolTag =
	Tagged productionTag (GroupedProductionTagged symbolTag)

---------------------------------------------------
-- concrete types for tagged grammars and productions:

data ProductionTag = ProductionTag {
		prodTag_firstSet :: Maybe FirstSet
	}
	deriving( Eq, Ord )

prodTag_empty = ProductionTag Nothing
prodTag_mapToFirstSet f t = t{ prodTag_firstSet = f (prodTag_firstSet t) }

type FirstSet = S.Set Terminal

data SymbolTag
	= ProductionRef Var
	deriving( Eq, Ord )

---------------------------------------------------
-- instances
---------------------------------------------------

-- ToTextAs

instance ToTextAs GrammarFormat SymbolTag where
	toTextAs _ _ = "!"

instance ToTextAs GrammarFormat ProductionTag where
	toTextAs format x =
		maybe "" id $
		fmap printFirstSet (prodTag_firstSet x)
		where
			printFirstSet set =
				concat $
				[ "FIRST={"
				, intercalate ", " $ fmap (toTextAs format) $ S.toList $ set
				, "}"
				]
		{-
		maybe "" id $
		(prodTag_firstSet x) >>= \set -> return $
			concat $
				[ "FIRST={"
				, intercalate ", " $ fmap (toTextAs format) $ S.toList $ set
				, "}"
				]
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

instance
	ToTextAs GrammarFormat (GroupedProduction_ProdAndSymbolsTagged ProductionTag [SymbolTag]) where
		toTextAs format p =
			let
				tagStr = toTextAs format $ tag p
				prodStr = toTextAs format $ value p
			in
				if tagStr /= ""
				then unwords [tagStr, prodStr]
				else prodStr
			{-
			unwords $
				[ toTextAs format $ tag p
				, toTextAs format $ value p
				]
			-}

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

-- Pretty

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
