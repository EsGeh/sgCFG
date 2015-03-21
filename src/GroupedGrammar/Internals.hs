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
import Utils (unlines)

import Prelude hiding(unlines)
import Data.List hiding (unlines)
import qualified Data.List as List
import Control.Applicative
import Control.Monad.Identity
import Data.Traversable
import Data.Functor
import qualified Data.Foldable as Fold


type GroupedGrammar = GrammarGen GroupedProduction
type GroupedProduction = GroupedProductionGen Var Symbol

data GroupedProductionGen left symbol
	= GroupedProduction {
		groupedProd_left :: left,
		groupedProd_right :: [[symbol]]
	}
	deriving (Eq, Ord, Show, Functor, Fold.Foldable, Traversable)
groupedProdMapToLeftM f p = do
	new <- f (groupedProd_left p)
	return $ p{ groupedProd_left = new }
groupedProdMapToRightM f p = do
	new <- f (groupedProd_right p)
	return $ p{ groupedProd_right = new }
groupedProdMapToLeft f = runIdentity . groupedProdMapToLeftM (return . f)
groupedProdMapToRight f = runIdentity . groupedProdMapToRightM (return . f)

instance
	(ToTextAs GrammarFormat left, ToTextAs GrammarFormat symbol) =>
		ToTextAs GrammarFormat (GrammarGen (GroupedProductionGen left symbol)) where
	toTextAs format groupedGrammar =
		unlines $
		map (toTextAs format) $
		fromGrammar groupedGrammar

instance
	(ToTextAs GrammarFormat left, ToTextAs GrammarFormat symbol) =>
	ToTextAs GrammarFormat (GroupedProductionGen left symbol) where
	toTextAs format p =
		concat $
		[ toTextAs format $ groupedProd_left p
		, " "
		, head $ grammarFormat_arrow format
		, " "
		, showRightProdSide $ groupedProd_right p
		]
		where
			showRightProdSide x =
				intercalate (head $ grammarFormat_or format) $
				map (unwords . map (toTextAs format)) $
				x

{-
instance ToText GroupedGrammar where
	toText groupedGrammar =
		unlines $
		map (uncurry showEntry . (\x -> (groupedProd_left x, groupedProd_right x))) $ fromGroupedGrammar groupedGrammar
		where
			showEntry var rightRuleSides =
				concat $
				[ pretty var
				, " -> "
				, intercalate " | " $ map (unwords . map pretty) rightRuleSides
				]
-}
