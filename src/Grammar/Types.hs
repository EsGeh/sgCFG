{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Grammar.Types where

import Types
import GrammarFormat

import qualified Data.Foldable as Fold


---------------------------------------------------
-- very general types:

data GrammarGen prod =
	Grammar {
		fromGrammar :: [prod]
	}
	deriving (Eq, Ord, Show, Functor, Fold.Foldable, Traversable)

grammar_mapToProductionsM f g =
	do
		new <- f $ fromGrammar g
		return $ Grammar $ new
grammar_mapToProductions =
	fromMonadicLens grammar_mapToProductionsM

data ProductionGen left right
	= Production {
		prod_left :: left,
		prod_right :: right
	}
	deriving (Eq, Ord, Show, Functor, Fold.Foldable, Traversable)
prod_mapToLeftM f p = do
	new <- f (prod_left p)
	return $ p{ prod_left = new }
prod_mapToRightM f p = do
	new <- f (prod_right p)
	return $ p{ prod_right = new }
prod_mapToLeft =
	fromMonadicLens prod_mapToLeftM
prod_mapToRight =
	fromMonadicLens prod_mapToRightM

---------------------------------------------------
-- more concrete types:

type Grammar = GrammarGen Production
type Production = ProductionGen Var [Symbol]

type Symbol = Either Terminal Var

epsilon = Terminal ""

newtype Terminal = Terminal {
	terminal_name :: String
}
	deriving (Eq, Ord, Show)

newtype Var = Var {
	var_name :: String
}
	deriving (Eq, Ord, Show)

data Tagged tag a =
	Tagged {
		tag :: tag,
		value :: a
	}
	deriving( Eq, Ord, Show)
tagged = Tagged
tagged_mapToTagM f x = do
	new <- f $ tag x
	return $ x{ tag = new }
tagged_mapToValueM f x = do
	new <- f $ value x
	return $ x{ value = new }
tagged_mapToTag =
	fromMonadicLens tagged_mapToTagM
tagged_mapToValue =
	fromMonadicLens tagged_mapToValueM

---------------------------------------------------
-- instances
---------------------------------------------------

-- ToTextAs
instance ToTextAs GrammarFormat Var where
	toTextAs format x =
		applySymbolFormat (grammarFormat_var format) $ var_name x

instance ToTextAs GrammarFormat Terminal where
	toTextAs format x =
		applySymbolFormat (grammarFormat_terminal format) $ terminal_name x

instance ToTextAs GrammarFormat Symbol where
	toTextAs format =
			either (toTextAs format) (toTextAs format)

instance ToTextAs GrammarFormat Production where
	toTextAs format p =
		concat $
		[ toTextAs format $ prod_left p
		--, " "
		, prodSign
		--, " "
		, unwords $ map (toTextAs format) $ prod_right p
		]
		where
			prodSign = head $ grammarFormat_arrow format

-- if you can serialize a prod, you can serialize the grammar:
instance
	(ToTextAs GrammarFormat prod) =>
		ToTextAs GrammarFormat (GrammarGen prod) where
	toTextAs format groupedGrammar =
		unlines $
		map (toTextAs format) $
		fromGrammar groupedGrammar

-- ToText
instance ToText Grammar where
	toText g =
		unlines $
		map pretty $
		fromGrammar g

-- Pretty
instance Pretty Var where
	pretty v = var_name v

instance Pretty Terminal where
	pretty x = "\"" ++ terminal_name x ++ "\""

instance Pretty Symbol where
	pretty x =
		either pretty pretty x

instance Pretty Production where
	pretty p =
		(
			concat $
			[ pretty $ prod_left p
			, " -> "
			]
		)
		++
		(unwords $ map pretty $ prod_right p)
