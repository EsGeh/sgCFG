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
import qualified Data.Either as Either


---------------------------------------------------
-- very general types:

data GrammarGen prod =
	Grammar {
		fromGrammar :: [prod]
	}
	deriving (Eq, Ord, Show, Functor, Fold.Foldable, Traversable)

grammar_mapToProductionsM :: Monad m =>
	([prod1] -> m [prod2])
	-> GrammarGen prod1 -> m (GrammarGen prod2)
grammar_mapToProductionsM f g =
	do
		new <- f $ fromGrammar g
		return $ Grammar $ new
grammar_mapToProductions :: ([prod1] -> [prod2]) -> GrammarGen prod1 -> GrammarGen prod2
grammar_mapToProductions =
	fromMonadicLens grammar_mapToProductionsM

data ProductionGen left right
	= Production {
		prod_left :: left,
		prod_right :: right
	}
	deriving (Eq, Ord, Show, Functor, Fold.Foldable, Traversable)
prod_mapToLeftM ::
	Monad m =>
	(t -> m left)
	-> ProductionGen t right -> m (ProductionGen left right)
prod_mapToLeftM f p = do
	new <- f (prod_left p)
	return $ p{ prod_left = new }
prod_mapToRightM ::
	Monad m =>
	(t -> m right)
	-> ProductionGen left t -> m (ProductionGen left right)
prod_mapToRightM f p = do
	new <- f (prod_right p)
	return $ p{ prod_right = new }
prod_mapToLeft ::
	(a1 -> b)
	-> ProductionGen a1 right -> ProductionGen b right
prod_mapToLeft =
	fromMonadicLens prod_mapToLeftM
prod_mapToRight ::
	(a1 -> b)
	-> ProductionGen left a1 -> ProductionGen left b
prod_mapToRight =
	fromMonadicLens prod_mapToRightM

---------------------------------------------------
-- more concrete types:

type Grammar = GrammarGen Production
type Production = ProductionGen Var [Symbol]

type Symbol = Either Terminal Var

isVar, isTerminal :: Symbol -> Bool
isVar = Either.isRight
isTerminal = Either.isLeft

epsilon :: Terminal
epsilon = Terminal ""

varSymbol :: Var -> Symbol
varSymbol = Right
terminalSymbol :: Terminal -> Symbol
terminalSymbol = Left

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
tagged :: tag -> a -> Tagged tag a
tagged = Tagged
tagged_mapToTagM ::
	Monad m =>
	(a1 -> m tag) -> Tagged a1 a2 -> m (Tagged tag a2)
tagged_mapToTagM f x = do
	new <- f $ tag x
	return $ x{ tag = new }
tagged_mapToValueM :: Monad m =>
	(a1 -> m a2) -> Tagged tag a1 -> m (Tagged tag a2)
tagged_mapToValueM f x = do
	new <- f $ value x
	return $ x{ value = new }
tagged_mapToTag :: (a1 -> b) -> Tagged a1 a -> Tagged b a
tagged_mapToTag =
	fromMonadicLens tagged_mapToTagM
tagged_mapToValue :: (a1 -> b) -> Tagged tag a1 -> Tagged tag b
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
	pretty = var_name 

instance Pretty Terminal where
	pretty x = "\"" ++ terminal_name x ++ "\""

instance Pretty Symbol where
	pretty =
		either pretty pretty

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
