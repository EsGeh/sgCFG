{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module GrammarTypes where

import Types
import GrammarFormat

--import Data.List hiding (unlines)
--import Data.Traversable
--import Data.Functor
import qualified Data.Foldable as Fold
import Control.Monad.Identity


data GrammarGen prod =
	Grammar {
		fromGrammar :: [prod]
	}
	deriving (Show, Functor, Fold.Foldable, Traversable)

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
prod_mapToLeft f = runIdentity . prod_mapToLeftM (return . f)
prod_mapToRight f = runIdentity . prod_mapToRightM (return . f)

type Symbol = Either Terminal Var

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
tagged = Tagged
tagged_mapToTagM f x = do
	new <- f $ tag x
	return $ x{ tag = new }
tagged_mapToValueM f x = do
	new <- f $ value x
	return $ x{ value = new }
tagged_mapToTag f = runIdentity . tagged_mapToTagM (return . f)
tagged_mapToValue f = runIdentity . tagged_mapToValueM (return . f)

{-
-- |serialize to Nothing or a String
class ToTextMaybe a where
	toTextMaybe :: a -> Maybe String
-}

---------------------------------------------------
-- instances
---------------------------------------------------

{-
instance
	(Pretty left, Pretty right) =>
	Pretty (ProductionGen left right) where
		pretty p =
			concat $
			[ pretty $ prod_left p
			, " -> "
			, pretty $ prod_right p
			]
-}

-- Pretty
instance Pretty Var where
	pretty v = var_name v

instance Pretty Terminal where
	pretty x = "\"" ++ terminal_name x ++ "\""

instance Pretty Symbol where
	pretty x =
		either pretty pretty x

-- PrettyAs
-- TODO:
{-
instance PrettyAs GrammarFormat Var where
	prettyAs format x
-}

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

instance
	(ToTextAs GrammarFormat prod) =>
		ToTextAs GrammarFormat (GrammarGen prod) where
	toTextAs format groupedGrammar =
		unlines $
		map (toTextAs format) $
		fromGrammar groupedGrammar
