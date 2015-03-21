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

import Data.List hiding (unlines)
import Data.Traversable
import Data.Functor
import qualified Data.Foldable as Fold


data GrammarGen prod =
	Grammar {
		fromGrammar :: [prod]
	}
	deriving (Show, Functor, Fold.Foldable, Traversable)

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

-- |serialize to Nothing or a String
class ToTextMaybe a where
	toTextMaybe :: a -> Maybe String

---------------------------------------------------
-- instances
---------------------------------------------------

-- Pretty
instance Pretty Var where
	pretty v = var_name v

instance Pretty Terminal where
	pretty x = "\"" ++ terminal_name x ++ "\""

instance Pretty Symbol where
	pretty x =
		either pretty pretty x

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

-- ToTextMaybe
instance ToTextMaybe a => ToTextMaybe (Maybe a) where
	toTextMaybe (Just x) = toTextMaybe x
	toTextMaybe Nothing = Nothing

instance ToTextMaybe String where
	toTextMaybe x = Just x

instance ToTextMaybe Var where
	toTextMaybe = Just . pretty

instance ToTextMaybe Terminal where
	toTextMaybe = Just . pretty

instance ToTextMaybe Symbol where
	toTextMaybe = Just . pretty

-- Tagged things:
instance
	(ToTextAs GrammarFormat symbol, ToTextMaybe tag) =>
	ToTextAs GrammarFormat (Tagged tag symbol)
	where
		toTextAs format x =
			let
				valueStr = toTextAs format (value x)
			in
				flip (maybe valueStr) (toTextMaybe $ tag x) $ \tagStr ->
				let
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
