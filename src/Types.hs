{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
module Types where

import Control.Monad.Identity


-- the following are used for simple user interaction:
{- |to single line string -}
class Pretty a where
	pretty :: a -> String

class FromPretty a where
	fromPretty :: String -> Either ParseError a

{- |to multiline string -}
class ToText a where
	toText :: a -> String

-- used for grammar serialisation:
{- |(de-)serialize taking an additional config parameter -}
class FromTextAs cfg a where
	fromTextAs :: cfg -> String -> Either ParseError a

class ToTextAs cfg a where
	toTextAs :: cfg -> a -> String

type ParseError = String

instance ToText String where
	toText = id

-- helper to implement lenses by hand:
fromMonadicLens ::
	(forall m . Monad m => (a1 -> m b) -> a2 -> m c)
	-> (a1 -> b) -> a2 -> c
fromMonadicLens lens f =
	runIdentity . lens (return . f)
