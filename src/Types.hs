{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
fromMonadicLens lens f =
	runIdentity . lens (return . f)
