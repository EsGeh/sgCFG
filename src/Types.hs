{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where


{- |to single line string -}
class Pretty a where
	pretty :: a -> String

{- |to multiline string -}
class ToText a where
	toText :: a -> String

class PrettyAs cfg a where
	prettyAs :: cfg -> a -> String

class ToTextAs cfg a where
	toTextAs :: cfg -> a -> String

class FromTextAs cfg a where
	fromTextAs :: cfg -> String -> Either ParseError a

class FromPretty a where
	fromPretty :: String -> Either ParseError a

type ParseError = String

instance ToText String where
	toText = id
