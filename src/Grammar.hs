{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar(
	Grammar(), fromGrammar,
	Production(), production_left, production_right,
	Symbol,
	Terminal(..), Var(..),
	GrammarFormat(..),
	grammarFromTokens,
	grammarFromStr,
) where

import Grammar.Internals
import Types
import GrammarFormat
import GroupedGrammar
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Utils (mapLeft)

import Text.Parsec hiding (many, (<|>))
import Control.Monad
import Control.Applicative


instance FromTextAs GrammarFormat Grammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ grammarFromStr parseFormat str

grammarFromStr descr =
	liftM grammarFromGroupedGrammar . groupedGrammarFromStr descr

grammarFromTokens =
	liftM grammarFromGroupedGrammar . groupedGrammarFromTokens

grammarFromGroupedGrammar ast =
	Grammar $
	concat $
	map productionsFromGroupedProd $
	fromGroupedGrammar ast

productionsFromGroupedProd :: GroupedProduction -> [Production]
productionsFromGroupedProd x = Production <$> [groupedProd_left x] <*> groupedProd_right x
