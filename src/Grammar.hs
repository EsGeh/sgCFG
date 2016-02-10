{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Grammar(
	Grammar(), fromGrammar,
	Production(), prod_left, prod_right,
	Symbol,
	Terminal(..), Var(..),
	GrammarFormat(..),
	grammarFromTokens,
	grammarFromStr,
) where

--import Grammar.Internals
import Grammar.Types
import Types
import GrammarFormat
import GroupedGrammar
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Utils (mapLeft)

--import Text.Parsec hiding (many, (<|>))
import Control.Monad
--import Control.Applicative


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
	fromGrammar ast

productionsFromGroupedProd :: GroupedProduction -> [Production]
productionsFromGroupedProd x = Production <$> [prod_left x] <*> prod_right x
