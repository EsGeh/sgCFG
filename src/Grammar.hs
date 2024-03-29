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
import Parse.Token
import Parse.Format
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Utils (mapLeft)


instance FromTextAs GrammarFormat Grammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ grammarFromStr parseFormat str

grammarFromStr ::
	ParseFormat -> String
	-> Either String (GrammarGen Production)
grammarFromStr descr =
	fmap grammarFromGroupedGrammar . groupedGrammarFromStr descr

grammarFromTokens ::
	[Token]
	-> Either String (GrammarGen Production)
grammarFromTokens =
	fmap grammarFromGroupedGrammar . groupedGrammarFromTokens

