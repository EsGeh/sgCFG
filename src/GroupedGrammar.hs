{-# LANGUAGE MultiParamTypeClasses #-}
module GroupedGrammar(
	module GroupedGrammar.Internals,
	groupedGrammarFromTokens,
	groupedGrammarFromStr
) where

import GroupedGrammar.Internals
import GroupedGrammar.Parse
import GrammarFormat
import Parse.ParseFormatFromGrammarFormat (parseFormatFromGrammarFormat)
import Parse.Token
import Types
import Utils (mapLeft)

import qualified Text.Parsec as P
import Control.Monad

groupedGrammarFromStr descr =
	groupedGrammarFromTokens
	<=<
	tokensFromStr descr

groupedGrammarFromTokens =
	mapLeft show . P.parse parseGroupedGrammar ""


instance FromTextAs GrammarFormat GroupedGrammar where
	fromTextAs grammarFormat str =
		let parseFormat = parseFormatFromGrammarFormat grammarFormat
		in
			mapLeft show $ groupedGrammarFromStr parseFormat str
