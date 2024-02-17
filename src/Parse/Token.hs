module Parse.Token(
	module Parse.Token.Internals,
	tokensFromStr,
) where

import Parse.Token.Internals
import Parse.Token.Parse
import Utils (mapLeft)
import Parse.Format( ParseFormat )

import qualified Text.Parsec as P


tokensFromStr ::
	ParseFormat
	-> String -> Either String [Token]
tokensFromStr descr =
	mapLeft show . P.parse (parseTokens descr) ""
