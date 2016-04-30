module Parse.Token(
	module Parse.Token.Internals,
	tokensFromStr,
) where

import Parse.Token.Internals
import Parse.Token.Parse
import Utils (mapLeft)

import qualified Text.Parsec as P


tokensFromStr descr =
	mapLeft show . P.parse (parseTokens descr) ""
