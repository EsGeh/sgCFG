module Parse.Token(
	module Parse.Token.Internals,
	tokensFromStr,
) where

import Parse.Token.Internals
import Parse.Token.Parse

import qualified Text.Parsec as P


tokensFromStr descr = P.parse (parseTokens descr) ""
