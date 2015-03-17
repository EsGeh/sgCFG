module Parse.BNF where

import Parse.ParseToken

import Text.Parsec hiding (many, (<|>), optional)
import Control.Monad
import Control.Applicative

bnf =
	GrammarDescr {
		grammarDescr_symbol = bnfSymbol,
		--grammarDescr_terminal = many anyChar,
		grammarDescr_or = [optional (string "\n") *> string "|"],
		grammarDescr_prodSign = [string "::="],
		grammarDescr_whitespaces = [try (string " "), lineComment],
		grammarDescr_prodSep = [string "\n"]
	}

bnfSymbol stop =
	(try $ liftM (Right) $ parseVar)
	<|>
	(liftM (Left) $ parseTerminal stop)

parseVar = string "<" *> (many $ noneOf ['>']) <* string ">"

parseTerminal stop =
	anyChar `manyTill` stop

lineComment = do
	x <- string "%"
	y <- anyChar `manyTill` (string "\n")
	return $ ""
