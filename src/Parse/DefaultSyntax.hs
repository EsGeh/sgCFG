module Parse.DefaultSyntax where

import Parse.ParseToken

import Text.Parsec hiding (many, (<|>), optional)
import Control.Monad
import Control.Applicative

syntaxDescr =
	GrammarDescr {
		grammarDescr_symbol = symbol,
		--grammarDescr_terminal = many anyChar,
		grammarDescr_or = [optional (string "\n") *> string "|"],
		grammarDescr_prodSign = [string "->"],
		grammarDescr_whitespaces = [try (string " "), lineComment],
		grammarDescr_prodSep = [string "\n"]
	}

symbol stop =
	(liftM (Left) $ parseTerminal stop)
	<|>
	(liftM (Right) $ parseVar stop)

parseVar stop = do
	anyChar `manyTill` stop

parseTerminal stop = do
	string "\""
	anyChar `manyTill` (try (string "\"" >> return ()) <|> stop)

lineComment = do
	x <- string "%"
	y <- anyChar `manyTill` (string "\n")
	return $ ""
