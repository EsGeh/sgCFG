{-# LANGUAGE FlexibleContexts #-}
module Parse.ParseFormatFromGrammarFormat where

import GrammarFormat
import Parse.Format

import Text.Parsec as P hiding(many, (<|>))
import Control.Monad
import Control.Applicative
import Data.List

parseFormatFromGrammarFormat grammarFormat =
	ParseFormat {
		parseFormat_symbol =
			parseSymbol grammarFormat,
		parseFormat_or =
			map (P.try . P.string) $
			orderByPrefix $
			grammarFormat_or grammarFormat,
		parseFormat_prodSign =
			map (P.try . P.string) $
			orderByPrefix $
			grammarFormat_arrow grammarFormat,
		parseFormat_whitespaces =
			map (P.try . P.string) (grammarFormat_whitespaces grammarFormat)
		,
		parseFormat_comment =
			[parseLineComment grammarFormat],
		parseFormat_prodSep = map (P.try . P.string) $ grammarFormat_prodSep grammarFormat
	}

{-
	if string a is a prefix of string b, then b will come before a
	ex.:
	orderByPrefix ["a", "ab", "cde"] == ["ab", "a", "cde"]
-}
orderByPrefix strings = sortBy cmp strings
	where
		cmp [] [] = EQ
		cmp [] (_:_) = GT
		cmp (_:_) [] = LT
		cmp (x:xs) (y:ys) =
			case compare x y of
				EQ -> cmp xs ys
				other -> other

parseLineComment f =
	(choice $ map (P.try . P.string) $ grammarFormat_lineComment f)
	>>
	(noneOf ['\n']) `manyTill` (char '\n')

parseSymbol :: GrammarFormat -> Parsec String () ()
	-> Parsec String () (Either String String)
parseSymbol f stop =
	case (grammarFormat_var f, grammarFormat_terminal f) of
		(Just surroundVar, mSurroundTerm) ->
			( -- try to parse a variable:
				try $ liftM Right $ parseSurround surroundVar
			)
			<|>
			( -- parse as terminal:
				liftM Left $
				maybe
					(
						-- read until a variable, or 'stop' is found:
						parseUntilStop $
						(try $ parseSurround surroundVar >> return ()) <|> stop
					)
					parseSurround
					mSurroundTerm
			)
			where
		(Nothing, Just surroundTerm) ->
			( -- try to parse as a terminal:
				try $ liftM Left $ parseSurround surroundTerm
			)
			<|>
			( -- parse as a variable:
				liftM Right $ parseUntilStop stop
			)
		(Nothing, Nothing) ->
			-- default: parse as terminal
			liftM Left $ parseUntilStop stop

parseSurround surround =
	let (prefix, suffix) = fromSurroundBy surround
	in
		P.string prefix *> (P.anyChar `P.manyTill` (P.lookAhead $ P.try $ P.string suffix)) <* P.string suffix

parseUntilStop stop =
	P.anyChar `manyTill` (P.lookAhead stop)
