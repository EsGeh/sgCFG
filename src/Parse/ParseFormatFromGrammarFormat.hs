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
			symbol grammarFormat,
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

orderByPrefix strings = sortBy cmp strings
	where
		cmp [] [] = EQ
		cmp [] (_:_) = GT
		cmp (_:_) [] = LT
		cmp (x:xs) (y:ys) =
			case compare x y of
				EQ -> cmp xs ys
				other -> other

{-
orderByPrefix =
	sortBy $ \a b ->
		if commonPrefix a b == a
			then GT
			else
				if commonPrefix a b == b
					then LT
					else GT
-}

commonPrefix a b =
	case (a,b) of
		(x:xs, y:ys) | x == y -> x:(commonPrefix xs ys)
		_ -> ""

parseLineComment f =
	(choice $ map (P.try . P.string) $ grammarFormat_lineComment f)
	>>
	(noneOf ['\n']) `manyTill` (char '\n')

symbol :: GrammarFormat -> Parsec String () ()
	-> Parsec String () (Either String String)
symbol f stop =
	case (grammarFormat_var f, grammarFormat_terminal f) of
		(Just surroundVar, mSurroundTerm) ->
			(try $ liftM Right $ parseSurround surroundVar)
			<|>
			(liftM Left $
				maybe (parseUntilStop $ (try $ parseSurround surroundVar >> return ()) <|> stop)
					parseSurround
					mSurroundTerm
			)
		(Nothing, Just surroundTerm) ->
			(try $ liftM Left $ parseSurround surroundTerm)
			<|>
			(liftM Right $ parseUntilStop stop)
		--(Nothing, Nothing) ->

parseSurround surround =
	let (prefix, suffix) = fromSurroundBy surround
	in
		P.string prefix *> (P.anyChar `P.manyTill` (P.lookAhead $ P.try $ P.string suffix)) <* P.string suffix

parseUntilStop stop =
	anyChar `manyTill` (lookAhead stop)
