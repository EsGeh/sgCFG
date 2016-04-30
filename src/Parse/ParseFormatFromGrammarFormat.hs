{-# LANGUAGE FlexibleContexts #-}
module Parse.ParseFormatFromGrammarFormat where

import GrammarFormat
import Parse.Format
import Utils

import Text.Parsec as P hiding(many, (<|>))
import Control.Applicative

import qualified Data.List as List

parseFormatFromGrammarFormat grammarFormat =
	ParseFormat {
		parseFormat_symbol =
			parseSymbol grammarFormat,
		parseFormat_or =
			map (P.try . P.string) $
			List.sortBy (flip lexic) $
			grammarFormat_or grammarFormat,
		parseFormat_prodSign =
			map (P.try . P.string) $
			List.sortBy (flip lexic) $
			grammarFormat_arrow grammarFormat,
		parseFormat_whitespaces =
			map (P.try . P.string) (grammarFormat_whitespaces grammarFormat)
		,
		parseFormat_comment =
			[parseLineComment grammarFormat],
		parseFormat_prodSep = map (P.try . P.string) $ grammarFormat_prodSep grammarFormat
	}

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
				try $ fmap Right $ parseSurround surroundVar
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
