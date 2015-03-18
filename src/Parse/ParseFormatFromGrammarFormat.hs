module Parse.ParseFormatFromGrammarFormat where

import GrammarFormat
import Parse.Format

import Text.Parsec as P hiding(many, (<|>))
import Control.Monad
import Control.Applicative

parseFormatFromGrammarFormat grammarFormat =
	ParseFormat {
		parseFormat_symbol =
			symbol grammarFormat,
		parseFormat_or =
			map (P.try . P.string) $
			grammarFormat_or grammarFormat,
		parseFormat_prodSign =
			map (P.try . P.string) $
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

parseSurround surround =
	let (prefix, suffix) = fromSurroundBy surround
	in
		P.string prefix *> (P.anyChar `P.manyTill` (P.lookAhead $ P.try $ P.string suffix)) <* P.string suffix

parseUntilStop stop =
	anyChar `manyTill` (lookAhead stop)
