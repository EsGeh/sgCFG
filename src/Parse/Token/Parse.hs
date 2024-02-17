{-# LANGUAGE ScopedTypeVariables #-}
module Parse.Token.Parse where

import Parse.Format
import Parse.Token.Internals
import Utils (concLefts)

import Text.Parsec hiding(many, (<|>))
import Control.Applicative
import Data.Maybe
import Data.Functor.Identity


parseTokens :: ParseFormat -> ParsecT String () Identity [Token]
parseTokens descr =
	fmap (
		deleteRepetitiveSeperators
		. mapMaybe (either (maybe Nothing Just) Just)
	) $
	((try $ fmap Left $ sep descr) <|> fmap Right parseSymbol)
	`manyTill`
	(try $ lookAhead $ skipAtEndOfFile >> eof)
	where
		parseSymbol =
			either terminalToken varToken
			<$>
			parseFormat_symbol descr stop
			<*>
			getPos
		stop = (eof <|> try (sep descr >> return ()))
		skipAtEndOfFile =
			many (
				choice $ map try $
				[ parseComment descr >> return ()
				, parseWhitespace descr >> return ()
				, parseSepToken descr >> return ()
				]
			)
			>> return ()

getPos :: ParsecT s u Identity (Text.Parsec.Line, Column)
getPos = 
	fmap (\p -> (sourceLine p, sourceColumn p)) getPosition
	

deleteRepetitiveSeperators :: [Token] -> [Token]
deleteRepetitiveSeperators l =
	map (either head id) $
	concLefts $
	map (\t -> if tokenType t == SepTokenType then Left t else Right t) l

sep :: ParseFormat -> ParsecT String () Data.Functor.Identity.Identity (Maybe Token)
sep descr =
	choice $
	[ try $ parseComment descr *> return Nothing
	, try $ parseWhitespace descr *> return Nothing
	, fmap Just $
		choice $
		[ try $ fmap OrToken $ parseOrToken descr
		, try $ fmap ArrowToken $ parseArrow descr
		]
	, fmap (fmap SepToken) $ parseSepToken descr
	]

type ParseFormatted res = ParseFormat -> ParsecT String () Identity res

parseComment, parseWhitespace :: ParseFormatted [String]
parseComment descr =
	many1 $ choice $
	parseFormat_comment descr

parseWhitespace descr =
	many1 $
	choice $
	parseFormat_whitespaces descr

parseOrToken :: ParseFormatted (TokenInfo tokenType)
parseOrToken descr =
	TokenInfo
	<$>
	(choice $
	parseFormat_or descr)
	<*> getPos

parseSepToken :: ParseFormatted (Maybe SepTokenInfo)
parseSepToken descr =
	(\seps pos -> return $ TokenInfo (concat seps) pos)
	<$>
	many1 ((choice $ parseFormat_prodSep descr) <* notFollowedBy (parseWhitespace descr))
	<*>
	getPos

parseArrow :: ParseFormatted (TokenInfo tokenType)
parseArrow descr =
	TokenInfo
	<$>
	(choice $
	parseFormat_prodSign descr)
	<*> getPos

parseOrSep :: ParsecT s u m b -> ParsecT s u m a -> ParsecT s u m (Either a b)
parseOrSep p sepParser =
	fmap Left (try sepParser) <|> fmap Right p

parseStringOrSep ::
	Monad m =>
	ParsecT String u m sep -> ParsecT String u m [Either String sep]
parseStringOrSep parseSep =
	fmap concLefts $
	many $ parseCharOrSep parseSep

parseCharOrSep ::
	Monad m =>
	ParsecT String u m sep -> ParsecT String u m (Either Char sep)
parseCharOrSep parseSep =
	fmap Right (try parseSep) <|> fmap Left anyChar
