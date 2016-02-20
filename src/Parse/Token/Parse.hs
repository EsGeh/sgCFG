{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE ExistentialQuantification #-}
module Parse.Token.Parse where

import Parse.Format
import Parse.Token.Internals
import Utils (concLefts)

import Text.Parsec hiding(many, (<|>))
import Control.Applicative
import Data.Maybe

parseTokens descr =
	fmap (
		deleteRepetitiveSeperators
		. mapMaybe (either (maybe Nothing Just) Just)
	)$
		-- Either (Maybe Token) Token
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

getPos = 
	fmap (\p -> (sourceLine p, sourceColumn p)) getPosition
	

deleteRepetitiveSeperators :: [Token] -> [Token]
deleteRepetitiveSeperators l =
	map (either head id) $
	concLefts $
	map (\t -> if tokenType t == SepTokenType then Left t else Right t) l

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

parseComment descr =
	many1 $ choice $
	parseFormat_comment descr

parseWhitespace descr =
	many1 $
	choice $
	parseFormat_whitespaces descr

parseOrToken descr =
	TokenInfo
	<$>
	(choice $
	parseFormat_or descr)
	<*> getPos
	-- >> return OrTokenInfo

parseSepToken ::
	ParseFormat -> Parsec String () (Maybe SepTokenInfo)
parseSepToken descr =
	(\seps pos -> return $ TokenInfo (concat seps) pos)
	<$>
	many1 ((choice $ parseFormat_prodSep descr) <* notFollowedBy (parseWhitespace descr))
	<*>
	getPos
	{-
	(
		(choice $ parseFormat_prodSep descr)
		`sepBy1`
		(many $ ((try $ parseComment descr) <|> (parseWhitespace descr)))
	)
	-}
	{-
	>>
	return (Just SepTokenInfo)
	-}

	{-((eof >> return Nothing)
		<|>
		return (Just SepTokenInfo))
	-}
	--(eof >> return Nothing) <|> return (Just SepTokenInfo)

parseArrow descr =
	TokenInfo
	<$>
	(choice $
	parseFormat_prodSign descr)
	<*> getPos

parseOrSep p sep =
	fmap Left (try sep) <|> fmap Right p

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

{-
testTokenParser ::
	String -> Either ParseError [String]
testTokenParser =
		parse (parseToken descr <* eof) ""
-}
