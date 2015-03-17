{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE ExistentialQuantification #-}
module Parse.ParseToken where

import Parse.Token

import Text.Parsec hiding(many, (<|>))
import Control.Applicative
import Control.Monad
import Data.Maybe

data GrammarDescr
	= GrammarDescr {
		grammarDescr_symbol ::
			Parsec String () () -> Parsec String () (Either String String),
		--grammarDescr_terminal :: Parsec String () String,
		grammarDescr_or :: [Parsec String () String],
		grammarDescr_prodSign :: [Parsec String () String],
		grammarDescr_whitespaces :: [Parsec String () String],
		grammarDescr_prodSep :: [Parsec String () String]
	}
	--deriving (Show)

parseTokens descr =
	liftM catMaybes $
	liftM (map (either (maybe Nothing Just) Just)) $
	(liftM Left (sep descr) <|> liftM Right parseSymbol)
		`manyTill` eof
	where
		tokFromEither =
				(VarToken . VarTokenInfo)
		parseSymbol =
			liftM (either (TerminalToken . TerminalTokenInfo) (VarToken . VarTokenInfo)) $ grammarDescr_symbol descr stop
		stop = (eof <|> try (sep descr >> return ()))

sep descr=
	(try $ parseWhitespace *> return Nothing)
	<|>
	( liftM Just $
		(try $ liftM OrToken $ parseOrToken descr)
		<|> (try $ liftM SepToken $ parseSepToken descr)
		<|> (liftM ProdSignToken $ parseProdSign descr)
	)
	where
		parseWhitespace =
			many1 $
			choice $
			grammarDescr_whitespaces descr

{-
parseSymbol ::
	GrammarDescr -> 
	Parsec String () Token
parseSymbol descr =
-}

{-
	(liftM (TerminalToken . TerminalTokenInfo) $
		((lookAhead $ try $ sep descr) >> parserFail "sep found")
		<|> liftM anyChar 
	)
-}

{-
parseTokens descr =
	liftM (map $ either tokFromEither id) $
	liftM ( -- drop whitespaces
		catMaybes .
		map (either (Just . Left) $ maybe Nothing (Just . Right)) $
		) $
	parseStringOrSep $
		(try $ parseWhitespace *> return Nothing)
		<|>
		( liftM Just $
			(try $ liftM SepToken $ parseSepToken descr)
			<|> (try $ liftM ProdSignToken $ parseProdSign descr)
			<|> (try $ liftM OrToken $ parseOrToken descr)
		)
	where
		parseWhitespace = many1 $ choice $ grammarDescr_whitespaces descr
		tokFromEither =
				(VarToken . VarTokenInfo)

liftP :: Parsec String () String -> ParsecT String () m String
liftP = undefined
-}

parseOrToken descr =
	(choice $
	grammarDescr_or descr)
	>> return OrTokenInfo

parseSepToken descr =
	(many1 $ choice $
	grammarDescr_prodSep descr)
	>> return SepTokenInfo

parseProdSign descr =
	liftM ProdSignTokenInfo $
	choice $
	grammarDescr_prodSign descr


parseOrSep p sep =
	liftM Left (try sep) <|> liftM Right p

parseStringOrSep ::
	Monad m =>
	ParsecT String u m sep -> ParsecT String u m [Either String sep]
parseStringOrSep parseSep =
	liftM concLefts $
	many $ parseCharOrSep parseSep

parseCharOrSep ::
	Monad m =>
	ParsecT String u m sep -> ParsecT String u m (Either Char sep)
parseCharOrSep parseSep =
	liftM Right (try parseSep) <|> liftM Left anyChar

{-
testTokenParser ::
	String -> Either ParseError [String]
testTokenParser =
		parse (parseToken descr <* eof) ""
-}

isLeft (Right _) = False
isLeft (Left _) = True

mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x

mapRight f (Left x) = Left x
mapRight f (Right x) = Right (f x)

concLefts ::
	forall a b .
	[Either a b] -> [Either [a] b]
concLefts l =
	let
		(lefts, rest) = span isLeft l
		meltedLefts = Left $ map (\x -> case x of { Left x' -> x' }) $ lefts :: Either [a] b
	in
		case meltedLefts of
			Left [] -> processRest rest
			_ -> meltedLefts : processRest rest
		where
			processRest rest =
				(case rest of
					[] -> []
					(x:xs) -> mapLeft (const $ []) x : concLefts xs
				)
