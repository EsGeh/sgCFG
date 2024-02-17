{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GroupedGrammar.Parse where

import Grammar.Types
import GroupedGrammar.Types
import Parse.Token
import Types

import Text.Parsec hiding(many, (<|>), tokens, token)
import Control.Applicative
import Data.List( intercalate )


parseGroupedGrammar :: Monad m =>
	ParsecT [Token] u m GroupedGrammar
parseGroupedGrammar =
	fmap Grammar $
	many $
	do
		(VarToken varInfo) <- specificToken VarTokenType
		_ <- specificToken ArrowTokenType
		prod <- parseRightProdSide 
		return $
			Production (Var $ fromToken varInfo) prod

parseRightProdSide ::
	forall m u .
	Monad m =>
	ParsecT [Token] u m [[Symbol]]
parseRightProdSide = do
	tokens <-
		anyToken `manyTill` (
			(try $ specificToken SepTokenType >> return ())
			<|>
			eof
		)
		:: ParsecT [Token] u m [Token]
	return $
		map (map calcEntry) $
		splitBy ((==OrTokenType) . tokenType) tokens
	where
		calcEntry t =
			case t of
				VarToken info -> Right $ Var $ fromToken info
				TerminalToken info -> Left $ Terminal $ fromToken info
				_ -> error $ concat ["parseRightProdSide error: ", pretty t]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy cond l =
	case l of
		[] -> [[]]
		_ -> 
			let
				(s, rest) = break cond l
			in
				[s] ++ case rest of
					[] -> []
					(_:ys) -> splitBy cond ys

parseSymbol ::
	Monad m =>
	ParsecT [Token] u m Symbol
parseSymbol =
	do
		t <- oneOf' [VarTokenType, TerminalTokenType]
		return $
			case t of
				VarToken info -> Right $ Var $ fromToken info
				TerminalToken info -> Left $ Terminal $ fromToken info
				_ -> error "parseSymbol error"

oneOf' :: Monad m => [TokenType] -> ParsecT [Token] u m Token
oneOf' tokT =
	tok
		("token of type " ++ intercalate ", " (map show tokT) ++ " expected")
		((`elem` tokT) . tokenType)

specificToken :: Monad m => TokenType -> ParsecT [Token] u m Token
specificToken tokT =
	tok
		("token of type " ++ show tokT ++ " expected")
		((==tokT) . tokenType)

tok ::
	(Monad m) =>
	String -> (Token -> Bool) -> ParsecT [Token] u m Token
tok _ cond =
	tokenPrim show updatePos (\t -> if cond t then Just t else Nothing)
	where
		updatePos :: SourcePos -> Token -> [Token] -> SourcePos
		updatePos pos token _ =
			let (line, col) = tokenPos token
			in
				(flip setSourceLine line . flip setSourceColumn col) pos
