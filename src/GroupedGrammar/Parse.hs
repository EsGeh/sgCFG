{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GroupedGrammar.Parse where

import GrammarTypes
import GroupedGrammar.Internals
import Parse.Token
import Types

import Text.Parsec hiding(many, (<|>))
import Control.Applicative
import Control.Monad
import Data.List


parseGroupedGrammar =
	liftM Grammar $
	many $
	do
		(VarToken varInfo) <- specificToken VarTokenType
		specificToken ArrowTokenType
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

splitBy cond l =
	case l of
		[] -> [[]]
		_ -> 
			let
				(s, rest) = span (not . cond) l
			in
				[s] ++ case rest of
					[] -> []
					(y:ys) -> splitBy cond ys

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

oneOf' tokT =
	tok
		("token of type " ++ intercalate ", " (map show tokT) ++ " expected")
		((`elem` tokT) . tokenType)

specificToken tokT =
	tok
		("token of type " ++ show tokT ++ " expected")
		((==tokT) . tokenType)

tok ::
	(Monad m) =>
	String -> (Token -> Bool) -> ParsecT [Token] u m Token
tok errMsg cond =
	tokenPrim show updatePos (\t -> if cond t then Just t else Nothing)
	where
		updatePos :: SourcePos -> Token -> [Token] -> SourcePos
		updatePos pos token _ =
			let (line, col) = tokenPos token
			in
				(flip setSourceLine line . flip setSourceColumn col) pos
		{-
		updatePos pos token stream =
			case tokenType token of
				SepTokenType -> 
					incSourceLine pos 1
				_ -> 
					incSourceColumn pos 1
		-}
{-
	do
	t <- anyToken 
	if cond t then return t else parserFail errMsg
-}