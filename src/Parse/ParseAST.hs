{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Parse.ParseAST where

import Grammar.Internals hiding (unlines)
import Parse.AST
import Parse.Token

import Text.Parsec hiding(many, (<|>))
import Control.Applicative
import Control.Monad
import Data.List


{-
parseAST ::
	Monad m =>
	ParsecT [Token] u m AST
parseAST = undefined
-}

parseAST =
	liftM AST $
	many $
	do
		(VarToken varInfo) <- specificToken VarTokenType
		specificToken ProdSignTokenType
		prod <- parseRightProdSide 
		return $
			(
			(Var $ fromVarTok varInfo)
			,
			prod
			)

parseRightProdSide ::
	forall m u .
	Monad m =>
	ParsecT [Token] u m [[Symbol]]
parseRightProdSide = do
	tokens <-
		anyToken `manyTill` ((try $ specificToken ProdSepTokenType >> return ()) <|> eof)
		:: ParsecT [Token] u m [Token]
	return $
		map (map calcEntry) $
		splitBy ((==OrTokenType) . tokenType) tokens
	where
		calcEntry t =
			case t of
				VarToken info -> Right $ Var $ fromVarTok info
				TerminalToken info -> Left $ Terminal $ fromTermTok info

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

{-
parseRightProdSide ::
	forall m u .
	Monad m =>
	ParsecT [Token] u m [[Symbol]]
parseRightProdSide =
	(stopRule >> return [])
	<|>
	(do
		symbols <- parseSymbols
		rest <- parseRightProdSide
		{-
		rest <-
			(stopRule >> return [])
			<|> parseRightProdSide
		-}
		return $ (symbols:rest)
	)



--parseSymbols :: ParsecT [Token] u m [Symbol]
parseSymbols =
	(do
		((try $ specificToken OrTokenType) >> return ())
			<|> stopRule
		return []
	)
	<|>
	(do
		x <- parseVarOrTerm
		xs <- parseSymbols
		return $ x:xs
	)
stopRule =
	((try $ specificToken ProdSepTokenType) >> return ())
	<|> eof
-}

parseVarOrTerm ::
	Monad m =>
	ParsecT [Token] u m Symbol
parseVarOrTerm =
	do
		t <- oneOf' [VarTokenType, TerminalTokenType]
		return $
			case t of
				VarToken info -> Right $ Var $ fromVarTok info
				TerminalToken info -> Left $ Terminal $ fromTermTok info

oneOf' tokT =
	tok
		("token of type " ++ intercalate ", " (map show tokT) ++ " expected")
		((`elem` tokT) . tokenType)

specificToken tokT =
	tok
		("token of type " ++ show tokT ++ " expected")
		((==tokT) . tokenType)

tok :: (Show t, Monad m) =>
	String -> (t -> Bool) -> ParsecT [t] u m t
tok errMsg cond = do
	t <- anyToken 
	if cond t then return t else parserFail errMsg
