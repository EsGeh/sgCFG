{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar(
	Grammar(), fromGrammar,
	Production(), production_left, production_right,
	Symbol,
	Terminal(..), Var(..),
	GrammarFormat(..)
) where

import Types
import qualified Parse.DefaultSyntax as DefaultSyntax
import qualified Parse.BNF as BNF
import qualified Parse.BNFE as BNFE
import Grammar.Internals
--import Parse
import Parse.ParseToken
import Parse.AST
import Parse.ParseAST

import Text.Parsec hiding (many, (<|>))
import Control.Monad
import Control.Applicative


instance FromTextAs GrammarFormat Grammar where
	fromTextAs format str =
		mapLeft show $ grammarFromStr syntaxDescr str
		where
			syntaxDescr =
				case format of
					Default -> DefaultSyntax.syntaxDescr
					BNF -> BNF.bnf
					BNFE -> BNFE.bnfe


grammarFromStr descr = liftM grammarFromAST . astFromStr descr

astFromStr descr =
	astFromTok
	<=<
	tokensFromStr descr

astFromTok = parse parseAST ""

grammarFromAST ast =
	Grammar $
	map (uncurry Production) $
	concat $
	map allRules $
	fromAST ast
	where
		allRules :: (Var, [[Symbol]]) -> [(Var, [Symbol])]
		allRules x = (,) <$> [fst x] <*> snd x

tokensFromStr descr = parse (parseTokens descr) ""
