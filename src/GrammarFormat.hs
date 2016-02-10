module GrammarFormat where

import Control.Monad
import Text.Read


data GrammarFormat
	= GrammarFormat {
		grammarFormat_leftSide :: Maybe SurroundBy,
		grammarFormat_rightSide :: Maybe SurroundBy,
		grammarFormat_var :: Maybe SurroundBy,
		grammarFormat_terminal :: Maybe SurroundBy,
		grammarFormat_taggedSymbol :: AnnotationFormat,
		{-
			the following options are a list of strings.
			first entry is default.
			if the first entry can not be used (especially in case of parsing),
				further entries are tried.
		-}
		grammarFormat_or :: [String],
		grammarFormat_arrow :: [String],
		grammarFormat_whitespaces :: [String],
		grammarFormat_prodSep :: [String],
		grammarFormat_lineComment :: [String]
		--grammarFormat_comment :: [(String, String)]
	}
	deriving (Show)
gFormatMapToLeftSide f x = x{ grammarFormat_leftSide = f (grammarFormat_leftSide x) }
gFormatMapToRightSide f x = x{ grammarFormat_rightSide = f (grammarFormat_rightSide x) }
gFormatMapToVar
	:: (Maybe SurroundBy -> Maybe SurroundBy)
	-> GrammarFormat -> GrammarFormat
gFormatMapToVar f x = x{ grammarFormat_var = f (grammarFormat_var x) }
gFormatMapToTerminal f x = x{ grammarFormat_terminal = f (grammarFormat_terminal x) }
gFormatMapToOr f x = x{ grammarFormat_or = f (grammarFormat_or x) }
gFormatMapToArrow f x = x{ grammarFormat_arrow = f (grammarFormat_arrow x) }
gFormatMapToLineComment f x = x{ grammarFormat_lineComment = f (grammarFormat_lineComment x) }

data AnnotationFormat
	= AnnotationFormat {
		annotationFormat_prepend :: Bool,
		annotationFormat_intersectBy :: String,
		annotationFormat_surroundBy :: SurroundBy
	}
	deriving (Show)

data SurroundBy
	= SurroundBy {
		fromSurroundBy :: (String, String)
	}
	deriving (Show)
surroundBy_fromText :: String -> Maybe SurroundBy
surroundBy_fromText =
	liftM SurroundBy . readMaybe

data DefaultFormat
	= Default
	| BNF
	| BNFE
	deriving (Show)

defaultFormat f =
	case f of
		Default -> def
		BNF -> bnf
		BNFE -> bnfe

-- backus naur form
bnf =
	def{
		grammarFormat_var = Just $ SurroundBy ("<",">"),
		grammarFormat_terminal = Nothing,
		grammarFormat_arrow = ["::=\n  ", "::="]
		--grammarFormat_arrow = ["::="]
	}

-- extended backus naur form
bnfe =
	def{
		grammarFormat_var = Nothing,
		grammarFormat_terminal = Just $ SurroundBy ("\"","\""),
		grammarFormat_arrow = ["::="]
	}

def =
	GrammarFormat {
		grammarFormat_leftSide = Nothing,
		grammarFormat_rightSide = Nothing,
		grammarFormat_var = Nothing,
		grammarFormat_terminal = Just $ SurroundBy ("\"","\""),
		grammarFormat_taggedSymbol =
			AnnotationFormat {
				annotationFormat_prepend = False,
				annotationFormat_intersectBy = "",
				annotationFormat_surroundBy =
					SurroundBy ("(",")")
			},
		grammarFormat_or =
			[ "\n  |"
			, "\n |"
			, "\n|"
			, "|\n"
			, "|"
			],
		--grammarFormat_or = ["\n|", "|\n", "|"],
		grammarFormat_arrow =
			[ "->\n  "
			, "->\n "
			, "->"
			],
		--grammarFormat_arrow = ["->"],
		grammarFormat_whitespaces = [" "],
		grammarFormat_prodSep = ["\n"],
		grammarFormat_lineComment = ["#"]
		--grammarFormat_comment = [("", "]
	}

applySymbolFormat :: Maybe SurroundBy -> String -> String
applySymbolFormat mSurroundBy str =
	flip (maybe str) mSurroundBy $ \info ->
		let (prefix, suffix) = fromSurroundBy info
		in concat [ prefix, str, suffix ]
