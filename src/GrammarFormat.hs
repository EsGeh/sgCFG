module GrammarFormat where


data GrammarFormat
	= GrammarFormat {
		grammarFormat_var :: Maybe SurroundBy,
		grammarFormat_terminal :: Maybe SurroundBy,
		{-
		grammarFormat_symbol ::
			Parsec String () () -> (Either String String),
		-}
		grammarFormat_or :: [String],
		grammarFormat_arrow :: [String],
		grammarFormat_whitespaces :: [String],
		grammarFormat_prodSep :: [String],
		grammarFormat_lineComment :: [String]
		--grammarFormat_comment :: [(String, String)]
	}
	deriving (Show)
gFormatMapToProdSign f x = x{ grammarFormat_arrow = f (grammarFormat_arrow x) }
gFormatMapToLineComment f x = x{ grammarFormat_lineComment = f (grammarFormat_lineComment x) }

data SurroundBy
	= SurroundBy {
		fromSurroundBy :: (String, String)
	}
	deriving (Show)

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

bnf =
	def{
		grammarFormat_var = Just $ SurroundBy ("<",">"),
		grammarFormat_terminal = Nothing,
		grammarFormat_arrow = ["::="]
	}
bnfe =
	def{
		grammarFormat_var = Nothing,
		grammarFormat_terminal = Just $ SurroundBy ("\"","\""),
		grammarFormat_arrow = ["::="]
	}

def =
	GrammarFormat {
		grammarFormat_var = Nothing,
		grammarFormat_terminal = Just $ SurroundBy ("\"","\""),
		grammarFormat_or = ["\n|", "|\n", "|"],
		grammarFormat_arrow = ["->"],
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
