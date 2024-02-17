{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parse.Token.Internals where


import Types

class HasTokenType a where
	tokenType :: a -> TokenType

data TokenType
	= VarTokenType
	| TerminalTokenType
	| ArrowTokenType
	| SepTokenType
	| OrTokenType
	deriving (Eq, Ord, Show)

instance HasTokenType Token where
	tokenType a =
		case a of
			VarToken _ -> VarTokenType
			TerminalToken _ -> TerminalTokenType
			ArrowToken _ -> ArrowTokenType
			SepToken _ -> SepTokenType
			OrToken _ -> OrTokenType

data Token
	= VarToken VarTokenInfo
	| TerminalToken TerminalTokenInfo
	| ArrowToken ArrowTokenInfo
	| SepToken SepTokenInfo
	| OrToken OrTokenInfo
	deriving (Show)

varToken str = VarToken . TokenInfo str
terminalToken str = TerminalToken . TokenInfo str
arrowToken str = ArrowToken . TokenInfo str
sepToken str = SepToken . TokenInfo str
orToken str = OrToken . TokenInfo str

type VarTokenInfo = TokenInfo VarType
type TerminalTokenInfo = TokenInfo TerminalType
type ArrowTokenInfo = TokenInfo ArrowType
type SepTokenInfo = TokenInfo SepType
type OrTokenInfo = TokenInfo OrType

class IsToken a where
	tokenContent :: a -> String
	tokenPos :: a -> TokenPos

instance IsToken Token where
	tokenContent x =
		case x of
			(VarToken i) -> tokenContent i
			(TerminalToken i) -> tokenContent i
			(ArrowToken i) -> tokenContent i
			(SepToken i) -> tokenContent i
			(OrToken i) -> tokenContent i
	tokenPos x =
		case x of
			(VarToken i) -> tokenPos i
			(TerminalToken i) -> tokenPos i
			(ArrowToken i) -> tokenPos i
			(SepToken i) -> tokenPos i
			(OrToken i) -> tokenPos i


instance IsToken (TokenInfo a) where
	tokenContent = fromToken
	tokenPos = token_pos

data TokenInfo tokenType
	= TokenInfo {
		fromToken :: String,
		token_pos :: TokenPos
	}
	deriving (Show)

type TokenPos = (Line, Col)
type Line = Int
type Col = Int

data VarType
data TerminalType
data ArrowType
data SepType
data OrType

instance Pretty Token where
	pretty t =
		concat $
		[ "<"
		, show (tokenType t)
		, "("
		, show (tokenPos t)
		, ")"
		, ","
		, show $ tokenContent t
		, ">"
		]
