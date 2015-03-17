module Parse.Token where

import Types

class HasTokenType a where
	tokenType :: a -> TokenType

data TokenType
	= VarTokenType
	| TerminalTokenType
	| ProdSignTokenType
	| ProdSepTokenType
	| OrTokenType
	deriving (Eq, Ord, Show)

instance HasTokenType Token where
	tokenType a =
		case a of
			VarToken _ -> VarTokenType
			TerminalToken _ -> TerminalTokenType
			ProdSignToken _ -> ProdSignTokenType
			SepToken _ -> ProdSepTokenType
			OrToken _ -> OrTokenType

data Token
	= VarToken VarTokenInfo
	| TerminalToken TerminalTokenInfo
	| ProdSignToken ProdSignTokenInfo
	| SepToken SepTokenInfo
	| OrToken OrTokenInfo
	deriving (Show)

newtype VarTokenInfo
	= VarTokenInfo { fromVarTok :: String }
	deriving (Show)

newtype TerminalTokenInfo
	= TerminalTokenInfo { fromTermTok :: String }
	deriving (Show)

newtype ProdSignTokenInfo
	= ProdSignTokenInfo { fromProdSignTok :: String }
	deriving (Show)

data SepTokenInfo = SepTokenInfo
	deriving (Show)

data OrTokenInfo = OrTokenInfo
	deriving (Show)

instance Pretty Token where
	pretty t =
		"<" ++
		(case t of
			VarToken info -> (show $ tokenType t) ++ ", " ++ fromVarTok info
			TerminalToken info -> (show $ tokenType t) ++ ", " ++ fromTermTok info
			ProdSignToken info -> (show $ tokenType t)
			SepToken info -> (show $ tokenType t)
			OrToken info -> (show $ tokenType t)
		)
		++ ">"
