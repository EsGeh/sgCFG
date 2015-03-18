{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.Internals where

import Types
import GrammarFormat
import Prelude hiding (unlines)
import Utils (unlines)

data Grammar = Grammar { fromGrammar :: [Production] }
	deriving (Show)

data Production = Production {
	production_left :: Var,
	production_right :: [Symbol]
}
	deriving (Show)

type Symbol = Either Terminal Var

newtype Terminal = Terminal {
	terminal_name :: String
}
	deriving (Show)

newtype Var = Var {
	var_name :: String
}
	deriving (Show)

instance Pretty Var where
	pretty v = var_name v

instance Pretty Terminal where
	pretty x = "\"" ++ terminal_name x ++ "\""

instance Pretty Symbol where
	pretty x =
		either pretty pretty x

instance Pretty Production where
	pretty p =
		(concat $
			[ pretty $ production_left p
			, " -> "
			]
		)
		++
		(unwords $ map pretty $ production_right p)
		--(unwords $ map (\x -> "(" ++ pretty x ++ ")") $ production_right p)

instance ToText Grammar where
	toText g =
		unlines $
		map pretty $ fromGrammar g

instance ToTextAs GrammarFormat Var where
	toTextAs format x =
		applySymbolFormat (grammarFormat_var format) $ var_name x
		{-
		case format of
			Default -> var_name x
			BNF -> "<" ++ var_name x ++ ">"
			BNFE -> var_name x
		-}

instance ToTextAs GrammarFormat Terminal where
	toTextAs format x =
		applySymbolFormat (grammarFormat_terminal format) $ terminal_name x
	{-
		case format of
			Default -> "\"" ++ terminal_name x ++ "\""
			BNF -> terminal_name x
			BNFE -> "\"" ++ terminal_name x ++ "\""
	-}

instance ToTextAs GrammarFormat Symbol where
	toTextAs format =
			either (toTextAs format) (toTextAs format)

instance ToTextAs GrammarFormat Production where
	toTextAs format p =
		concat $
		[ toTextAs format $ production_left p
		, " "
		, prodSign
		, " "
		, unwords $ map (toTextAs format) $ production_right p
		]
		where
			prodSign = head $ grammarFormat_arrow format
				{-
				case format of
					Default -> "->"
					BNF -> "::="
					BNFE -> "::="
				-}

instance ToTextAs GrammarFormat Grammar where
	toTextAs format g =
		unlines $
		map (toTextAs format) $
		fromGrammar g

{-
data GrammarFormat
	= Default
	| BNF
	| BNFE
-}
