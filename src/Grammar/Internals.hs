{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.Internals where

import GrammarTypes
import Types
import GrammarFormat
import Prelude hiding (unlines)
import Utils (unlines)

{-
data Grammar =
	Grammar {
		fromGrammar :: [Production]
	}
	deriving (Show)
-}

type Grammar = GrammarGen Production
type Production = ProductionGen Var [Symbol]

{-
data Production = Production {
	prod_left :: Var,
	prod_right :: [Symbol]
}
	deriving (Eq, Ord, Show)
-}

---------------------------------------------------
-- instances
---------------------------------------------------

instance Pretty Production where
	pretty p =
		(concat $
			[ pretty $ prod_left p
			, " -> "
			]
		)
		++
		(unwords $ map pretty $ prod_right p)

instance ToText Grammar where
	toText g =
		unlines $
		map pretty $
		fromGrammar g

instance ToTextAs GrammarFormat Production where
	toTextAs format p =
		concat $
		[ toTextAs format $ prod_left p
		, " "
		, prodSign
		, " "
		, unwords $ map (toTextAs format) $ prod_right p
		]
		where
			prodSign = head $ grammarFormat_arrow format
