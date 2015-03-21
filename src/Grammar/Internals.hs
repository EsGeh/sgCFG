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

data Production = Production {
	production_left :: Var,
	production_right :: [Symbol]
}
	deriving (Eq, Ord, Show)

---------------------------------------------------
-- instances
---------------------------------------------------

instance Pretty Production where
	pretty p =
		(concat $
			[ pretty $ production_left p
			, " -> "
			]
		)
		++
		(unwords $ map pretty $ production_right p)

instance ToText Grammar where
	toText g =
		unlines $
		map pretty $
		fromGrammar g

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

instance ToTextAs GrammarFormat Grammar where
	toTextAs format g =
		unlines $
		map (toTextAs format) $
		fromGrammar g
