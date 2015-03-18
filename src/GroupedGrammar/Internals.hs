{-# LANGUAGE MultiParamTypeClasses #-}
module GroupedGrammar.Internals where


import Types
import Grammar.Internals
import GrammarFormat
import Utils (unlines)

import Prelude hiding(unlines)
import Data.List hiding (unlines)
import qualified Data.List as List
import Control.Applicative


data GroupedGrammar =
	GroupedGrammar {
		fromGroupedGrammar :: [GroupedProduction]
	}
	deriving (Show)

data GroupedProduction
	= GroupedProduction {
		groupedProd_left :: Var,
		groupedProd_right :: [[Symbol]]
	}
	deriving (Show)

instance ToText GroupedGrammar where
	toText groupedGrammar =
		unlines $
		map (uncurry showEntry . (\x -> (groupedProd_left x, groupedProd_right x))) $ fromGroupedGrammar groupedGrammar
		where
			showEntry var rightRuleSides =
				concat $
				[ pretty var
				, " -> "
				, intercalate " | " $ map (unwords . map pretty) rightRuleSides
				]

instance ToTextAs GrammarFormat GroupedGrammar where
	toTextAs format groupedGrammar =
		unlines $
		map (toTextAs format) $
		fromGroupedGrammar groupedGrammar

instance ToTextAs GrammarFormat GroupedProduction where
	toTextAs format p =
		concat $
		[ toTextAs format $ groupedProd_left p
		, " "
		, head $ grammarFormat_arrow format
		, " "
		, showRightProdSide $ groupedProd_right p
		]
		where
			showRightProdSide x =
				intercalate (head $ grammarFormat_or format) $
				map (unwords . map (toTextAs format)) $
				x
