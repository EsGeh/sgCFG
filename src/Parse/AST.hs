module Parse.AST where

import Types
import Grammar.Internals hiding (unlines)

import Prelude hiding(unlines)
import Data.List hiding (unlines)
import qualified Data.List as List
import Control.Applicative

unlines = List.intercalate "\n"


data AST =
	AST {
		fromAST :: [(Var, [[Symbol]])]
	}
	deriving (Show)

instance ToText AST where
	toText ast =
		unlines $
		map (uncurry showEntry) $ fromAST ast
		where
			showEntry var rightRuleSides =
				concat $
				[ pretty var
				, " -> "
				, intercalate " | " $ map (unwords . map pretty) rightRuleSides
				]
