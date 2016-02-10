module GroupedGrammar.Transformations.ElimLeftRekur where

import GroupedGrammar.Transformations.VarNameMonad
import GroupedGrammar.Types
import GroupedGrammar.Transformations.Types
import Grammar.Types
import Utils.Graph

import qualified Data.Map as M
import Data.List
import Control.Monad


elimLeftRekur ::
	GroupedGrammarTagged [SymbolTag] -> M.Map Var prodTag -> Graph Symbol (GroupedProductionTagged [SymbolTag])
	-> Maybe (GroupedGrammar_SeparateProdTags prodTag [SymbolTag])
elimLeftRekur grammar _ _ = undefined
