{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GroupedGrammar.Transformations.VarNameMonad(
	VarNameMonad(..),
	getNewVar,
	runVarNameMonad
) where

import GroupedGrammar.Internals
import GrammarTypes

import Control.Monad.State


newtype VarNameMonad a = VarNameMonad {
	fromVarNameMonad :: State VarNameState a
}
	deriving( Monad, Applicative, Functor )

{-
instance (Eq a) => VarNameMonad a where
	a == b = 
-}

data VarNameState = VarNameState {
	fromVarNameState :: Int
}
varNameState_map f s = s{ fromVarNameState = f (fromVarNameState s) }

{-
data VarNameState = VarNameState {
	fromVarNameState :: S.Set Var
}
-}

getNewVar :: VarNameMonad Var
getNewVar =
	VarNameMonad $
	do
		state <- get
		modify $ varNameState_map $ (+1)
		return $
			Var $ "genVar" ++ (show $ fromVarNameState $
			state)
		

runVarNameMonad :: VarNameMonad a -> GroupedGrammar -> a
runVarNameMonad m _ =
	(evalState $ fromVarNameMonad m) $ VarNameState 0
