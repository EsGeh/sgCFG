{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GroupedGrammar.Transformations.VarNameMonad(
	VarNameMonad(..),
	getSimilarVar,
	runVarNameMonad
) where

import GroupedGrammar.Internals
import GrammarTypes

import qualified Data.Set as S
import Data.Char
import Control.Monad.State


newtype VarNameMonad a = VarNameMonad {
	fromVarNameMonad :: State VarNameState a
}
	deriving( Monad, Applicative, Functor )

{-
instance (Eq a) => VarNameMonad a where
	a == b = 
-}

{-
data VarNameState = VarNameState {
	fromVarNameState :: Int
}
-}
data VarNameState = VarNameState {
	fromVarNameState :: S.Set Var
}

varNameState_map f s = s{ fromVarNameState = f (fromVarNameState s) }

getSimilarVar :: Var -> VarNameMonad Var
getSimilarVar var =
	VarNameMonad $
	do
		state <- get
		let newVar = calcNewVar var state
		modify $ varNameState_map $ (`S.union` S.singleton newVar)
		return $ newVar
	where
		calcNewVar var state =
			if var `S.member` fromVarNameState state
				then
					let
						varName = var_name var
						(prefix, suffix) = spanEnd isDigit $ varName
					in
						flip calcNewVar state $
						case suffix of
							[] -> Var $ prefix ++ "0"
							_ -> Var $ prefix ++ show ((read suffix :: Int) + 1)
				else
					var

spanEnd cond = (\(a,b) -> (reverse b, reverse a)) . span cond . reverse
{-
getNewVar :: VarNameMonad Var
getNewVar =
	VarNameMonad $
	do
		state <- get
		modify $ varNameState_map $ (+1)
		return $
			Var $ "genVar" ++ (show $ fromVarNameState $
			state)
-}

runVarNameMonad :: VarNameMonad a -> GroupedGrammar -> a
runVarNameMonad m g =
	(evalState $ fromVarNameMonad m) $ VarNameState $
	S.fromList $
	collectAllVars $
	fromGrammar g
	where
		collectAllVars =
			map $ \prod ->
				prod_left prod
