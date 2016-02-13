{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GroupedGrammar.Transformations.VarNameMonad(
	VarNameMonad(..),
	getSimilarVar,
	runVarNameMonad
) where

import GroupedGrammar.Types
import Grammar.Types
import GroupedGrammar.Transformations.Types

import qualified Data.Set as S
import qualified Data.Either as Either
import Data.Char
import Control.Monad.State


newtype VarNameMonad a = VarNameMonad {
	fromVarNameMonad :: State VarNameState a
}
	deriving( Monad, Applicative, Functor )

data VarNameState = VarNameState {
	fromVarNameState :: S.Set Var,
	varNameState_varScheme :: VarScheme
}

varNameState_map f s = s{ fromVarNameState = f (fromVarNameState s) }

getSimilarVar :: Var -> VarNameMonad Var
getSimilarVar var =
	VarNameMonad $
	do
		state <- get
		let
			varsSet = fromVarNameState state
			scheme = varNameState_varScheme state
		let
			newVar = calcNewVar scheme varsSet var
		modify $ varNameState_map $ (`S.union` S.singleton newVar)
		return $ newVar
	where
		calcNewVar :: VarScheme -> S.Set Var -> Var -> Var
		calcNewVar scheme varSet var' =
			genFreeVar $
			case scheme of
				FromVar -> var'
				Const varName -> Var $ varName
			where
				genFreeVar var =
					if var `S.member` varSet
						then
							let
								varName = var_name var
								(prefix, suffix) = spanEnd isDigit $ varName
							in
								genFreeVar $
								case suffix of
									[] -> Var $ prefix ++ "0"
									_ -> Var $ prefix ++ show ((read suffix :: Int) + 1)
						else
							var

runVarNameMonad :: VarScheme -> VarNameMonad a -> GroupedGrammar -> a
runVarNameMonad scheme m g =
	(evalState $ fromVarNameMonad m) $ flip VarNameState scheme $
	S.fromList $
	collectAllVars $
	fromGrammar g
	where
		collectAllVars :: [GroupedProduction] -> [Var]
		collectAllVars =
			join
			.
			map (
				\prod -> [prod_left prod] ++ join (map Either.rights (prod_right prod))
			)

spanEnd cond = (\(a,b) -> (reverse b, reverse a)) . span cond . reverse
