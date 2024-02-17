{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Utils(
	module Utils,
	module Control.Monad,
	module Control.Monad.Except
) where

import Control.Monad.Except
import Control.Monad
import Data.List( sortBy )


mapFst :: (a -> t) -> (a, b) -> (t, b)
mapFst f (a, b) = (f a, b)
mapSnd :: (b -> t) -> (a, b) -> (a, t)
mapSnd f (a, b) = (a, f b)

mapFstM :: Monad m => (a -> m t) -> (a, b) -> m (t, b)
mapFstM f (a, b) = fmap (, b) $ f a
mapSndM :: Monad m => (b -> m t) -> (a, b) -> m (a, t)
mapSndM f (a, b) = fmap (a, ) $ f b

isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _) = True

mapLeft :: (l -> t) -> Either l r -> Either t r
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

mapRight :: (r -> t) -> Either l r -> Either l t
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right (f x)

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a,b,c) = f a b c

concLefts ::
	forall a b .
	[Either a b] -> [Either [a] b]
concLefts l =
	let
		(lefts, rest) = span isLeft l
		meltedLefts = Left $ map (\x -> case x of { Left x' -> x'; _ -> error "this error never occurs" }) $ lefts :: Either [a] b
	in
		case meltedLefts of
			Left [] -> processRest rest
			_ -> meltedLefts : processRest rest
		where
			processRest rest =
				case rest of
					[] -> []
					(x:xs) -> mapLeft (const []) x : concLefts xs


-- if two or more lists have the same prefix (/= epsilon), they are returned as a group.
{- e.g.
	groupByPrefix ["abcdef", "abc", "abcHURZ", "xyz"] == [("xyz",[]), ("abc",["HURZ", "", "def"]) ]
-}
groupByPrefix ::
	Ord a => [[a]] -> [([a], [[a]])]
groupByPrefix l =
	reverse $ -- <- quick and dirty solution
	foldl conc start $
	sortBy lexic l
	where
		conc :: (Eq a) => [([a],[[a]])] -> [a] -> [([a],[[a]])] 
		conc res l2 =
				case res of
					((pref, rests):xs) ->
						case longestCommonPrefix' pref l2 of
							(pref', _, newRest2) | pref' /= [] ->
								(pref'
								, newRest2
								  : map (drop (length pref') pref ++) rests
								)
								: xs
							_ -> (l2,[[]]):res
					_ -> [(l2, [[]])]

		start = []

-- |note: if a is a prefix of b, then a < b
lexic :: Ord a => [a] -> [a] -> Ordering
lexic a b =
	case (a,b) of
		([],[]) -> EQ
		([],_) -> LT
		(_,[]) -> GT
		(x:xs, y:ys) | x == y -> lexic xs ys
		(x:_, y:_) -> compare x y

longestCommonPrefix' :: Eq a => [a] -> [a] -> ([a],[a],[a])
longestCommonPrefix' l1 l2 =
	case (l1, l2) of
		(x:xs, y:ys) | x == y ->
			let (pref, rest1, rest2) = longestCommonPrefix' xs ys
			in
				(x : pref, rest1, rest2)
		_ -> ([], l1, l2)

replaceAllByCond :: (a -> Bool) -> [a] -> [a] -> [a]
replaceAllByCond cond strToInsert l =
	case l of
		(x:xs) | cond x -> strToInsert ++ replaceAllByCond cond strToInsert xs
		(x:xs) -> x: replaceAllByCond cond strToInsert xs
		_ -> []

replaceAll_byIndex :: Int -> [a] -> [a] -> [a]
replaceAll_byIndex index insertThis l =
		case splitAt index l of
			(prec,_:rest) -> prec ++ insertThis ++ rest
			_ -> l

firstTimeNotChanging :: Eq a => a -> [a] -> a
firstTimeNotChanging def [] = def
firstTimeNotChanging _ [a] = a
firstTimeNotChanging _ (a:b:_) | a == b = a
firstTimeNotChanging def (_:b:bs) = firstTimeNotChanging def $ b:bs

repeatTillNotChanging :: Eq a => (a -> a) -> a -> a
repeatTillNotChanging f x
	| f x == x = x
	| otherwise = repeatTillNotChanging f (f x)

repeatTillNotChangingM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
repeatTillNotChangingM f x =
	do
		new <- f x
		if new == x
		then return $ x
		else
			repeatTillNotChangingM f new
