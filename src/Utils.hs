{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Data.List as List


unlines = List.intercalate "\n"

mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)

isLeft (Right _) = False
isLeft (Left _) = True

mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

mapRight _ (Left x) = Left x
mapRight f (Right x) = Right (f x)

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

{-
{-
	if string a is a prefix of string b, then b will come before a
	ex.:
	orderByPrefix ["a", "ab", "cde"] == ["ab", "a", "cde"]
-}
orderByPrefix :: Ord a => [[a]] -> [[a]]
orderByPrefix = sortBy cmp
	where
		cmp [] [] = EQ
		cmp [] (_:_) = GT
		cmp (_:_) [] = LT
		cmp (x:xs) (y:ys) =
			case compare x y of
				EQ -> cmp xs ys
				other -> other
-}

-- if two or more lists have the same prefix (/= epsilon), they are returned as a group.
{- e.g.
	groupByPrefix ["abcdef", "abc", "abcHURZ", "xyz"] == [("xyz",[]), ("abc",["HURZ", "", "def"]) ]
-}
groupByPrefix ::
	(Eq a, Ord a) => [[a]] -> [([a], [[a]])]
groupByPrefix l =
	foldl conc init $
	sortBy lexic l
	where
		conc :: (Eq a, Ord a) => [([a],[[a]])] -> [a] -> [([a],[[a]])] 
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

		init = []

-- |note: if a is a prefix of b, then a < b
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
