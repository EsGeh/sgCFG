{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import qualified Data.List as List

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
				(case rest of
					[] -> []
					(x:xs) -> mapLeft (const $ []) x : concLefts xs
				)

firstTimeNotChanging :: Eq a => a -> [a] -> a
firstTimeNotChanging def [] = def
firstTimeNotChanging _ [a] = a
firstTimeNotChanging _ (a:b:_) | a == b = a
firstTimeNotChanging def (_:b:bs) = firstTimeNotChanging def $ b:bs
