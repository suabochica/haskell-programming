module Strings where

-- 1. Fold MyOr
myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

-- 2. Fold MyAnd
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\a acc -> acc || f a) False xs

-- 3. Fold MyElem
myElem :: Eq a => a -> [a] -> Bool
myElem y ys = myAny (\x -> y == x) ys

-- 4. Fold MyReverse
myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

-- 5. Fold MyMap
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> f x : acc) [] xs

-- 6. Fold MyFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

-- 7. Fold squish
squish :: [[a]] -> [a]
squish xxs = foldr (++) [] xxs

-- 8. Fold squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish . (myMap f) $ xs

-- 9. Fold squishAgain
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy _ []
-- myMaximumBy f (x:xs) = foldr g x xs where
--   g x y
--     | f x y == GT = x
--     | otherwise = y

-- myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy _ []
-- myMinimumBy f (x:xs) = foldr g x xs where
--   g x y
--     | f x y == LT = x
--     | otherwise = y
