import Data.List (intersperse)

-- Maximum
-- i: maximum' [1, 3, 8, 9, 46, 12, 24, 5]
-- o: 46
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Replicate
-- i: replicate' 3 5
-- o: [5, 5, 5]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate'(n-1) x

-- Take
-- i: take' 3 [5, 4, 3, 2, 1]
-- o: [5, 4, 3]
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Reverse
-- i: reverse' [5, 4, 3, 2, 1]
-- o: [1, 2, 3, 4, 5]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Zip
-- i: zip' [1, 2, 3] [2, 1]
-- o: [(1, 2), (3, 4)]
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip'(x:xs) (y:ys) = (x,y):zip' xs ys

-- Elem: Check if the element exist in the list
-- i: elem' 2 [2, 1]
-- o: True
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

-- Quicksort
-- i: quicksort' [2, 1, 6, 7, 3, 6, 5, 3]
-- o: [1, 2, 3, 3, 5, 6, 6, 7]
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- Digit to word
digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = ""

digits :: Int -> [Int]
digits 0 = [0]
digits x = (go x) where
  go x
    | x == 0 = []
    | otherwise = (digits (div x 10)) ++ [(mod x 10)]

wordNumber :: Int -> String
wordNumber x = concat (intersperse ("-"::String) (map digitToWord (digits x))) :: String
