import Data.List (intersperse)

-- Maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

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