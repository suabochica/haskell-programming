module BasicTypes where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1. length function
-- length is a function that takes a list and return a result that tell
-- how may items are in the list.

-- 8. palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)

-- 9. absolute value
myAbs :: Integer -> Integer
myAbs n = if n >= 0 then n else -n
