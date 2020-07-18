module Strings where

-- Chapter Exercises
--- Building Functions
-- 1.
-- a) Given "Curry is awesome" returns " Curry is awesome!"
addExclamationMark x = concat [x, "!"]

-- b) Given "Curry is awesome" returns "y"
returnY x = (!!) x 5

-- c) Given "Curry is awesome" returns "awesome"
returnAwesome x = drop 9 x

-- 3. thirdLetter
thirdLetter :: [Char] -> Char
thirdLetter x = (!!) x 2

-- 4. letterIndex
curryIsAwesome !! x
letterIndex :: Int -> Char
letterIndex x = curryIsAwesome !! x
