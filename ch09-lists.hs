-- 2. Filter letters
hello :: () -> String
hello () = [ x | x <- "HbEfLrLx0", is Upper x ]

-- 3. Capitalize
capitalize :: String -> string
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

-- 4. All caps recursive
allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

-- 5. Cap head
capHead :: String -> Char
capHead = toUpper . head

-- Caesar cipher
-- cycle function give us the relation alphabet index
-- ord c give us the integer value of a char
-- 97 is the ASCII value for char 'a'
-- !! operator that return the access of an element
caesarCipher :: Int -> Char -> Char
caesarCipher n = go
  where
    go :: Char -> Char
    go char = cycle ['a'..'z'] !! ((ord c) + n - 97)

caesarDecipher :: Int -> Char -> Char
caesarDecipher n = go
  where
    go :: Char -> Char
    go char = cycle ['a'..'z'] !! ((ord c) - n - 97)

caesarIdentity :: Char -> Char
caesarIdentity = (caesarDecipher 3) . (caesarCipher 3)

cipherMessage :: String -> String
cipherMessage str = map (caesarCipher 3) str

decipherMessage :: String -> String
decipherMessage str = map (caesarDecipher 3) str


 -- Standard functions
 -- 1. MyAnd
myAnd :: [Bool] -> Bool
myAnd [] = False
myAnd (x:xs) = x && myAnd xs

-- 2. MyOr
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 3. MyAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

-- 4. MyElem
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

-- 5. MyReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6. squish: flatMap
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish(xs)

-- 7. squishMap: flatMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f(x) ++ squishMap f xs
