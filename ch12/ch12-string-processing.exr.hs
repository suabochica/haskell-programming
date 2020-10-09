module StringProcessing where

-- id :: a -> a
-- a has the kind *

-- r :: a -> f a
-- a has the kind *
-- f has the kind * -> *


-- 1. notThe and replaceThe
-- i: "the", o: Nothing
-- i: "blahtheblah", o: "blahtheblah"
notThe :: String -> Maybe String
notThe ('t':'h':'e':' ':xs) = Nothing
notThe xs = Just xs

-- i: "the cow love use", o: "a cow love use"
replaceThe :: String -> String
replaceThe "" = ""
replaceThe xs =
  case notThe xs of
    Nothing -> 'a' : replaceThe (drop 3 xs)
    Just (y:ys) -> y : replaceThe ys

-- 2. countTheBeforeVowel
-- i: "the cow", o: 0
-- i: "the cow evil", o: 1
isVowel :: Char -> Bool
isVowel x = any (== x) "aeiou"

-- @ "Read as" in pattern matching
beforeVowel phrase@('t':'h':'e':' ':x:xs)
  | isVowel x = Nothing
  | otherwise = Just phrase
beforeVowel xs = Just xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel xs =
  case beforeVowel xs of
    Nothing -> 1 + countTheBeforeVowel (drop 4 xs)
    Just (y:ys) -> 0 + countTheBeforeVowel ys

-- 3. countVowels
-- i: "the cow", o: 2
-- i: "Mikolajczak", o: 4
countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

-- Validate the word
isConsonant :: Char -> Bool
isConsonant x = any (== x) consonants
  where
    consonants = filter (not . isVowel) ['a'..'z']

countConsonants :: String -> Integer
countConsonants = toInteger . length . filter isConsonant

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs
  | invalidWord = Nothing
  | otherwise = Just (Word' xs)
  where
    invalidWord = countVowels xs > countConsonants xs
