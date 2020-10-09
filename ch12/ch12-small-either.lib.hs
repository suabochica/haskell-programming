module EitherLib where

-- i: [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- o: ["foo","bar","baz"]
lefts' :: [Either a b] -> [a]
lefts' = foldr concatLeft []
  where
    concatLeft (Left a) xs = a : xs
    concatLeft (Right _) xs = xs

-- i: [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- o: [3,7]
rights' :: [Either a b] -> [b]
rights' = foldr concatRight []
  where
    concatRight (Left _) xs = xs
    concatRight (Right a) xs = a : xs

-- i: [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- o: (["foo","bar","baz"],[3,7])
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- let s = Left "foo" :: Either String Int
-- let n = Right 3 :: Either String Int

-- i: either' length (*2) s
-- o: 3

-- i: either' length (*2) n
-- o: 6
either':: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y
