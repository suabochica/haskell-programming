module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

--lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip x and y using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip z y

-- zip x and y using 3 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- x1: make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- x2: make a tuple of xs and ys
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- x3: takes one input and makes a tuple of the results of two applications of z'
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 x = (z' x, z' x)

-- helper functions

-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- summed: function to allow us to add the two values that are inside a tuple
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- bolt: function that lifts a boolean function over two partially-applied fns
-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print "My turn"
  print $ and <$> sequA <$> fromMaybe 0 $ xs
  print $ and <$> sequA <$> fromMaybe 0 $ s'
  print $ bolt <$> fromMaybe 0 $ ys
  print $ (fmap . fmap) bolt z' <$> fromMaybe 0 $ xs
