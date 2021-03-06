module Sum where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- A Functor instance that applies the function only to First,
-- Either's left is impossible because the Functor instance
-- requires a Kind of * -> *
