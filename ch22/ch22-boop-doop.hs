import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- Functorial context
-- We can write it like

bloop :: Integer -> Integer
bloop = fmap boop doop

-- As in function composition, fmap composes the two functions before applying
-- them to the argument.

fmap boop doop x == (*2) ((+10) x)
-- when this x comes along, it's the
-- first necessary argument to (+10)
-- then the result for that is the
-- first necessary argument to (*2)

-- Applicative context
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- Arguments are passed in parallel, and results will be added togehter

-- Monadic context
boopDoop :: Integer -> Integer boopDoop = do
a <- boop
b <- doop return (a + b)

-- We assign the variable 𝑎 to the partially-applied function boop, and 𝑏 to
-- doop. As soon as we receive an input, it will fill the empty slots in boop
-- and doop. The results will be bound to the variables 𝑎 and 𝑏 and passed into
-- return.
