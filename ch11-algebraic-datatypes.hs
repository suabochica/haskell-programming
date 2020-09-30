-- A type can be thought of as an enumeration of constructors that have zero or more arguments
-- Sum types: Union/Exclusive OR, Boolean
-- Product types: Record/Inclusive AND, Tuples
-- Product types with record syntax
-- Type aliases

-- Data declarations: data Bool = False | True
--                          [1]        [2]
-- 1. Type constructor, type level
-- 2. Data constructor, term level

-- Data constructor with parameters
-- type Name = string
-- data Person = None | Person String deriving Show

-- Type constructor with parameters
-- data Maybe a = Nothing | Some a

--Kinds
-- "Kinds are to types, what types are to data"
-- Kinds are the types of type, and in haskell they are repesented with *

-- Arity
-- Quantity of parameter that a definitiona receive
-- Nullary, zero parameters
-- Unary, one parameter
-- Binary, two parameters

-- Algebraic Data Types
-- Cardinality: In set, is the cuantity of numbers that an element can contain
-- Relationship between category theory and set theory

-- newtype
-- The number of inhabitants of Goats is the same as Int in
-- data Goats = Int deriving (Eq, Show)
-- i.e Unary constructors are the identity function for cardinality
-- newtype keyword is used to define types with a single unary data constructor

-- Records
-- -----------
-- Records make it easy to define record file accesors
dataPlace = House {price :: Int, address :: String, area :: Float}

-- Accidental bottoms from records
-- Error when you try to access to the price field of a Castle
dataPlace = House {price :: Int, address :: String, area :: Float}
          | Caste {area :: Int}

-- Valid definition
dataPlace = House {area :: Int}
          | Caste {area :: Int}

-- Normal Form
-- -----------
-- The normal form means representing a type as a sum of products.

a * (b + c) = (a * b) + (a * c)

data BookType = FictionBook | NonFictionBook
type AuthorName = String

-- === a * (a + c)
data Author = Author AuthorName BookType

-- === (a + b) + (a * c)
data Author = Fiction AuthorName | NonFiction AuthorName

-- Constructing and decosntructing values
-- -----------
-- Constructor: produce a value
-- Deconstructor: consume a value, Pattern matching consume the value
-- Not recommendable, build records in partial form

data FarmerRec =
  FarmerRec {name :: Name, acres :: Acres, farmerType :: FarmerType} deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> Trye
  _ -> False

-- Exercises
-- -----------

data Quad =
  One
  | Two
  | Three
  | Four

-- 1, eQuad :: Either Quad Quad 4^4
-- 1, eQuad :: Either Quad Quad 4^4

-- Higher kinded datatypes
-- -----------
-- A kind that is not fully applied 

-- List are polymorphic
-- -----------
-- Lists can contain values of any type

data [] a = [] | a : [a]

-- Binary Tree
-- -----------

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- insertion operation
insert' :: Ord a => -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b ( Node lert a right )
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- map operation
map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf = Leaf
map' f ( Node lert a right ) = Nod (map' f left) (f a) (map'f right)
