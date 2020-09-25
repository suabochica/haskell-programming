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

-- Kinds
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
