module Traversable where

-- Solutions : Problems
-- Monoides : Unir cosas
-- Funtores : Transformar estructuras
-- Applicativos : Raro. La abstracción más fuerte.
-- Monadas : Desanidar las estrucuras, limpieza. No tener estructuras dentro estructuras
-- Foldable : Catamorfismo, romper la estructura.
-- Traversable : Evitar la destrucción de estructuras para hacer un intercambio entre ellas.

-- El arreglo siempre existe. Tener estructuras dentro arreglos. Recorrer las estructuras dentro de un arreglo. Flip de estructuras + fmap: moverse en la la estructura

-- Pregunta: Intercambio y luego mapeo o viceversa. Primero mapeo y luego intercambio.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = (f a)
  foldr f b (Identity a) = f a b

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving ( Eq, Show )

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap __ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a
