{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

-- | b. Write a type family '*' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.





{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

-- append :: Vector m a -> Vector n a -> Vector ??? a

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

-- flatMap :: Vector n a -> (a -> Vector m b) -> Vector ??? b
flatMap = error "Implement me!"





{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

-- | b. Write the type-level @||@ function for booleans.

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.





{- FOUR -}

-- | a. Nat fun! Write a type-level '<=' function using the promoted 'Ordering'
-- type.

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

-- | c. Write a family to get the maximum natural in a list.





{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.





{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.





{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: Type) :: Constraint where
  -- ...

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
