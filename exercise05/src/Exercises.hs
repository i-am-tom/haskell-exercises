{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
module Exercises where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)





{- ONE -}

-- | The following GADT creates a list of values of unknown types:

data Exlistential where
  Nil  :: Exlistential
  Cons :: a -> Exlistential -> Exlistential

-- | a. Write a function to "unpack" this exlistential into a list.

unpackExlistential :: Exlistential -> (forall a. a -> r) -> [r]
unpackExlistential  Nil        f = []
unpackExlistential (Cons x xs) f = f x : unpackExlistential xs f

-- | b. Regardless of which type @r@ actually is, what can we say about the
-- values in the resulting list?

-- They're all the same value - we don't know anything about @a@, so the
-- function provided will always have to be @const <something>@.

-- | c. How do we "get back" knowledge about what's in the list? Can we?

-- We can't! We'd need to alter the definition in some way to include a
-- constraint, or a non-existential index.





{- TWO -}

-- | Consider the following GADT that existentialises a 'Foldable' structure
-- (but, crucially, not the type inside).

data CanFold a where
  CanFold :: Foldable f => f a -> CanFold a

-- | a. The following function unpacks a 'CanFold'. What is its type?

unpackCanFold :: (forall f. Foldable f => f a -> r) -> CanFold a -> r
unpackCanFold f (CanFold x) = f x

-- | b. Can we use 'unpackCanFold' to figure out if a 'CanFold' is "empty"?
-- Could we write @length :: CanFold a -> Int@? If so, write it!

length :: CanFold a -> Int
length = unpackCanFold (foldr (\_ acc -> acc + 1) 0)

-- | c. Write a 'Foldable' instance for 'CanFold'. Don't overthink it.

instance Foldable CanFold where
  foldMap f (CanFold xs) = foldMap f xs





{- THREE -}

-- | Recall our existential 'EqPair' GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. Write a function that "unpacks" an 'EqPair' by applying a user-supplied
-- function to its pair of values in the existential type.

unpackEqPair :: (forall a. Eq a => a -> a -> r) -> EqPair -> r
unpackEqPair f (EqPair x y) = f x y

-- | b. Write a function that takes a list of 'EqPair's and filters it
-- according to some predicate on the unpacked values.

filter :: (forall a. Eq a => a -> a -> Bool) -> [EqPair] -> [EqPair]
filter f = Prelude.filter (unpackEqPair f)

-- | c. Write a function that unpacks /two/ 'EqPair's. Now that both our
-- variables are in rank-2 position, can we compare values from different
-- pairs?

unpackEqPairs :: (forall a. Eq a => a -> a -> r) -> EqPair -> EqPair -> (r, r)
unpackEqPairs f xs ys = (unpackEqPair f xs, unpackEqPair f ys)





{- FOUR -}

-- | When I was building @purescript-panda@, I came across a neat use case for
-- rank-2 types. Consider the following sketch of a type:

data Component input output
  -- = Some sort of component stuff.

-- | Now, let's imagine we want to add a constructor to "nest" a component
-- inside another component type. We need a way of transforming between our
-- "parent" I/O and "child" I/O, so we write this type:

data Nested input output subinput suboutput
  = Nested
      { inner  :: Component subinput suboutput
      , input  :: input -> subinput
      , output :: suboutput -> output
      }

-- | a. Write a GADT to existentialise @subinput@ and @suboutput@.

data NestedX input output where
  Nest :: Nested input output subinput suboutput -> NestedX input output

-- | b. Write a function to "unpack" a NestedX. The user is going to have to
-- deal with all possible @subinput@ and @suboutput@ types.

unpackNestedX
  :: (forall subinput suboutput. Nested input output subinput suboutput -> r)
  -> NestedX input output -> r
unpackNestedX f (Nest x) = f x

-- | c. Why might we want to existentialise the subtypes away? What do we lose
-- by doing so? What do we gain?

-- If we can get from our parent input to the child input, and from the child
-- output to the parent output, do we really care about the child inputs and
-- outputs? Probably not - by existentialising them, we don't have to keep
-- track of them in types.

-- In case you're interested in where this actually turned up in the code:
-- https://github.com/i-am-tom/purescript-panda/blob/master/src/Panda/Internal/Types.purs#L84





{- FIVE -}

-- | Let's continue with the theme of the last question. Let's say I have a few
-- HTML-renderable components:

data FirstGo input output
  = FText String
  | FHTML (String, String) [FirstGo input output]
  --       ^ properties     ^ children

-- | This is fine, but there's an issue: some functions only really apply to
-- 'FText' /or/ 'FHTML'. Now that this is a sum type, they'd have to result in
-- a 'Maybe'! Let's avoid this by splitting this sum type into separate types:

data Text = Text String
-- data HTML = HTML { properties :: [(String, String)], children :: ??? }

-- | Uh oh! What's the type of our children? It could be either! In fact, it
-- could probably be anything that implements the following class, allowing us
-- to render our DSL to an HTML string:
class Renderable component where render :: component -> String

-- | a. Write a type for the children.

data Child where
  Child :: Renderable a => a -> Child

data HTML = HTML { properties :: [(String, String)], children :: [Child] }

-- | b. What I'd really like to do when rendering is 'fmap' over the children
-- with 'render'; what's stopping me? Fix it!

instance Renderable Child where
  render (Child x) = render x

-- | c. Now that we're an established Haskell shop, we would /also/ like the
-- option to render our HTML to a Shakespeare template to write to a file
-- (http://hackage.haskell.org/package/shakespeare). How could we support this
-- new requirement with minimal code changes?

-- Add another method to the type class, and fix the instances!





{- SIX -}

-- | Remember our good ol' mystery box?

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | a. Knowing what we now know about RankNTypes, we can write an 'unwrap'
-- function! Write the function, and don't be too upset if we need a 'Maybe'.

unwrap :: MysteryBox a -> (forall a. MysteryBox a -> r) -> Maybe r
unwrap  EmptyBox        f = Nothing
unwrap (IntBox    _ xs) f = Just (f xs)
unwrap (StringBox _ xs) f = Just (f xs)
unwrap (BoolBox   _ xs) f = Just (f xs)

-- | b. Why do we need a 'Maybe'? What can we still not know?

-- We can't unwrap an empty box!

-- | c. Write a function that uses 'unwrap' to print the name of the next
-- layer's constructor.

printInner :: MysteryBox a -> String
printInner xs = fromMaybe "No inner layer" $ unwrap xs $ \case
  IntBox    _ _ -> "Now... an Int!"
  StringBox _ _ -> "Now... a String!"
  BoolBox   _ _ -> "Now... a Bool!"





{- SEVEN -}

-- | When we talked about @DataKinds@, we briefly looked at the 'SNat' type:

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | We also saw that we could convert from an 'SNat' to a 'Nat':

toNat :: SNat n -> Nat
toNat  SZ    = Z
toNat (SS n) = S (toNat n)

-- | How do we go the other way, though? How do we turn a 'Nat' into an 'SNat'?
-- In the general case, this is impossible: the 'Nat' could be calculated from
-- some user input, so we have no way of knowing what the 'SNat' type would be.
-- However, if we could have a function that would work /for all/ 'SNat'
-- values...

-- | Implement the 'fromNat' function. It should take a 'Nat', along with some
-- SNat-accepting function (maybe at a higher rank?) that returns an @r@, and
-- then returns an @r@. The successor case is a bit weird here - type holes
-- will help you!

fromNat :: Nat -> (forall n. SNat n -> r) -> r
fromNat  Z    f = f SZ
fromNat (S n) f = fromNat n (f . SS)

-- | If you're looking for a property that you could use to test your function,
-- remember that @fromNat x toNat === x@!





{- EIGHT -}

-- | Bringing our vector type back once again:

data Vector (n :: Nat) (a :: Type) where
  VNil  ::                    Vector  'Z    a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | It would be nice to have a 'filter' function for vectors, but there's a
-- problem: we don't know at compile time what the new length of our vector
-- will be... but has that ever stopped us? Make it so!

filterV :: (a -> Bool) -> Vector n a -> (forall m. Vector m a -> r) -> r
filterV p  VNil        f = f VNil
filterV p (VCons x xs) f = filterV p xs (if p x then f . VCons x else f)
