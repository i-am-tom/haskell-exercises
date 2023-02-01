{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import GHC.Generics (Generic (..))
import GHC.Generics





{- ONE -}

-- | Recall an old friend, the 'Newtype' class:

class Newtype (new :: Type) (old :: Type) | new -> old where
  wrap   :: old -> new
  unwrap :: new -> old

-- | a. Can we add a functional dependency to this class?

-- | b. Why can't we add two?

-- We wouldn't want to! It's very useful to be able to add multiple newtypes
-- around the /same/ old type - for example, we might want to make sure we
-- don't confuse two 'Double' values when one is being used as a distance and
-- the other as a time duration.





{- TWO -}

-- | Let's go back to a problem we had in the last exercise, and imagine a very
-- simple cache in IO. Uncomment the following:

class CanCache (f :: Type -> Type) (entity :: Type) (index :: Type)
    | entity -> index where
  store :: entity -> f ()
  load  :: index -> f (Maybe entity)

-- | a. Uh oh - there's already a problem! Any @entity@ type should have a
-- fixed type of id/@index@, though... if only we could convince GHC... Could
-- you have a go?

-- | b. @IO@ is fine, but it would be nice if we could choose the functor when
-- we call @store@ or @load@... can we parameterise it in some way?

-- | c. Is there any sort of functional dependency that relates our
-- parameterised functor to @entity@ or @index@? If so, how? If not, why not?

-- | No, and nor would we want one - it would be nice to pick the functor for
-- the same entity/index pair to be different things depending on, say, whether
-- we're running a test or in production.





{- THREE -}

-- | Let's re-introduce one of our old favourites:
data Nat = Z | S Nat

-- | When we did our chapter on @TypeFamilies@, we wrote an @Add@ family to add
-- two type-level naturals together. If we do a side-by-side comparison of the
-- equivalent "class-based" approach:

class       Add  (x :: Nat) (y :: Nat) (z :: Nat) | x y -> z, z x -> y
type family Add' (x :: Nat) (y :: Nat)    :: Nat

-- | We see here that there are parallels between classes and type families.
-- Type families produce a result, not a constraint, though we could write
-- @Add' x y ~ z => ...@ to mean the same thing as @Add x y z => ...@. Also,
-- the result of a type family is determined by its inputs - something we can
-- express as a functional dependency!

-- | a. Write the two required instances for the 'Add' class by
-- pattern-matching on the first argument. Remember that instances can have
-- constraints, and this is how we do recursion!

instance Add 'Z y y
instance Add x y z => Add ('S x) y ('S z)

-- | b. By our analogy, a type family has only "one functional dependency" -
-- all its inputs to its one output. Can we write _more_ functional
-- dependencies for @Add@? Aside from @x y -> z@? 

-- | c. We know with addition, @x + y = z@ implies @y + x = z@ and @z - x = y@.
-- This should mean that any pair of these three variables should determine the
-- other! Why couldn't we write all the possible functional dependencies that
-- /should/ make sense?

-- We should want to write @y z -> x@, but this isn't true: both instances
-- would apply to any @y@/@z@ pair where @z@ is a successor, so there isn't a
-- unique @x@ for all @y@/@z@ combinations!




{- FOUR -}

data Proxy (a :: k) = Proxy

-- | As we all know, type signatures are /not/ documentation. This is really
-- because the names of types are far too confusing. To that end, we can give
-- our types friendlier names to make the coding experience less intimidating:

class (x :: k) `IsNamed` (label :: Symbol) | x -> label, label -> x where
  fromName :: Proxy x     -> Proxy label
  fromName _ = Proxy

  toName :: Proxy label -> Proxy x
  toName _ = Proxy

-- | Now we have this class, we can get to work!

instance Int   `IsNamed` "Dylan"
instance IO    `IsNamed` "Barbara"
instance Float `IsNamed` "Kenneth"

-- | a. In our glorious new utopia, we decide to enact a law that says, "No two
-- types shall have the same name". Similarly, "No type shall have two names".
-- Is there a way to get GHC to help us uphold the law?

-- | b. Write the identity function restricted to types named "Kenneth".

kennethId :: x `IsNamed` "Kenneth" => x -> x
kennethId = id

-- | c. Can you think of a less-contrived reason why labelling certain types
-- might be useful in real-world code?

-- It might be useful, for example, if we want to produce some sort of
-- language-independent description of data structures. If our description (for
-- example, postgres, or some IDL) refers to "Number", we might want to link
-- that in some way to 'Double'.





{- FIVE -}

-- | Here's a fun little class:
class Omnipresent (r :: Symbol) (s :: Symbol) | -> r s

-- | Here's a fun little instance:
instance Omnipresent "Tom!" "Harding!"

-- | a. Is there a way to enforce that no other instance of this class can ever
-- exist? Do we /need/ variables on the left-hand side of a functional
-- dependency arrow?

-- | b. Can you think of a time you would ever want this guarantee? Is this
-- "trick" something you can think of a practical reason for doing? Perhaps if
-- we added a method to the class? (Very much an open question).

-- | We could add a method to the class that pulls the string down to the
-- value level. I'm honestly not sure that it's ever useful, though I'm hoping
-- someone wil tell me otherwise!

-- | c. Add another similarly-omnipresent parameter to this type class.





{- SIX -}

-- | You knew it was coming, didn't you?

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

data SNat (n :: Nat) where
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

-- | a. Write a function (probably in a class) that takes an 'SNat' and an
-- 'HList', and returns the value at the 'SNat''s index within the 'HList'.

class Get (xs :: [Type]) (n :: Nat) (o :: Type) | xs n -> o where
  get :: SNat n -> HList xs -> o

instance Get (x ': xs) 'Z x where
  get _ (HCons x _) = x

instance Get xs n y => Get (x ': xs) ('S n) y where
  get (SS n) (HCons _ xs) = get n xs

-- Note that we have to fix the `o` type so that it's determined!
instance TypeError ('Text "Out-of-bounds index!") => Get '[] n () where
  get _ _ = ()

-- | b. Add the appropriate functional dependency.

-- | c. Write a custom type error!

-- | d. Implement 'take' for the 'HList'.

class Take (n :: Nat) (xs :: [Type]) (os :: [Type]) | n xs -> os where
  take :: SNat n -> HList xs -> HList os

instance Take 'Z xs '[] where
  take _ _ = HNil

instance Take n xs os => Take ('S n) (x ': xs) (x ': os) where
  take (SS n) (HCons x xs) = HCons x (Exercises.take n xs)





{- SEVEN -}

-- | Recall our variant type:

data Variant (xs :: [Type]) where
  Here  ::         x  -> Variant (x ': xs)
  There :: Variant xs -> Variant (y ': xs)

-- | We previously wrote a function to "inject" a value into a variant:

class Inject (x :: Type) (xs :: [Type]) where
  inject :: x -> Variant xs

instance Inject x (x ': xs) where
  inject = Here

instance {-# INCOHERENT #-} Inject x xs
    => Inject x (y ': xs) where
  inject = There . inject

-- | Write a function to "project" a value /out of/ a variant. In other words,
-- I would like a function that takes a proxy of a type, a variant containing
-- that type, and returns /either/ a value of that type /or/ the variant
-- /excluding/ that type:
--
-- @
--   project (Proxy :: Proxy Bool) (inject True :: Variant '[Int, String, Bool])
--     === Left Bool :: Either Bool (Variant '[Int, String])
-- @

class Project (x :: Type) (xs :: [Type]) (os :: [Type]) where
  project :: Proxy x -> Variant xs -> Either x (Variant os)

instance xs ~ ys => Project x (x ': xs) ys where
  project _ (Here  x)  = Left  x
  project _ (There xs) = Right xs

instance {-# INCOHERENT #-} Project x xs os
    => Project x (y ': xs) (y ': os) where
  project _ (Here  x)  = Right (inject x)
  project p (There xs) = fmap There (project p xs)





{- EIGHT -}

-- | It would be nice if I could update a particular index of an HList by
-- providing an index and a (possibly-type-changing) function. For example:
--
-- @
--   update SZ length (HCons True (HCons "Hello" HNil))
--     === HCons True (HCons 5 HNil)
-- @

-- | Write the type class required to implement this function, along with all
-- its instances and functional dependencies.

class Update (i :: Nat) (s :: [Type]) (t :: [Type]) (a :: Type) (b :: Type)
    | i s -> a, i t -> b, i s b -> t, i t a -> s where
  update :: SNat i -> (a -> b) -> HList s -> HList t

instance Update 'Z (x ': xs) (y ': xs) x y where
  update _ f (HCons x xs) = HCons (f x) xs

instance Update n xs ys x y => Update ('S n) (a ': xs) (a ': ys) x y where
  update (SS n) f (HCons x xs) = HCons x (update n f xs)





{- NINE -}

-- | If you've made it this far, you're more than capable of digesting and
-- understanding some advanced GHC docs! Read the documentation at
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html, and
-- keep going until you hit 'Generic1' - we won't worry about that today.

-- | We can write a little function to get the name of a type as a type-level
-- symbol like so:

class NameOf (x :: Type) (name :: Symbol) | x -> name
instance GNameOf (Rep x) name => NameOf x name

-- | We then have to implement this class that examines the generic tree...
class GNameOf (rep :: Type -> Type) (name :: Symbol) | rep -> name
instance GNameOf (D1 ('MetaData name a b c) d) name

-- | Write a function to get the names of the constructors of a type as a
-- type-level list of symbols.

class ConstructorsOf (x :: Type) (names :: [Symbol]) | x -> names
instance GConstructorsOf (Rep x) names => ConstructorsOf x names

class GConstructorsOf (rep :: Type -> Type) (names :: [Symbol]) | rep -> names
instance GConstructorsOf inner name => GConstructorsOf (D1 meta inner) name

class Append (x :: [k]) (y :: [k]) (z :: [k]) | x y -> z
instance Append '[] ys ys
instance Append xs ys zs => Append (x ': xs) ys (x ': zs)

instance (GConstructorsOf left l, GConstructorsOf right r, Append l r lr)
  => GConstructorsOf (left :+: right) lr

instance GConstructorsOf (C1 ('MetaCons name x y) z) '[name]





{- TEN -}

-- In the standard library, we have a series of @liftA*@ functions, such as
-- 'liftA2', 'liftA3', 'liftA4'... wouldn't it be nice if we just had /one/
-- function called 'lift' that generalised all these?
--
-- liftA1 :: Applicative f => (a -> b) -> f a -> f b
-- liftA1 = lift
-- 
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 = lift
--
-- 
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftA3 = lift

-- Write this function, essentially generalising the f <$> a <*> b <*> c...
-- pattern. It may help to see it as pure f <*> a <*> b <*> c..., and start
-- with a function like this:

lift :: (Applicative f, Lift f i o) => i -> o
lift = lift' . pure

-- @class Lift f i o ... where lift' :: ...@ is your job! If you get this
-- right, perhaps with some careful use of @INCOHERENT@, equality constraints,
-- and functional dependencies, you should be able to get some pretty amazing
-- type inference:
--
-- >>> :t lift (++)
-- lift (++) :: Applicative f => f [a] -> f [a] -> f [a]

class Lift (f :: Type -> Type) (i :: Type) (o :: Type)
    | o -> f, f i -> o where
  lift' :: f i -> o

instance (Applicative f, Lift f b o', o ~ (f a -> o'))
    => Lift f (a -> b) o where
  lift' fs xs = lift' (fs <*> xs)

instance {-# INCOHERENT #-} Applicative f => Lift f a (f a) where
  lift' = id

test :: Applicative f => f [a] -> f [a] -> f [a]
test = lift (++)
