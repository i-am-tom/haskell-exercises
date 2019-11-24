{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exercises where

import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Control.Applicative (liftA2)




{- ONE -}

-- | Consider the following types:

newtype MyInt   = MyInt   Int
newtype YourInt = YourInt Int

-- | As Haskell programmers, we love newtypes, so it would be super useful if
-- we could define a class that relates a newtype to the type it wraps, while
-- also giving us functions to get between them (we can call them 'wrap' and
-- 'unwrap').

-- | a. Write the class!

class Newtype a b where
 wrap   :: a -> b
 unwrap :: b -> a

-- | b. Write instances for 'MyInt' and 'YourInt'.

instance Newtype Int MyInt where
  wrap             = MyInt
  unwrap (MyInt n) = n

instance Newtype Int YourInt where
  wrap             = YourInt
  unwrap (YourInt n) = n

-- | c. Write a function that adds together two values of the same type,
-- providing that the type is a newtype around some type with a 'Num' instance.
addNewtype :: forall a b. (Num a, Newtype a b) => Proxy a -> b -> b -> b
addNewtype _ b1 b2 = wrap @a $ unwrap b1 + unwrap b2

-- | d. We actually don't need @MultiParamTypeClasses@ for this if we use
-- @TypeFamilies@. Look at the section on associated type instances here:
-- https://wiki.haskell.org/GHC/Type_families#Associated_type_instances_2 -
-- rewrite the class using an associated type, @Old@, to indicate the
-- "unwrapped" type. What are the signatures of 'wrap' and 'unwrap'?

class Newtype' a where
  type Old a :: Type
  wrap' :: a -> Old a
  unwrap' :: Old a -> a

instance {-# OVERLAPPING #-} Newtype' Int where
  type Old Int = MyInt
  wrap'             = MyInt
  unwrap' (MyInt a) = a

--instance [># OVERLAPPABLE #<] Newtype' Int where
  --type Old Int = YourInt
  --wrap'             = YourInt
  --unwrap' (YourInt a) = a




{- TWO -}

-- | Who says we have to limit ourselves to /types/ for our parameters? Let's
-- look at the definition of 'traverse':

traverse1 :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse1 = traverse

-- | This is all very well, but we often don't need @f@ to be an 'Applicative'.
-- For example, let's look at the good ol' 'Identity' type:

newtype Identity a = Identity a
  deriving Functor -- LANGUAGE DeriveFunctor

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- | We can see that, in the @Traversable@ instance, we don't actually use
-- @pure@ /or/ @(<*>)@ - we only use @<$>@! It would be nice if we could have a
-- better @Traversable@ class that takes both the @t@ type /and/ the constraint
-- we want on the @f@...

-- | a. Write that little dazzler! What error do we get from GHC? What
-- extension does it suggest to fix this?

class Wanderable (c :: (Type -> Type) -> Constraint) (t :: Type -> Type) where
 wander :: c f => (a -> f b) -> t a -> f (t b)


-- | b. Write a 'Wanderable' instance for 'Identity'.

instance Wanderable Functor Identity where
  wander f (Identity x) = Identity <$> f x

-- | c. Write 'Wanderable' instances for 'Maybe', '[]', and 'Proxy', noting the
-- differing constraints required on the @f@ type. '[]' might not work so well,
-- and we'll look at /why/ in the next part of this question!

instance Wanderable Applicative Maybe where
  wander f (Just a) = Just <$> f a
  wander _ Nothing = pure Nothing

instance Wanderable Applicative Proxy where
  wander _ _ = pure Proxy

-- GHC can decide which c1 use for the recursive wander.
--instance Wanderable Applicative [] where
  --wander f []       = pure []
  --wander f (a : as) = liftA2 (:) (f a) (wander f as)

-- | d. Assuming you turned on the extension suggested by GHC, why does the
-- following produce an error? Using only the extensions we've seen so far, how
-- could we solve this, perhaps in a way that involves another parameter to the
-- 'wander' function? A parameter whose type could be annotated? (Don't worry -
-- we'll see in later chapters that there are neater solutions to this
-- problem!)

class Wanderable' (c :: (Type -> Type) -> Constraint) (t :: Type -> Type) where
 wander' :: c f => Proxy c -> (a -> f b) -> t a -> f (t b)

instance Wanderable' Applicative [] where
  wander' _ f []       = pure []
  wander' p f (a : as) = liftA2 (:) (f a) (wander' p f as)

-- UNCOMMENT
--test = wander' (Proxy :: Proxy Applicative) Just [1, 2, 3]





{- THREE -}

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

-- | In the @DataKinds@ chapter, we wrote the 'SmallerThan' data type, which
-- we'll call 'Fin' from now on:

data Fin (limit :: Nat) where
  FZ ::          Fin ('S n)
  FS :: Fin n -> Fin ('S n)

-- | We can write a class to take an 'SNat' to a 'Fin' using
-- @MultiParamTypeClasses@. We can even use @TypeOperators@ to give our class a
-- more intuitive name:

class (x :: Nat) < (y :: Nat) where
  convert :: SNat x -> Fin y

-- | a. Write the instance that says @Z@ is smaller than @S n@ for /any/ @n@.
instance 'Z < ('S n) where
  convert SZ = FZ

-- | b. Write an instance that says, if @x@ is smaller than @y@, then @S x@ is
-- smaller than @S y@.
instance (n < n') => ('S n) < ('S n') where
  convert (SS n) = FS (convert n)

-- | c. Write the inverse function for the class definition and its two
-- instances.
-- TODO: not sure what to do here.
unconvert
  :: x < y
  => Fin y
  -> SNat x
unconvert = undefined




{- FOUR -}

-- | In a couple places, we've seen the @(~)@ (or "equality") constraint being
-- used. Essentially, we can think of it as a two-parameter typeclass with one
-- instance.

-- | a. Write that typeclass!
--UNCOMMENT
--class a ~~ b where

-- | b. Write that instance!
--UNCOMMENT
--instance a ~~ a where

-- | c. When GHC sees @x ~ y@, it can apply anything it knows about @x@ to @y@,
-- and vice versa. We don't have the same luxury with /our/ class, however –
-- because we can't convince the compiler that only one instance will ever
-- exist, it can't assume that we want the instance we've just written. No
-- matter, though - we can just add two functions (@x -> y@ and @y -> x@) to
-- our class to convert between the types. Write them, and don't overthink!
class a ~~ b where
  to  :: b -> a
  from :: a -> b

instance {-# OVERLAPPABLE #-} a ~~ a where
  to = Prelude.id
  from = Prelude.id

-- | d. GHC can see @x ~ y@ and @y ~ z@, then deduce that @x ~ z@. Can we do
-- the same? Perhaps with a second instance? Which pragma(s) do we need and
-- why? Can we even solve this?

-- TODO: GHC is not ok with this..
--instance {-# OVERLAPPING #-} (a ~~ b, b ~~ c) => a ~~ c where
  --to = to . to
  --from = from . from




{- FIVE -}

-- | It wouldn't be a proper chapter without an @HList@, would it?

data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)


-- | Consider the following class for taking the given number of elements from
-- the front of an HList:

class HTake (n :: Nat) (xs :: [Type]) (ys :: [Type]) where
  htake :: SNat n -> HList xs -> HList ys

-- | a. Write an instance for taking 0 elements.

instance HTake 'Z xs '[] where
  htake SZ _ = HNil

-- | b. Write an instance for taking a non-zero number. You "may" need a
-- constraint on this instance.

instance HTake n xs ys => HTake ('S n) (x ': xs) (x ': ys) where
  htake (SS n) (HCons x xs) = HCons x (htake n xs)

-- | c. What case have we forgotten? How might we handle it?

instance HTake n '[] '[] where
  htake _ HNil = HNil





{- SIX -}

-- | We could also imagine a type class to "pluck" types out of @HList@:

class Pluck (x :: Type) (xs :: [Type]) where
  pluck :: HList xs -> x

-- | a. Write an instance for when the head of @xs@ is equal to @x@.

instance Pluck x (x ': xs) where
  pluck (HCons x _) = x

-- | b. Write an instance for when the head /isn't/ equal to @x@.

-- NOTE: OVERLAPPING is sufficient for this exercise, but I made it INCOHERENT
-- to get a better error for part (d). Check the instance resolution rules
-- again to see why this made the error better!
instance {-# INCOHERENT #-} Pluck x xs => Pluck x (y ': xs) where
  pluck (HCons _ xs) = pluck xs

-- | c. Using [the documentation for user-defined type
-- errors](http://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-TypeLits.html#g:4)
-- as a guide, write a custom error message to show when you've recursed
-- through the entire @xs@ list (or started with an empty @HList@) and haven't
-- found the type you're trying to find.

instance TypeError
    ( 'Text "Couldn't find " ':<>: 'ShowType x ':<>: 'Text " here!" )
    => Pluck x '[] where
  pluck = undefined

-- | d. Making any changes required for your particular HList syntax, why
-- doesn't the following work? Hint: try running @:t 3@ in GHCi.

-- The type of @3@ isn't @Int@ - it's more polymorphic than that.
-- mystery :: Int
-- mystery = pluck (HCons 3 HNil)





{- SEVEN -}

-- | A variant is similar to an 'Either', but generalised to any non-zero
-- number of parameters. Typically, we define it with two parameters: @Here@
-- and @There@. These tell us which "position" our value inhabits:

variants :: [Variant '[Bool, Int, String]]
variants = [ Here True, There (Here 3), There (There (Here "hello")) ]

-- | a. Write the 'Variant' type to make the above example compile.

data Variant (xs :: [Type]) where
  Here  ::         x  -> Variant (x ': xs)
  There :: Variant xs -> Variant (y ': xs)

-- | b. The example is /fine/, but there's a lot of 'Here'/'There' boilerplate.
-- Wouldn't it be nice if we had a function that takes a type, and then returns
-- you the value in the right position? Write it! If it works, the following
-- should compile: @[inject True, inject (3 :: Int), inject "hello"]@.

class Inject (x :: Type) (xs :: [Type]) where
  inject :: x -> Variant xs

instance Inject x (x ': xs) where
  inject = Here

instance {-# INCOHERENT #-} Inject x xs
    => Inject x (y ': xs) where
  inject = There . inject

test :: [ Variant '[Bool, Int, String] ]
test = [ inject True, inject (3 :: Int), inject "hello" ]

-- | c. Why did we have to annotate the 3? This is getting frustrating... do
-- you have any (not necessarily good) ideas on how we /could/ solve it?

-- We could do a fallback instance that tries to unify any type variables with
-- @Int@? There's honestly not much we can do here. :(





{- EIGHT -}

-- | As engineers, we are wont to over-think day-to-day problems in order to
-- justify our existence to scrum masters. As such, we are compelled to visit
-- our friendly neighbourhood angel investor with a new idea: given the weather
-- and rough temperature, our web2.0, blockchain-ready app - chil.ly - will
-- tell you whether or not you need a coat. Let's start by defining our inputs:

data Weather     = Sunny | Raining
data Temperature = Hot   | Cold

-- ... and some singletons, why not?

data SWeather (w :: Weather) where
  SSunny   :: SWeather 'Sunny
  SRaining :: SWeather 'Raining

data STemperature (t :: Temperature) where
  SHot  :: STemperature 'Hot
  SCold :: STemperature 'Cold

-- | Now, our app is going to be ready-for-scale, B2B, and proven with zero
-- knowledge, so we want type safety /at the core/. Naturally, we've defined
-- the relationship between the two domains as a type class.

class Coat (a :: Weather) (b :: Temperature) where
  doINeedACoat :: SWeather a -> STemperature b -> Bool

-- | It's early days, and we're just building an MVP, but there are some rules
-- that /everyone/ knows, so they should be safe enough!

-- No one needs a coat when it's sunny!
instance {-# INCOHERENT #-} Coat Sunny b where doINeedACoat _ _ = False

-- It's freezing out there - put a coat on!
instance Coat a Cold where doINeedACoat _ _ = True

-- | Several months pass, and your app is used by billions of people around the
-- world. All of a sudden, your engineers encounter a strange error:

test' :: Bool
test' = doINeedACoat SSunny SCold

-- | Clearly, our data scientists never thought of a day that could
-- simultaneously be sunny /and/ cold. After months of board meetings, a
-- decision is made: you /should/ wear a coat on such a day. Thus, the
-- __second__ rule is a higher priority.

-- | a. Uncomment the above, and add OVERLAPPING and/or OVERLAPPABLE pragmas
-- to prioritise the second rule. Why didn't that work? Which step of the
-- instance resolution process is causing the failure?

-- | b. Consulting the instance resolution steps, which pragma /could/ we use
-- to solve this problem? Fix the problem accordingly.

-- | c. In spite of its scary name, can we verify that our use of it /is/
-- undeserving of the first two letters of its name?

-- Yes! There's only one INCOHERENT instance, so the choice will never be
-- non-deterministic.





{- NINE -}

-- | The 'Show' typeclass has two instances with which we're probably quite
-- familiar:

-- instance Show a => Show [a]
-- instance           Show String

-- | a. Are these in conflict? When?

-- They overlap! @String ~ [Char]@, so any mention of showing a string /could/
-- upset GHC.

-- | b. Let's say we want to define an instance for any @f a@ where the @f@ is
-- 'Foldable', by converting our type to a list and then showing that. Is there
-- a pragma we can add to the first 'Show' instance above so as to preserve
-- current behaviour? Would we need /more/ pragmas than this?

-- OVERLAPPABLE would do it for the GHC case. In our specific contrived case,
-- we might be better off adding OVERLAPS to all of them, so we simply pick the
-- most specific instance available at the time.

-- | c. Somewhat confusingly, we've now introduced incoherence: depending on
-- whether or not I've imported this module, 'show' will behave in different
-- ways. Your colleague suggests that your use of pragmas is the root issue
-- here, but they are missing the bigger issue; what have we done? How could we
-- have avoided it?

-- @f a@ is almost certainly an orphan instance! We'd have been better off
-- creating something like @newtype ShowFoldable f a = SF (f a)@ and writing a
-- Show instance for that.





{- TEN -}

-- | Let's imagine we have some types in our codebase:

newtype UserId = UserId Int

data User
  = User
      { id      :: UserId
      , knownAs :: String
      }

newtype CommentId = CommentId Int

data Comment
  = Comment
      { id     :: CommentId
      , author :: UserId
      , text   :: String
      }

data Status = Blocked | Deleted

-- | In order to better facilitate mobile devices, we now want to introduce
-- caching. I start work, and eventually slide a pull request into your DMs:

class UserCache where
  storeUser :: User -> Map UserId User -> Map UserId User
  loadUser :: Map UserId User -> UserId -> Either Status User

class CommentCache where
  storeComment :: Comment -> Map CommentId Comment -> Map CommentId Comment
  loadComment  :: Map CommentId Comment -> CommentId -> Maybe Comment

-- | "This is silly", you exclaim. "These classes only differ in three ways! We
-- could write this as a multi-parameter type class!"

-- | a. What are those three ways? Could we turn them into parameters to a
-- typeclass? Do it!

class Cache (value :: Type) (id :: Type) (f :: Type -> Type) where
  store :: value -> Map id value -> Map id value
  load :: Map id value -> id -> f value

instance (value ~ User, f ~ Either Status) => Cache value UserId f where
  store = undefined
  load = undefined

instance (value ~ Comment, f ~ Maybe) => Cache value CommentId f where
  store = undefined
  load = undefined

-- | b. Write instances for 'User' and 'Comment', and feel free to implement
-- them as 'undefined' or 'error'. Now, before uncommenting the following, can
-- you see what will go wrong?

-- We don't know which instance to pick based solely on the fact that it's a
-- UserId!
oops cache = load cache (UserId (123 :: Int))

-- | c. Do we know of a sneaky trick that would allow us to fix this? Possibly
-- involving constraints? Try!
