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
module Exercises where

import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Proxy (Proxy (..))





{- ONE -}

-- | Consider the following types:

newtype MyInt   = MyInt   Int
newtype YourInt = YourInt Int

-- | As Haskell programmers, we love newtypes, so it would be super useful if
-- we could define a class that relates a newtype to the type it wraps, while
-- also giving us functions to get between them (we can call them 'wrap' and
-- 'unwrap').

-- | a. Write the class!

-- class Newtype ... ... where
--   wrap   :: ...
--   unwrap :: ...

-- | b. Write instances for 'MyInt' and 'YourInt'.

-- | c. Write a function that adds together two values of the same type,
-- providing that the type is a newtype around some type with a 'Num' instance.

-- | d. We actually don't need @MultiParamTypeClasses@ for this if we use
-- @TypeFamilies@. Look at the section on associated type instances here:
-- https://wiki.haskell.org/GHC/Type_families#Associated_type_instances_2 -
-- rewrite the class using an associated type, @Old@, to indicate the
-- "unwrapped" type. What are the signatures of 'wrap' and 'unwrap'?





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

-- class Wanderable … … where
--   wander :: … => (a -> f b) -> t a -> f (t b)

-- | b. Write a 'Wanderable' instance for 'Identity'.

-- | c. Write 'Wanderable' instances for 'Maybe', '[]', and 'Proxy', noting the
-- differing constraints required on the @f@ type. '[]' might not work so well,
-- and we'll look at /why/ in the next part of this question!

-- | d. Assuming you turned on the extension suggested by GHC, why does the
-- following produce an error? Using only the extensions we've seen so far, how
-- could we solve this, perhaps in a way that involves another parameter to the
-- 'wander' function? A parameter whose type could be annotated? (Don't worry -
-- we'll see in later chapters that there are neater solutions to this
-- problem!)

-- test = wander Just [1, 2, 3]





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

-- | b. Write an instance that says, if @x@ is smaller than @y@, then @S x@ is
-- smaller than @S y@.

-- | c. Write the inverse function for the class definition and its two
-- instances.





{- FOUR -}

-- | In a couple places, we've seen the @(~)@ (or "equality") constraint being
-- used. Essentially, we can think of it as a two-parameter typeclass with one
-- instance.

-- | a. Write that typeclass!

-- | b. Write that instance!

-- | c. When GHC sees @x ~ y@, it can apply anything it knows about @x@ to @y@,
-- and vice versa. We don't have the same luxury with /our/ class, however –
-- because we can't convince the compiler that only one instance will ever
-- exist, it can't assume that we want the instance we've just written. No
-- matter, though - we can just add two functions (@x -> y@ and @y -> x@) to
-- our class to convert between the types. Write them, and don't overthink!

-- | d. GHC can see @x ~ y@ and @y ~ z@, then deduce that @x ~ z@. Can we do
-- the same? Perhaps with a second instance? Which pragma(s) do we need and
-- why? Can we even solve this?





{- FIVE -}

-- | It wouldn't be a proper chapter without an @HList@, would it?

data HList (xs :: [Type]) where
  -- In fact, you know what? You can definitely write an HList by now – I'll
  -- just put my feet up and wait here until you're done!

-- | Consider the following class for taking the given number of elements from
-- the front of an HList:

class HTake (n :: Nat) (xs :: [Type]) (ys :: [Type]) where
  htake :: SNat n -> HList xs -> HList ys

-- | a. Write an instance for taking 0 elements.

-- | b. Write an instance for taking a non-zero number. You "may" need a
-- constraint on this instance.

-- | c. What case have we forgotten? How might we handle it?





{- SIX -}

-- | We could also imagine a type class to "pluck" types out of @HList@:

class Pluck (x :: Type) (xs :: [Type]) where
  pluck :: HList xs -> x

-- | a. Write an instance for when the head of @xs@ is equal to @x@.

-- | b. Write an instance for when the head /isn't/ equal to @x@.

-- | c. Using [the documentation for user-defined type
-- errors](http://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-TypeLits.html#g:4)
-- as a guide, write a custom error message to show when you've recursed
-- through the entire @xs@ list (or started with an empty @HList@) and haven't
-- found the type you're trying to find.

-- | d. Making any changes required for your particular HList syntax, why
-- doesn't the following work? Hint: try running @:t 3@ in GHCi.

-- mystery :: Int
-- mystery = pluck (HCons 3 HNil)





{- SEVEN -}

-- | A variant is similar to an 'Either', but generalised to any non-zero
-- number of parameters. Typically, we define it with two parameters: @Here@
-- and @There@. These tell us which "position" our value inhabits:

-- variants :: [Variant '[Bool, Int, String]]
-- variants = [ Here True, There (Here 3), There (There (Here "hello")) ]

-- | a. Write the 'Variant' type to make the above example compile.

data Variant (xs :: [Type]) where
  -- Here  :: ...
  -- There :: ...

-- | b. The example is /fine/, but there's a lot of 'Here'/'There' boilerplate.
-- Wouldn't it be nice if we had a function that takes a type, and then returns
-- you the value in the right position? Write it! If it works, the following
-- should compile: @[inject True, inject (3 :: Int), inject "hello"]@.

-- class Inject … … where
--   inject :: …

-- | c. Why did we have to annotate the 3? This is getting frustrating... do
-- you have any (not necessarily good) ideas on how we /could/ solve it?





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
instance Coat Sunny b where doINeedACoat _ _ = False

-- It's freezing out there - put a coat on!
instance Coat a Cold where doINeedACoat _ _ = True

-- | Several months pass, and your app is used by billions of people around the
-- world. All of a sudden, your engineers encounter a strange error:

-- test :: Bool
-- test = doINeedACoat SSunny SCold

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





{- NINE -}

-- | The 'Show' typeclass has two instances with which we're probably quite
-- familiar:

-- instance Show a => Show [a]
-- instance           Show String

-- | a. Are these in conflict? When?

-- | b. Let's say we want to define an instance for any @f a@ where the @f@ is
-- 'Foldable', by converting our type to a list and then showing that. Is there
-- a pragma we can add to the first 'Show' instance above so as to preserve
-- current behaviour? Would we need /more/ pragmas than this?

-- | c. Somewhat confusingly, we've now introduced incoherence: depending on
-- whether or not I've imported this module, 'show' will behave in different
-- ways. Your colleague suggests that your use of pragmas is the root issue
-- here, but they are missing the bigger issue; what have we done? How could we
-- have avoided it?





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

-- | b. Write instances for 'User' and 'Comment', and feel free to implement
-- them as 'undefined' or 'error'. Now, before uncommenting the following, can
-- you see what will go wrong? (If you don't see an error, try to call it in
-- GHCi...)

-- oops cache = load cache (UserId (123 :: Int))

-- | c. Do we know of a sneaky trick that would allow us to fix this? Possibly
-- involving constraints? Try!
