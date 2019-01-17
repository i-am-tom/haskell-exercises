{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Exercises where

import Data.Kind    (Constraint, Type)
import GHC.TypeLits (Symbol)





{- ONE -}

-- | Let's look at the following type family to build a constraint:

type family All (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- | a. Why does it have to be restricted to 'Type'? Can you make this more
-- general?

-- | b. Why does it have to be restricted to 'Constraint'? Can you make this
-- more general? Why is this harder?





{- TWO -}

-- | Sometimes, we're working with a lot of values, and we'd like some way of
-- identifying them for the compiler. We could make newtypes, sure, but maybe
-- there are loads, and we just don't want the boilerplate. Luckily for us, we
-- can introduce this new type:

data Tagged (name :: Symbol) (a :: Type)
  = Tagged { runTagged :: a }

-- | 'Tagged' is just like 'Identity', except that it has a type-level string
-- attached to it. This means we can write things like this:

x :: Tagged "Important" Int
x = Tagged 42

y :: Tagged "Not so important" Int
y = Tagged 23

-- | What's more, we can use that tag in function signatures to restrict the
-- input:

f :: Tagged "Important" Int -> IO ()
f (Tagged x) = putStrLn (show x <> " is important!")

-- | a. Symbols are all well and good, but wouldn't it be nicer if we could
-- generalise this?

-- | b. Can we generalise 'Type'? If so, how? If not, why not?

-- | c. Often when we use the 'Tagged' type, we prefer a sum type (promoted
-- with @DataKinds@) over strings. Why do you think this might be?





{- THREE -}

-- | We can use the following to test type-level equivalence.

data a :=: b where
  Refl :: a :=: a

-- | a. What do you think the kind of (:=:) is?

-- | b. Does @PolyKinds@ make a difference to this kind?

-- | c. Regardless of your answer to part (b), is this the most general kind we
-- could possibly give this constructor? If not (hint: it's not), what more
-- general kind could we give it, and how would we tell this to GHC?





{- FOUR -}

-- | We've talked about singleton types previously, and how they allow us to
-- share a value between the value and type levels. Libraries such as
-- @singletons@ provide many utilities for working with these types, many of
-- which rely on a (type-level) function to get from a kind to its singleton.
-- In our toy version of the library, that type family may look like this:

type family Sing (x :: k) :: Type

-- | Notice that it's an /open/ type family, thus we define instances for it
-- using the @type instance Sing x = y@ syntax.

-- | a. Here's the singleton for the @Bool@ kind. Remembering that the
-- singleton for some @x@ of kind @Bool@ is @SBool x@, write a @Sing@ instance
-- for the @Bool@ kind. Remember that we can pattern-match on /kinds/ in type
-- families - if you're on the right lines, this is a one-liner!

data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- type instance Sing ...

-- | b. Repeat the process for the @Nat@ kind. Again, if you're on the right
-- lines, this is very nearly a copy-paste job!

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)





{- FIVE -}

-- | In dependently-typed programming, we talk about the notion of a "Sigma
-- type", or "dependent pair". This is a single-constructor GADT that takes two
-- arguments: a singleton for some kind, and some type indexed by the type
-- represented by the singleton. Don't panic, let's do an example:

-- | Consider the following GADT to describe a (fixed-length) list of strings:

data Strings (n :: Nat) where
  SNil ::                        Strings  'Z
  (:>) :: String -> Strings n -> Strings ('S n)

-- | If we have a working sigma type, we should be able to package a @Strings
-- n@ and an @SNat n@ into @Sigma Strings@, existentialising the actual length.
--
-- @
--   example :: [Sigma Strings]
--   example
--     = [ Sigma         SZ   SNil
--       , Sigma     (SS SZ)  ("hi" :> SNil)
--       , Sigma (SS (SS SZ)) ("hello" :> ("world" :> SNil))
--       ]
-- @

-- | a. Write this type's definition: If you run the above example, the
-- compiler should do a lot of the work for you...

data Sigma (f :: Nat -> Type) where
  -- Sigma :: ... -> Sigma f

-- | b. Surely, by now, you've guessed this question? Why are we restricting
-- ourselves to 'Nat'? Don't we have some more general way to talk about
-- singletons? The family of singletons? Any type within the family of
-- singletons? Sing it with me! Generalise that type!

-- | c. In exercise 5, we wrote a 'filter' function for 'Vector'. Could we
-- rewrite this with a sigma type, perhaps?

data Vector (a :: Type) (n :: Nat) where -- @n@ and @a@ flipped... Hmm, a clue!
  VNil  ::                    Vector a  'Z
  VCons :: a -> Vector a n -> Vector a ('S n)






{- SIX -}

-- | Our sigma type is actually very useful. Let's imagine we're looking at
-- a communication protocol over some network, and we label our packets as
-- @Client@ or @Server@:

data Label = Client | Server

-- | Client data and server data are different, however:

data ClientData
  = ClientData
      { name         :: String
      , favouriteInt :: Int
      }

data ServerData
  = ServerData
      { favouriteBool     :: Bool
      , complimentaryUnit :: ()
      }

-- | a. Write a GADT indexed by the label that holds /either/ client data or
-- server data.

data Communication (label :: Label) where
  -- {{Fill this space with your academic excellence}}

-- | b. Write a singleton for 'Label'.

-- | c. Magically, we can now group together blocks of data with differing
-- labels using @Sigma Communication@, and then pattern-match on the 'Sigma'
-- constructor to find out which packet we have! Try it:

-- serverLog :: [Sigma Communication] -> [ServerData]
-- serverLog = error "YOU CAN DO IT"

-- | d. Arguably, in this case, the Sigma type is overkill; what could we have
-- done, perhaps using methods from previous chapters, to "hide" the label
-- until we pattern-matched?
