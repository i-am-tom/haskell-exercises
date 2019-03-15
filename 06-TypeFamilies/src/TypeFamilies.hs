{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{- Today's -} module {- will be a very brief introduction to -} TypeFamilies {-
and their uses. We're not going to go -} where {- Csongor's talk went, though
there is plenty of further reading to be done if you're interested! -}

import Data.Kind (Type)

data Nat = Z | S Nat

{-
  We're hopefully all aware of value-level functions:
-}

add :: Nat -> Nat -> Nat
add  Z    y = y
add (S x) y = S (add x y)

{-
  Type families, by their simplest definition, are functions at the type-level.
  In case you're wondering, the @TypeFamilies@ extension implies the
  @KindSignatures@ extension:
-}

type family Add (x :: Nat) (y :: Nat) :: Nat where
  Add  'Z    y = y
  Add ('S x) y = 'S (Add x y)

{-
  The syntax is a bit funny, but hopefully the symmetry is obvious: we specify
  the parameters in the first line, as well as the return type, and then we
  write "equations" on the subsequent lines. Technically, this is a /closed/
  type family: we specify all equations up front, like a value-level function.

  There are also open type families, which behave more like type classes, but
  we won't pay them too much attention:
-}

type family Open (x :: Type) :: Type

type instance Open Int = String

-- ... Loads more code and whatnot, maybe other files ...

type instance Open (Maybe Bool) = IO Int

{-
  So, why would this be useful? Well, we'll save the fun examples for the
  exercises, as most of these are pretty intuitive, but let's pick a nice, easy
  one. A while ago, we talked about a singleton boolean type:
-}

data SBool (value :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

{-
  What if we wanted to "flip" the boolean value? What would the type signature
  be? As well as the value-level "not", we'd need a type-level equivalent to
  match! We can define this quite neatly as a type family:
-}

type family Not (input :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

{-
  Now, we can write the function and its signature!
-}

not :: SBool input -> SBool (Not input)
not STrue  = SFalse
not SFalse = STrue

{-
  The @input@ type determines what the output type will be, and the type family
  will be evaluated for each pattern-match in our function. That's all there is
  to it!

  One last thing that may be of interest: type families don't necessarily have
  to be total.
-}

type family Subtract (x :: Nat) (y :: Nat) :: Nat where
  Subtract     x   'Z    = x
  Subtract ('S x) ('S y) = Subtract x y

{-
  'Subtract' subtracts the second value from the first. However, it doesn't
  deal with the posibility that the second value is /bigger/ than the first,
  because we don't have a way of representing negative numbers! So, what
  happens when we get it wrong? Let's load up ghci with @cabal repl@:

  > :set -XDataKinds
  > :kind! Subtract 'Z ('S 'Z)
  Subtract 'Z ('S 'Z) :: Nat
  = Subtract 'Z ('S 'Z)

  This is the first time we've played with ghci, so notice a couple things:

  - The @:set -X...@ syntax is how we turn on language extentsions within GHCi.

  - The @:kind ...@ command will tell us the kind of an extension, but the
    @:kind! ...@ command will tell us the normalised (simplified) form of the
    type. In our case, that means evaluating type families as far as we can.

  So, what happened? Well, if a we can't find an equation that matches our call
  to a type family, we can't reduce the expression, and it stays how it is.
  This is in part due to things like open type families - we might work out how
  to reduce the type family later on, so we can't definitely say it's an
  /error/...

  In this particular case, we say our expression is "stuck". We're not going to
  go into any further detail about what this means, but talk to Csongor if
  you're interested in the ways you can misuse this!
-}
