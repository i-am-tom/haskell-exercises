{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- Our -} module {- du jour is strangely-abbreviated and weirdly-capitalised:
the -} MultiParamTypeClasses {- extension opens a lot of doors for us that
we'll explore in this chapter /and/ future chapters - Let's see -} where {- we
end up! -}

import Data.Kind (Type)

{-
  By now in your Haskell quest, you'll have seen many type classes. Already,
  the `README` of this project has mentioned two such classes: `Monoid` and
  `Monad`. Add these to `Eq`, `Ord`, `Show`, `Functor`... well, we're using a
  lot of typeclasses!

  The common thread between these is that the class defines a characteristic of
  a **single** type. 'Eq' tells us that /a type/ has a notion of equivalence.
  'Show' tells us that /a type/ has a string representation. 

  Let's think about the rough idea behind the 'Show' class:
-}

class SimpleShow (x :: Type) where
  simpleShow :: x -> String

{-
  Our class represents a type that can be transformed into a string. What if we
  want to generalise the "target" type, though? What if we want to define a
  class that characterises this relationship? Enter @MultiParamTypeClasses@.
-}

class Transform (source :: Type) (target :: Type) where
  transform :: source -> target

{-
  We've said here that the 'Transform' class defines a relationship between two
  types for which we can implement the 'transform' function. This function has
  a type that is hopefully not too surprising: @Transform s t => s -> t@. Now,
  how do we write instances?
-}

instance Transform Bool Int where
  transform x = if x then 1 else 0

{-
  Perfect! We write it just as we would a normal instance, except there are
  more types in the instance head. We can even use type parameters in our
  instance heads (note that using 'String' here means we need
  'FlexibleInstances' - the same rules apply, regardless of how many parameters
  you have):
-}

instance Show x => Transform x String where
  transform = show

{-
  The @MultiParamTypeClasses@ extension also allows us to have /nullary/
  (0-argument) classes, though we won't give them much time as they're
  relatively straightforward. PureScript has a type class called
  [Partial](https://github.com/purescript/documentation/blob/master/guides/The-Partial-type-class.md)
  to keep track of partial functions, which gives a good intuition of the ways
  in which these can be useful.
-}

{-
  Let's talk briefly about what happens when we have conflicting definitions.
  This isn't specific to @MultiParamTypeClasses@, and all the following rules
  apply to single-parameter type classes as well, though we're certainly more
  likely to run into these use cases in multi-parameter situations. Let's
  imagine, for example, that we want a special string representation of the
  unit type, '()'.
-}

instance Transform () String where
  transform _ = "UNIT"

{-
  All good, but we get some ugly errors when we try to use it:
-}

-- test :: String
-- test = transform ()

{-
  • Overlapping instances for Transform () String
      arising from a use of ‘transform’
    Matching instances:
      instance Show x => Transform x String
        -- Defined at src/MultiParamTypeClasses.hs:55:10
      instance Transform () String
        -- Defined at src/MultiParamTypeClasses.hs:73:10

  GHC has correctly realised that we actually have __two__ instances that match
  our needs. Because of this ambiguity, it gives us an error - it simply
  refuses to guess. How, then, would we solve this ambiguity? Luckily, GHC
  provides some language pragmas to help here - let's write a new instance:
-}

instance {-# OVERLAPPING #-} Transform String String where
  transform = id

test2 :: String
test2 = transform "hello"

{-
  @OVERLAPPING@ is how we tell GHC that, given the choice between this instance
  and another, this is the "preferred" option, and should be picked. The
  opposite pragma is @OVERLAPPABLE@, which says "pick the other one".
  Fortunately, this is an either/or deal - when we have a choice between two
  instances, GHC needs to find an @OVERLAPPING@ /or/ and @OVERLAPPABLE@
  instance to make a decision. Both are fine as long as they agree, though!

  There's a third, more mystical pragma, however: @OVERLAPS@. Let's have a look
  at such an instance now:
-}

data Which = A | B | C deriving Show

instance {-# OVERLAPS #-} Transform (f a) Which where
  transform _ = A

{-
  Now, let's introduce a couple of others:
-}

instance Transform a Which where
  transform _ = B

instance Transform (Maybe a) Which where
  transform _ = C

{-
  ... and let's use them!
-}

test3 :: Which
test3 = transform (Just (42 :: Int)) -- Prints C

test4 :: Which
test4 = transform [True] -- Prints A

{-
  Interesting... so, in the first case, we have two matching instances - the
  @A@ instance and the @C@ instance. @Maybe a@ is deemed more "specific" than
  @f a@, and so @OVERLAPS@ acts like @OVERLAPPABLE@, and we get the @C@
  instance!

  In the second case, however, our @[Bool]@ matches both the @A@ instance and
  the @B@ instance. This time, @f a@ is more "specific" than @a@, so @OVERLAPS@
  acts like @OVERLAPPING@!

  @OVERLAPS@ is a strange pragma, and you won't see it used too much in the
  wild. Furthermore, "specific" is a strange thing to intuit, but we can think
  about it in GHC's terms: if @x@ is a valid substitution of @y@, but @y@ isn't
  a valid substitution for @x@, then @x@ is more "specific". For example,
  @Maybe Int@ would be a valid substitution for @Maybe a@, but not vice versa,
  so we consider @Maybe Int@ to be __more specific__. I hope that makes
  sense...
-}

{-
  There is one last oft-misunderstood pragma to discuss: INCOHERENT. To
  understand this, we'll paraphrase [a GHC user guide
  entry](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-class-extensions.html#instance-overlap)
  and step through GHC's process when picking an instance.

  1. Find all instances that match given what we know already* (those into
     which we could "substitute" our type).

  2. Discard any less-specific overlappable instances (or those not /as/
     specific as an overlapping instance).

  3. Ignoring any instances marked @INCOHERENT@, if exactly one match remains,
     we have a winner! If all remaining instances /are/ marked @INCOHERENT@,
     pick one of them at random, and we have a winner! (Not really random, but
     the choice isn't done according to some defined behaviour).

  4. If our winner is incoherent, we're done!
  
  5. If not, find all the instances that /could/ match if we knew more*. If all
     these other instances are incoherent, return our winner! Otherwise, we've
     failed, and we don't know enough to decide :(

  * Knowing enough is important here. Let's clean the `Transform` slate and
  look at this:
-}

class Sneaky (source :: Type) (target :: Type) where
  familiar :: source -> target

instance Sneaky Float Float where
  familiar = id

{-
  Now, let's use `familiar` in a situation without giving GHC a way to figure
  out the second type:
-}

-- huh = familiar (3.0 :: Float)

{-
  As we're starting to learn, GHC is actually pretty good at telling us what is
  upsetting it.

  • Ambiguous type variable ‘target0’ arising from a use of ‘familiar’
    prevents the constraint ‘(Sneaky Float target0)’ from being solved.

  GHC is telling us that, until it knows what the _second_ type is, it can't
  figure out which instance to pick! Why? Well, for a start, we've just seen
  @instance Transform a Which@, which might well be what we're after! Until GHC
  knows more, it can't sensibly choose one or the other. Of course, there are a
  few tricks to play here.
-}

instance a ~ [Double] => Sneaky Double a where
  familiar = pure

heh = familiar (3.0 :: Double)

{-
  This time, no complaints! Why? Because in the instance head (note: this
  __does not include constraints__), we've said we have an instance for
  @Double@ and /any/ second parameter you like. After we've successfully chosen
  that instance, the constraints /then/ tell us that the second type /must/ be
  @Double@, and we'll get an error if it isn't. This separation between
  "instance resolution" and "constraint solving" is one worth exploring, as
  many of our clever tricks rely on it heavily.

  Now, some of our types might have interesting instances for 'Sneaky', but
  we'd like to offer 'Sneaky a a' as a "default" instance. In other words, we
  only want to use this instance __if all others fail__. Sound familiar?
-}

instance {-# INCOHERENT #-} a ~ b => Sneaky a b where
  familiar = id

{-
  Here's a scary-looking line of code, right? What we're saying here is, if you
  can't find an instance to suit your needs, this "fallback" instance will
  work. The only constraint is that your @a@ and @b@ types must be the same
  type. Because it's marked as @INCOHERENT@, we'll totally ignore it /unless/
  we have no other option. Perfect!

  Now, this all sounds sensible, so... why the scary name? Friends, we have
  some misconceptions to address:

  1. Most "incoherence" around type classes comes from what we call /orphan
     instances/. These are instances defined for types that exist neither in
     the same module as the type declaration /nor/ the class declaration. If
     we avoid orphan instances (by, for example, creating @newtype@s when we
     want different behaviours instead of overlapping existing instances),
     there is no incoherence with @OVERLAPPING@, @OVERLAPS@, or @OVERLAPPABLE@,
     thus they are simply safe – and extremely useful – tools for us to use.
  
  2. @INCOHERENT@ instances only become incoherent when GHC is given a choice
     between more than one of them. If you only ever have one defined - or find
     a clever way to avoid ever matching more than one* - then this is a very
     scary name for a not-very-scary idea. Just... be careful with this one,
     OK? If you can convince yourself that, following the steps of instance
     resolution above, GHC's selection will always be predictable, then this is
     all good. /Be careful/.

  * https://kcsongor.github.io/generic-deriving-bifunctor/#incoherent-instances
-}
