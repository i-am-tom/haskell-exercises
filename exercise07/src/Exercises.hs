{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where -- ^ This is starting to look impressive, right?

import Data.Kind (Constraint, Type)

-- | Just a quick one today - there really isn't much to cover when we talk
-- about ConstraintKinds, as it's hopefully quite an intuitive extension: we're
-- just extending the set of things that we can use as constraints to include
-- type parameters!





{- ONE -}

-- | Here's a super uninteresting list, which comes with an unfortunate
-- restriction: everything in the list must have the same exact type.

data List a = Nil | Cons a (List a)

-- | We can generalise this data structure to a /constrained list/, in which we
-- say, instead of "every value has this type", we say, "every value's type
-- satisfies this constraint".

-- | a. Do it! Think about the @Nil@ and @Cons@ cases separately; which
-- constraints can the @Nil@ case satisfy?

data ConstrainedList (c :: Type -> Constraint) where
  CNil :: ConstrainedList c
  CCons :: c x => x -> ConstrainedList c -> ConstrainedList c

-- | b. Using what we know about RankNTypes, write a function to fold a
-- constrained list. Note that we'll need a folding function that works /for
-- all/ types who implement some constraint @c@. Wink wink, nudge nudge.

foldConstrainedList
  :: Monoid m
  => (forall x. c x => x -> m)
  -> ConstrainedList c
  -> m

foldConstrainedList f  CNil        = mempty
foldConstrainedList f (CCons x xs) = f x <> foldConstrainedList f xs

-- | Often, I'll want to constrain a list by /multiple/ things. The problem is
-- that I can't directly write multiple constraints into my type, because the
-- kind of @(Eq, Ord)@ isn't @Type -> Constraint@ - it's actually a kind error!

-- | There is hope, however: a neat trick we can play is to define a new class,
-- whose super classes are all the constraints we require. We can then write an
-- instance for any a who satisfies these constraints. Neat, right?

-- | c. Write this class instance so that we can have a constraint that
-- combines `Monoid a` and `Show a`. What other extension did you need to
-- enable? Why?

-- UndecidableInstances is required because nothing is getting "smaller" - when
-- GHC encounters this constraint, it ends up with more things to solve! This
-- means that GHC's (limited) termination checker can't be convinced that we'll
-- ever terminate.
class (Monoid a, Show a) => Constraints a
instance (Monoid a, Show a) => Constraints a

-- | What can we now do with this constrained list that we couldn't before?
-- There are two opportunities that should stand out!

-- We can show everything in it, and we can fill it with 'mempty'. We can't
-- write a monoid instance, because we have no way of telling that two
-- constrained lists contain the /same/ monoids. :(





{- TWO -}

-- | Recall our HList:

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Ideally, we'd like to be able to fold over this list in some way, ideally
-- by transforming every element into a given monoid according to the interface
-- of some constraint. To do that, though, we'd need to know that everything in
-- the list implemented a given constraint... if only we had a type family for
-- this...

-- | a. Write this fold function. I won't give any hints to the definition, but
-- we will probably need to call it like this:

type family Every (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  Every c '[] = ()
  Every c (x ': xs) = (c x, Every c xs)

data TCProxy (c :: Type -> Constraint)
  = TCProxy

fold
  :: (Every c xs, Monoid m)
  => TCProxy c
  -> (forall x. c x => x -> m)
  -> HList xs
  -> m

fold _ f  HNil        = mempty
fold p f (HCons x xs) = f x <> fold p f xs

test :: Every Show xs => HList xs -> String
test = fold (TCProxy :: TCProxy Show) show

-- | b. Why do we need the proxy to point out which constraint we're working
-- with?  What does GHC not like if we remove it?

-- The type is ambiguous! We've no way of telling it what the constraint /is/.
-- Even if we give it a function like 'show', GHC's not going to /guess/ that
-- we care about the 'Show' constraint, when the empty constraint would also
-- fit the gap!

-- | We typically define foldMap like this:

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f = foldr (\x acc -> f x <> acc) mempty

-- | c. What constraint do we need in order to use our @(a -> m)@ function on
-- an @HList@? You may need to look into the __equality constraint__ introduced
-- by the @GADTs@ and @TypeFamilies@ extensions, written as @(~)@:

    -- * This tells GHC that @a@ and @b@ are equivalent.
f :: a ~ b => a -> b
f = id

-- | Write @foldMap@ for @HList@!

foldMapH :: (Monoid m, Every ((~) a) xs) => (a -> m) -> HList xs -> m
foldMapH f  HNil        = mempty
foldMapH f (HCons x xs) = f x <> foldMapH f xs
