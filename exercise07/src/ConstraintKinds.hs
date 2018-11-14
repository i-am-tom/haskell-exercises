{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- Two of the -} module {- topics I found particularly difficult to place were
-} ConstraintKinds {- and @PolyKinds@. I think as we explore concepts like
MultiParamTypeClasses, it will become obvious why it's hard to say exactly
-} where {- it belongs. Anyway, all that for later; let's go! -}

import Data.Kind (Constraint, Type)

{-
  In Haskell, we are hopefully all familiar with the idea of type parameters.
  For example, the @Maybe@ type has one, @a@:
-}

data Maybe a
  = Nothing
  | Just a

{-
  We also saw in the earlier chapters that we can use kind signatures to
  parameterise different kinds:
-}

data TTProxy (x :: Type -> Type)
  = TTProxy

eg0 :: TTProxy []
eg0 = TTProxy

eg1 :: TTProxy IO
eg1 = TTProxy

data TTTProxy (x :: Type -> Type -> Type)
  = TTTProxy

eg2 :: TTTProxy Either
eg2 = TTTProxy

eg3 :: TTTProxy (->)
eg3 = TTTProxy

{-
  We even saw that we could parameterise over constraints!
-}

data CProxy (x :: Constraint)
  = CProxy

eg4 :: CProxy (Eq Int)
eg4 = CProxy

data TCProxy (x :: Type -> Constraint)
  = TCProxy

eg5 :: TCProxy Eq
eg5 = TCProxy

{-
  Let's take this one step further. If we think about our original
  understanding of GADTs, we said that they allowed us to embed constraints
  within our data constructors. In that case, why can't we use a constraint
  parameter /as/ a constraint?

  This, dear reader, is the magic of ConstraintKinds!
-}

data HasConstraint (c :: Type -> Constraint) where
  Item :: c x => x -> HasConstraint c

{-
  So, what's going on here? We've said that our type, 'HasConstraint', will be
  parameterised by some constraint @c@. We've said that, in order to construct
  a @HasConstraint c@, we must satisfy @c x@. The exciting thing here is that
  our constraint is polymorphic!

  Let's look at another example: @Dict@.
-}

data Dict (c :: Constraint) where
  Dict :: c => Dict c

{-
  Dict is a constraint as a data type. True to the magic of a GADT,
  pattern-matching on a @Dict@ will bring its contents into scope. This allows
  us to write functions like this:
-}

eq :: Dict (Eq a) -> a -> a -> Bool
eq Dict x y = x == y

{-
  Perhaps interestingly, the following will fail to compile. This is because we
  haven't pattern-matched! Although there's only one possible value, we still
  need to pattern-match in order to bring the constraint evidence into scope.
-}

-- eq :: Dict (Eq a) -> a -> a -> Bool
-- eq notPatternMatched x y = x == y
