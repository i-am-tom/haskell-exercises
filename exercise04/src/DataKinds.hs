{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

{- Today's -} module {- will focus on one of my favourite GHC magic tricks: the
-} DataKinds {- extension. We'll see how and -} where {- to use it to enhance
GADTs in particular, but there are plenty more cases to follow! -}

---

import Data.Kind (Constraint, Type)
import Prelude hiding (zip)

{-
  Now we know about flexible instances, kind signatures, and GADTs, we have a
  pretty solid base to start talking about data kinds. Recall that all types we
  define have kind 'Type':
-}

data Natural = Zero | Successor Natural

{-
  This is a type with two constructors, 'Zero' - which we'll use to represent
  zero - and 'Successor' - which we'll use to represent "one more" than its
  argument, another natural number. Let's write everything out:

  - The 'Natural' type has kind 'Type'.
  - The 'Zero' constructor has type 'Natural'.
  - The 'Successor' constructor has type 'Natural -> Natural'.

  'Successor' takes an argument, another natural, and returns us a natural that
  is one more.

  Recall also that there are other kinds available to us:
-}

class ConstraintKind (a :: Constraint)
instance ConstraintKind (Eq Int)

class TypeToType (a :: Type -> Type)
instance TypeToType Maybe
instance TypeToType (Either Int)

{-
  The @DataKinds@ extension /extends/ the behaviour of data constructors. Now,
  whenever we introduce a data type like 'Natural', we also introduce a new
  kind called 'Natural'. Similarly, we introduce two /type constructors/:

  - 'Zero      :: Natural
  - 'Successor :: Natural -> Natural

  Let's write everything out again.

  - 'Natural' is now a kind.
  - The @'Zero@ constructor has kind 'Natural'.
  - The @'Successor@ constructor has kind 'Natural -> Natural'.

  @'Successor@ is a /type/ of /kind/ @Natural -> Natural@, as it takes another
  type of kind 'Natural' as an argument. We say that this is a /promoted/ data
  type, as it now exists at both the value level and the type level. We use the
  apostrophe (usually called a "tick") before the type to distinguish between
  its value- and type-level versions.
-}

class NaturalKind (a :: Natural)
instance NaturalKind 'Zero
instance NaturalKind ('Successor 'Zero)
instance NaturalKind ('Successor ('Successor 'Zero))

{-
  A thing worth mentioning here is that things of kind 'Natural' /don't have
  any values/. They /only/ exist at the type-level. There is no value that has
  type @'Zero@. With that in mind, you might be forgiven for wondering what the
  point is. The power of data kinds is best demonstrated when we use them for
  /phantom type parameters/ (which, remember, /also/ don't have a value
  attached!):
-}

data Vector (length :: Natural) (a :: Type) where
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Successor n) a

{-
  What we have here is a list, as we have seen before, but with a twist: the
  type of this list (which we'll call a "vector") contains the length of the
  list! Now, why would we want this? Well, recall the magic of the exhaustivity
  checker when we use GADTs:
-}

head :: Vector ('Successor n) a -> a
head (VCons head _) = head

{-
  'head' is a /total/ function. We don't have to match on the 'VNil' case
  because we could never construct a 'VNil' whose @length@ parameter is a
  @'Successor@! There's no need for 'Maybe' because it /can't/ go wrong at
  run-time. Why stop there? Let's do something /really/ clever:
-}

zip :: Vector n a -> Vector n b -> Vector n (a, b)
zip  VNil         VNil        = VNil
zip (VCons x xs) (VCons y ys) = VCons (x, y) (zip xs ys)

{-
  Now, if you've written 'zip' before, you'll be used to seeing /four/ cases on
  which to pattern-match: nil/nil, cons/nil, nil/cons, cons/cons. However,
  we've said in the type that our vectors are of the same length: by saying the
  first one is 'VNil', the type-checker /knows/ the second must also be the
  same. Similarly for the 'VCons' case: the type-checker /knows/ they must both
  be 'VCons' and their tails must be the same length.

  The type-checker can do a /lot/ with this little bit of extra information,
  and we can express some much more powerful concepts. We'll see later on that,
  with some pretty simple extensions (such as @TypeOperators@), @DataKinds@ is
  the first step towards an extremely rich type-level vocabulary for us to
  misuse!
-}
