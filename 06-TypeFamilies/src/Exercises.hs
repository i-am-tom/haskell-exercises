{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (a :: Nat) + (b :: Nat) :: Nat where
  'Z + x = x
  ('S a) + b = 'S (a + b)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (a :: Nat) ** (b :: Nat) :: Nat where
  'Z ** _ = 'Z
  -- keeping these base cases confusees ghc,
  -- it cannot derive 'S x ** y == y + (x ** y)
  -- unless we can keep all recursion on the left
  -- _ ** 'Z = 'Z
  -- x ** 'S 'Z = x
  -- just switching to (a ** b) + b also breaks it.
  'S a ** b = b + (a ** b) -- UndecidableInstances
-- quote: Illegal nested type family application ‘b + (a ** b)’

-- | c. Write a function to add two 'SNat' values.

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

addSNat :: SNat a -> SNat b -> SNat (a + b)
addSNat SZ = id
addSNat (SS sn) = SS . addSNat sn





{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil = id
append (VCons h t) = VCons h . append t

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap VNil _ = VNil
flatMap (VCons h t) f = f h `append` flatMap t f

-- from answers:
-- | This is a really interesting problem, and really exposes the problems we
-- have in type-level programming: we can't convince GHC that @x + y == y + x@,
-- or that @x + (y + z) == (x + y) + z@, without providing a very explicit
-- proof. It just so happens that, if we define `**` with the successor's
-- recursive step on the /right/ (as above), we're fine and don't need to do
-- any of this hard work. Unfortunately, though, we'll regularly be less lucky.
--
-- This is irritating enough that libraries (or, rather, plugins) such as
-- http://hackage.haskell.org/package/ghc-typelits-natnormalise exist purely to
-- avoid these messes.





{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (a :: Bool) && (b :: Bool) :: Bool where
  'False && _ = 'False
  'True  && x = x

-- | b. Write the type-level @||@ function for booleans.

type family (a :: Bool) || (b :: Bool) :: Bool where
  'False || x = x
  'True  || _ = 'True

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of booleans are @'True@.

type family All (l :: [Bool]) where
  All '[] = 'True
  All (h:t) = h && All t




{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (a :: Nat) (b :: Nat) :: Ordering where
  Compare 'Z ('S _) = 'LT
  Compare 'Z 'Z     = 'EQ
  Compare ('S _) 'Z = 'GT
  Compare ('S x) ('S y) = Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

-- type family Max (a :: Nat) (b :: Nat) :: Nat where
--   Max 'Z y = y
--   Max x 'Z = x
--   Max ('S x) ('S y) = Max x y

-- partially INCORRECT!: what I wanted but couldn't figure out:

type family Max' (ord :: Ordering) (a :: Nat) (b :: Nat) :: Nat where
  Max' 'LT _ y = y
  Max' _   x _ = x

type family Max (a :: Nat) (b :: Nat) where
  Max x y = Max' (Compare x y) x y

-- | c. Write a family to get the maximum natural in a list.

type family Maximum (l :: [Nat]) where
  Maximum '[x] = x
  Maximum (h:t) = Max h (Maximum t)



{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (x :: Nat) (t :: Tree) :: Tree where
  Insert e 'Empty = Node 'Empty e 'Empty
  Insert e ('Node l a r) = Insert' (Compare e a) e ('Node l a r)

type family Insert' (ord :: Ordering) (x :: Nat) (t :: Tree) where
  Insert' 'LT x ('Node l a r) = 'Node (Insert x l) a r
  Insert' 'EQ _ ('Node l a r) = 'Node l a r
  Insert' 'GT x ('Node l a r) = 'Node l a (Insert x r)




{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

-- type family Delete (x :: Nat) (t :: Tree) :: Tree where
--   Delete n 'Empty = 'Empty
--   Delete n ('Node l a r) = Delete' (Compare n a) ('Node l a r)

-- type family Delete' (ord :: Compare) (x :: Nat) (t :: Tree) where
--   Delete' 'LT x ('Node l _ _) = Delete x l
--   Delete' 'EQ _ ('Node l _ r) = Slip l r
--   Delete' 'GT x ('Node _ _ r) = Delete x r

-- type family Slip (l :: Tree) (r :: Tree) where
--   Slip l 'Empty = l
--   Slip l' ('Node l a r) = 'Node l' a (Slip l r)

--- (partially) INCORRECT! the invariant is not preserved:
-- SUPER UGLY.

type family Delete (x :: Nat) (xs :: Tree) :: Tree where
  Delete x  'Empty       = 'Empty
  Delete x ('Node l c r) = Delete' (Compare x c) x ('Node l c r)

-- We can't let-bind the result of a function like 'Compare', so we have to
-- have a helper family to compute the above.
type family Delete' (o :: Ordering) (x :: Nat) (xs :: Tree) :: Tree where
  Delete' 'LT x ('Node  l     c r) = 'Node (Delete x l) c r
  Delete' 'GT x ('Node  l     c r) = 'Node l c (Delete x r)
  Delete' 'EQ x ('Node 'Empty c r) = r
  Delete' 'EQ x ('Node  l     c r) = Repair (Biggest l) r

-- ... We also can't have a helper family for the last case above, so we need
-- two more helper families:
type family Repair (parts :: (Nat, Tree)) (xs :: Tree) :: Tree where
  Repair '(c, l) r = 'Node l c r

type family Biggest (xs :: Tree) :: (Nat, Tree) where
  Biggest ('Node l c 'Empty) = '(c, l)
  Biggest ('Node l c r)      = Biggest' l c (Biggest r)

-- Reconstructing the tree would also require a let-binding, so we have
-- /another/ helper family. Eurgh!
type family Biggest' (l :: Tree) (c :: Nat) (r' :: (Nat, Tree)) :: (Nat, Tree) where
  Biggest' l c '(x, r) = '(x, 'Node l c r)

-- We can use this type to write "tests" for the above. Any mention of Refl
-- will force GHC to try to unify the two type parameters. If it fails, we get
-- a type error!
data (x :: Tree) :~: (y :: Tree) where
  Refl :: x :~: x

deleteTest0 :: Delete 'Z 'Empty :~: 'Empty
deleteTest0 = Refl

deleteTest1 :: Delete 'Z (Insert 'Z 'Empty) :~: 'Empty
deleteTest1 = Refl

deleteTest2 :: Insert 'Z (Insert 'Z 'Empty) :~: Insert 'Z 'Empty
deleteTest2 = Refl

deleteTest3
   :: Insert ('S 'Z) (Insert 'Z 'Empty)
  :~: 'Node 'Empty 'Z ('Node 'Empty ('S 'Z) 'Empty)
deleteTest3 = Refl




{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[]      ++ y = y
  (h ': t) ++ y = h ': (t ++ y)

appendH :: HList as -> HList bs -> HList (as ++ bs)
appendH HNil rs = rs
appendH (HCons h t) rs = HCons h (appendH t rs)





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _ '[] = ()
  Every f (h ': t) = (f h, Every f t)

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance Every Show as => Show (HList as) where
  show HNil = []
  show (HCons h t) = show h ++ ":" ++ show t

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance Every Eq as => Eq (HList as) where
  HNil        == HNil        = True
  HCons h0 t0 == HCons h1 t1 = h0 == h1 && t0 == t1

instance (Every Eq as, Every Ord as) => Ord (HList as) where
  HNil <= HNil = True
  HCons h0 t0 <= HCons h1 t1 = h0 <= h1 && t0 <= t1





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- type family Range (n :: Nat) :: [Nat] where
--   Range 'Z = '[]
--   Range ('S x) = Range' (Range x)

-- type family Range' (ns :: [Nat]) :: [Nat] where Range' xs = Length xs : xs
-- type family Length (ns :: [Nat]) :: Nat where
--   Length '[] = Z
--   Length (_ ': t) = 'S (Length t)

-- ALT:
type family (a :: [Nat]) +++ (b :: [Nat]) :: [Nat] where
  '[] +++ bs = bs
  (a ': as) +++ bs = a ': as +++ bs

type family UpTo (n :: Nat) :: [Nat] where
  UpTo 'Z = '[ 'Z ]
  UpTo ('S n) = UpTo n +++ '[ 'S n ]

-- | b. Write a type-level prime number sieve.

--- uhhhhh no thank you.

-- trying to recreate answer IsMod

-- if x | n
type family (n :: Nat) |%| (x :: Nat) :: Bool where
  n |%| x = ModClock n n x

type family ModClock (n :: Nat) (clock :: Nat) (x :: Nat) :: Bool where
  ModClock _ 'Z     'Z = 'True
  ModClock _ ('S _) 'Z = 'False
  -- tick down the clock, one at a time, keeping the original denom
  ModClock n ('S m) ('S x) = ModClock n m x
  -- if the clock hits zero, reset, ticking
  ModClock n 'Z    ('S x) = ModClock n n x


-- | c. Why is this such hard work?

-- | no lets, no pattern-matching, no gaurding
-- no higher order funcs.
