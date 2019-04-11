{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
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

type family (x :: Nat) + (y :: Nat) :: Nat where
  'Z   + y =         y
  'S x + y = 'S (x + y)

-- | b. Write a type family '*' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (x :: Nat) ** (y :: Nat) :: Nat where
  'Z   ** y = 'Z
  'S x ** y =  y + (x ** y)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

add :: SNat x -> SNat y -> SNat (x + y)
add  SZ    y =           y
add (SS x) y = SS (add x y)





{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append  VNil        ys =                    ys
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

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

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap  VNil        _ = VNil
flatMap (VCons x xs) f = append (f x) (flatMap xs f)





{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (x :: Bool) && (y :: Bool) :: Bool where
  'False && _ = 'False
  'True  && y =  y

-- | b. Write the type-level @||@ function for booleans.

type family (x :: Bool) || (y :: Bool) :: Bool where
  'False || _ = 'False
  'True  || y =  y

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (xs :: [Bool]) :: Bool where
  All '[     ]  = 'True
  All (x ': xs) =  x && All xs





{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare  'Z     'Z    = 'EQ
  Compare  'Z    ('S n) = 'LT
  Compare ('S n)  'Z    = 'GT
  Compare ('S x) ('S y) =  Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max x y = Max' (Compare x y) x y

type family Max' (o :: Ordering) (x :: Nat) (y :: Nat) :: Nat where
  Max' 'LT _ y = y
  Max'  _  x _ = x

-- | c. Write a family to get the maximum natural in a list.

type family Maximum (xs :: [Nat]) :: Nat where
  Maximum '[     ]  = 'Z
  Maximum (x ': xs) = Max x (Maximum xs)





{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (x :: Nat) (xs :: Tree) :: Tree where
  Insert x  'Empty       = 'Node 'Empty x 'Empty
  Insert x ('Node l c r) = Insert' (Compare x c) x ('Node l c r)

type family Insert' (o :: Ordering) (x :: Nat) (xs :: Tree) :: Tree where
  Insert' 'LT x ('Node l c r) = 'Node (Insert x l) c r
  Insert' 'GT x ('Node l c r) = 'Node l c (Insert x r)
  Insert' 'EQ x  xs           =  xs




{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

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

-- In case you're interested, here's a failing test!
-- deleteTest4 :: Insert 'Z 'Empty :~: 'Empty
-- deleteTest4 = Refl

{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[     ]  ++ ys =             ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

appendH :: HList xs -> HList ys -> HList (xs ++ ys)
appendH  HNil        ys = ys
appendH (HCons x xs) ys = HCons x (appendH xs ys)





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
  Every c '[] = ()
  Every c (x ': xs) = (c x, Every c xs)

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance Every Show xs => Show (HList xs) where
  show  HNil        = "[]"
  show (HCons x xs) = show x ++ " : " ++ show xs

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance Every Eq xs => Eq (HList xs) where
  HCons x xs == HCons y ys = x == x && xs == ys
  _          == _          = True -- Could only be two HNils!

-- We have to add 'Every Eq xs' here, which may seem odd, as 'Ord' has
-- previously /implied/ 'Eq'. If GHC knows that every element has an 'Ord'
-- instance, why can't it tell that every one has an 'Eq' instance? The reason
-- is that it certainly /could/ if it tried a bit harder. GHC can't /see/ a
-- constraint that says any particular @x@ has an 'Ord' constraint, which means
-- it can't convince itself that this will be true in the general case.
instance (Every Eq xs, Every Ord xs) => Ord (HList xs) where
  compare (HCons x xs) (HCons y ys) = compare x y <> compare xs ys
  compare  _            _           = EQ





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- Let's worry about performance another day...
type family (x :: [Nat]) +++ (y :: [Nat]) :: [Nat] where
  '[] +++ ys = ys
  (x ': xs) +++ ys = x ': (xs +++ ys)

type family UpTo (x :: Nat) :: [Nat] where
  UpTo  'Z    = '[ 'Z ]
  UpTo ('S n) = UpTo n +++ '[ 'S n ]

-- | b. Write a type-level prime number sieve.
type family Sieve (x :: Nat) :: [Nat] where
  Sieve x = Sieve' (Drop ('S ('S 'Z)) (UpTo x))

type family Drop (n :: Nat) (xs :: [Nat]) :: [Nat] where
  Drop  'Z     xs       = xs
  Drop   _    '[     ]  = '[]
  Drop ('S n) (x ': xs) = Drop n xs

type family Sieve' (xs :: [Nat]) :: [Nat] where
  Sieve' '[     ]  = '[]
  Sieve' (x ': xs) = x ': Sieve' (DropEvery x xs)

type family DropEvery (n :: Nat) (xs :: [Nat]) :: [Nat] where
  DropEvery n xs = DropEvery' n n xs

type family DropEvery' (c :: Nat) (n :: Nat) (xs :: [Nat]) :: [Nat] where
  DropEvery n ('S 'Z) (x ': xs) = DropEvery' n n xs
  DropEvery n   _     '[     ]  = '[]
  DropEvery n ('S c)  (x ': xs) = x ': DropEvery' n c xs

type N0  = 'Z
type N1  = 'S N0
type N2  = 'S N1
type N3  = 'S N2
type N4  = 'S N3
type N5  = 'S N4
type N6  = 'S N5
type N7  = 'S N6
type N8  = 'S N7
type N9  = 'S N8
type N10 = 'S N9

-- Little test...
data (x :: [Nat]) :~~: (y :: [Nat]) where
  NRefl :: x :~~: x

test :: Sieve N10 :~~: '[ N2, N3, N5, N7 ]
test = NRefl


-- | c. Why is this such hard work?

-- I think it boils down to a few things:
--
-- * No let-binding at the type level.
--
-- * No higher-order functions - I can't write higher-order helpers without
--   running into complaints about type families not having all their
--   arguments. Recent work on unsaturated type families (type families without
--   all their arguments) promises a solution to this, though!
--
-- * Syntax!
