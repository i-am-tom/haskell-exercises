{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercises where

import Prelude hiding (head)



{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil  :: CountableList
  CountableCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountableNil = 0
countList (CountableCons head tail) = count head + countList tail


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountableNil = CountableNil
dropZero (CountableCons head tail) =
  if (count head == 0) then
    dropZero tail
  else
    CountableCons head (dropZero tail)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyListNil  :: AnyList
  AnyListCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyListNil = AnyListNil
reverseAnyList (AnyListCons head xs) = insertHead head xs where
  insertHead :: a -> AnyList -> AnyList
  insertHead head' AnyListNil = AnyListCons head' AnyListNil
  insertHead head' (AnyListCons head xs') = AnyListCons head $ insertHead head' xs'


filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined -- no, we could have different `a`s in the AnyList to that supplied

lengthAnyList :: AnyList -> Int
lengthAnyList AnyListNil = 0
lengthAnyList (AnyListCons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined -- no, we could have different `a`s in the AnyList and Monoid m applies to a specific `a`

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList xs = lengthAnyList xs == 0

instance Show AnyList where
  show = error "What about me?" -- no, we could have different `a`s in the AnyList and they don't have a Show constraint





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is *input*
-- the only thing we can do to it?

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq a => Eq (TransformableTo a) where
  (TransformWith f x) == (TransformWith g y) = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?


instance Functor TransformableTo where
  fmap fob (TransformWith fio i) = TransformWith (fob . fio) i


{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

isEqual :: EqPair -> Bool
isEqual (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair2 a where
  EqPair2 :: Eq a => a -> a -> EqPair2 a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqPair3 a = EqPair3 a a
-- you would still need a GADT because we want the `Eq a` type constraint


{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int
-- getInt (CharBox ch _) = 10

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox int _)) = int

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox          = 1
countLayers (IntBox _ box)    = 1 + countLayers box
countLayers (StringBox _ box) = 1 + countLayers box
countLayers (BoolBox _ box)   = 1 + countLayers box

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- Does not work without some trickery as we don't know about the nested type.

-- Here's a workaround from https://github.com/AntonRich/haskell-exercises/blob/answers/01-GADTs/src/Exercises.hs:

-- data Layer a b where -- We can use a GADT to encode the layers...
--   Int'    :: Layer Int ()
--   String' :: Layer String Int
--   Bool'   :: Layer Bool String

-- -- And now we can write this function:
-- unpeel :: Layer a b -> MysteryBox a -> MysteryBox b
-- unpeel Int'    (IntBox    _ xs) = xs
-- unpeel String' (StringBox _ xs) = xs
-- unpeel Bool'   (BoolBox   _ xs) = xs

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (a, b) -> a
head (HCons head' _) = head'

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- can't be done because we don't have an exact constructor that matches

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- can't be done?


{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HTreeNil :: HTree Empty
  HTreeCons :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (HTreeCons _ c r) = HTreeCons HTreeNil c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (HTreeCons l1 c1 r1) == (HTreeCons l2 c2 r2) = l1 == l2 && c1 == c2 && r1 == r2


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  AltNil  :: AlternatingList a b
  AltCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts AltNil = []
getFirsts (AltCons firstA rest) = firstA : (getFirsts $ getNextA rest) where
  getNextA :: AlternatingList b a -> AlternatingList a b
  getNextA AltNil = AltNil
  getNextA (AltCons firstB rest') = rest'

getSeconds :: AlternatingList a b -> [b]
getSeconds AltNil = []
getSeconds (AltCons firstA rest) = getFirsts rest

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues AltNil = (mempty, mempty)
foldValues list = (mconcat $ getFirsts list, mconcat $ getSeconds list)

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues'  AltNil        = (mempty, mempty)
foldValues' (AltCons x xs) =
  let (b, a) = foldValues' xs
  in  (x <> a, b)

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals int1Ex1 intEx2) = (eval int1Ex1) == (eval intEx2)
eval (Add intEx1 intEx2)     = (eval intEx1) + (eval intEx2)
eval (If cond ex1 ex2)       = if (eval cond) then (eval ex1) else (eval ex2)
eval (IntValue intVal)       = intVal
eval (BoolValue boolVal)     = boolVal

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyEquals _ _)  = Nothing
  -- case (exp1, exp2) of
  --   (DirtyIntValue intVal1, DirtyIntValue intVal2) -> Just $ Equals (IntValue intVal1) (IntValue intVal2)
  --   _ -> Nothing
parse (DirtyAdd exp1 exp2) =
  case (exp1, exp2) of
    (DirtyIntValue intVal1, DirtyIntValue intVal2) -> Just $ Add (IntValue intVal1) (IntValue intVal2)
    _ -> Nothing
parse (DirtyIf cond exp1 exp2) =
  case cond of
    (DirtyBoolValue boolCond) ->
      case (exp1, exp2) of
        (DirtyIntValue intVal1, DirtyIntValue intVal2) -> Just $ If (BoolValue boolCond) (IntValue intVal1) (IntValue intVal2)
        _ -> Nothing
    _ -> Nothing
parse (DirtyIntValue  intVal)  = Just $ IntValue intVal
parse (DirtyBoolValue _) = Nothing -- we can't return Expr Bool with: Just $ BoolValue boolVal

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

-- Yes?
-- We can't above Maybe I think. How would we represent an Expr that is not Expr Int like Expr Bool?


{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TNil :: TypeAlignedList a a
  TCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

int2String :: Int -> String
int2String = undefined

string2Bool :: String -> Bool
string2Bool = undefined

pipe :: TypeAlignedList Int Bool
pipe = TCons int2String (TCons string2Bool TNil)

-- | b. Which types are existential?
-- The input type of the function

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs xs TNil = xs
composeTALs xs (TCons y ys) = TCons y (composeTALs xs ys)

