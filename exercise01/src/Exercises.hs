{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Exercises where





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
  CountableCons :: Countable x => x -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList (CountableCons x xs) = count x + countList xs
countList  CountableNil        = 0


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero (CountableCons x xs)
  = if count x == 0
      then                  dropZero xs
      else CountableCons x (dropZero xs)

dropZero empty = empty


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"

-- We can't - we know nothing about the type inside the 'CountableList' other
-- than the fact that we can 'count' it. This means it could be anything with a
-- 'Countable' instance!





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: x -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList xs = go xs id
  where
    go  AnyNil        f = f AnyNil
    go (AnyCons x xs) f = go xs (AnyCons x . f)

-- We can't write a function that actually filters our list: @a -> Bool@ means
-- every type in our 'AnyList' has to be @a@; how could we know that this is
-- true? There are a few ways to implement a function with this type
-- signature, but none of them will filter anything.

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

countAnyList :: AnyList -> Int
countAnyList (AnyCons _ xs) = 1 + countAnyList xs
countAnyList  AnyNil        = 0

-- The only implementation would be 'mempty', as we can't know that our types
-- are all 'm'. This isn't really a fold, so we're out of luck :(

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _      = False

-- We can't show any of the elements, because we don't know whether they're
-- showable. Morally, the answer to this one is therefore "no". However, we
-- could at least have a go at representing the size of the structure. For
-- example:

instance Show AnyList where
  show  AnyNil        = "!"
  show (AnyCons _ xs) = '?' : show xs





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

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- @input@ is the existential type: all we can do is transform it to our output
-- type, at which point we /can/ know its type.

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- We would only be able to compare the result of the transformation:
instance Eq a => Eq (TransformableTo a) where
  TransformWith f x == TransformWith g y = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap g (TransformWith f x) = TransformWith (g . f) x





{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

areEqual :: EqPair -> Bool
areEqual (EqPair x y) = x == y

areNotEqual :: EqPair -> Bool
areNotEqual = not . areEqual

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- We can represent this as a normal pair:

data EqPair'' a = EqPair'' a a

-- HOWEVER, we now have no guarantee that the type inside the 'EqPair''' is an
-- instance of 'Eq'. If we wanted this guarantee, we would need the GADT in
-- order to require that instance and then assume it when we inspect them.





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

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox x _)) = x

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers  EmptyBox        = 0
countLayers (IntBox    _ xs) = 1 + countLayers xs
countLayers (StringBox _ xs) = 1 + countLayers xs
countLayers (BoolBox   _ xs) = 1 + countLayers xs

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- You can't write a function to do this because we have no way of figuring out
-- the type of the layer below. However, if you're willing to be a bit
-- sneaky... (hat-tip @LiamGoodacre):

data Layer a b where -- We can use a GADT to encode the layers...
  Int'    :: Layer Int ()
  String' :: Layer String Int
  Bool'   :: Layer Bool String

-- And now we can write this function:
unpeel :: Layer a b -> MysteryBox a -> MysteryBox b
unpeel Int'    (IntBox    _ xs) = xs
unpeel String' (StringBox _ xs) = xs
unpeel Bool'   (BoolBox   _ xs) = xs

-- It's definitely not perfect, but it's a pretty good go!





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
head (HCons x _) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- We can't pattern-match on this, because there's no constructor of our GADT
-- that makes a four-tuple type. The type is therefore uninhabited.
patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- The problem is that we need to combine the two tuples into a bigger one /at
-- the type level/. We'll see in later chapters that we can absolutely do this,
-- but not using GADTs alone.





{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty
    :: HTree Empty

  HBranch
    :: HTree left -> centre -> HTree right
    -> HTree (Branch left centre right)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft
  :: HTree (Branch left centre right)
  -> HTree (Branch Empty centre right)

deleteLeft (HBranch _ centre right)
  = HBranch HEmpty centre right

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. Recursion is your friend here - you
-- shouldn't need to add a constraint to the GADT!

-- This is a little bit naughty - it requires the 'FlexibleInstances' and
-- 'FlexibleContexts' extensions. Luckily, GHC suggests both of these to us, so
-- we can get here if we keep doing as we're told. We'll look into these
-- extensions later.

instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree left), Eq centre, Eq (HTree right))
    => Eq (HTree (Branch left centre right)) where
  HBranch l x r == HBranch l' x' r' = l == l' && x == x' && r == r'





{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  AlternatingNil :: AlternatingList a b
  AlternatingCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts  AlternatingNil        = []
getFirsts (AlternatingCons x xs) = x : getSeconds xs

getSeconds :: AlternatingList a b -> [b]
getSeconds  AlternatingNil        = []
getSeconds (AlternatingCons _ xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues  AlternatingNil        = (mempty, mempty)
foldValues (AlternatingCons x xs) = let (b, a) = foldValues xs in (x <> a, b)
-- foldValues xs = (mconcat (getFirsts xs), mconcat (getSeconds xs))





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
eval (Equals x y) = eval x == eval y
eval (Add x y) = eval x + eval y
eval (If p t f) = if eval p then eval t else eval f
eval (IntValue x) = x
eval (BoolValue x) = x

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

-- This is a super tricky one. First, we need a GADT that means we can
-- patten-match to figure out the type of our expression. If we have an
-- @IntType x@, we know that @x :: Expr Int@. Similarly for @BoolType x@!

data Typed where
  IntType  :: Expr Int  -> Typed
  BoolType :: Expr Bool -> Typed

-- Now, we do some really grotty pattern-matching to guarantee that our types
-- line up. 'Typed' gives us a way to figure out whether we have the right
-- types, and so we can just start to put together an expression. The nice
-- thing here is that it's pretty much guaranteed to work if it compiles
-- because of how strict the types are!

tidy :: DirtyExpr -> Maybe Typed

tidy (DirtyEquals x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (BoolType (Equals x y))
  _                                    -> Nothing

tidy (DirtyAdd x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (IntType (Add x y))
  _                                    -> Nothing

tidy (DirtyIf p t f) = case (tidy p, tidy t, tidy f) of
  (Just (BoolType p'), Just (IntType t'), Just (IntType f')) ->
    Just (IntType (If p' t' f'))
  (Just (BoolType p'), Just (BoolType t'), Just (BoolType f')) ->
    Just (BoolType (If p' t' f'))

tidy (DirtyIntValue  x) = Just (IntType  (IntValue  x))
tidy (DirtyBoolValue x) = Just (BoolType (BoolValue x))

-- Finally, 'parse' is just a little one.

parse :: DirtyExpr -> Maybe (Expr Int)
parse xs = case tidy xs of
    Just (IntType x) -> Just x
    Nothing          -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe'?

-- We can steal functions from Haskell to achieve this sort of thing. When we
-- do so, it's called a Higher-Order Abstract Syntax (or HOAS for short). This
-- means we end up with a pretty little thing like:

data MoreExpr a where
-- ....
  Function :: (a -> MoreExpr b) -> MoreExpr (a -> b)
  Apply    :: MoreExpr (a -> b) -> MoreExpr a -> MoreExpr b





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TypeAlignedNil :: TypeAlignedList a a
  TypeAlignedCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- All of them except the first and last!

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs xs ys = TypeAlignedCons (toFunction ys) xs
  where
    toFunction :: TypeAlignedList a b -> (a -> b)
    toFunction  TypeAlignedNil        = id
    toFunction (TypeAlignedCons f fs) = toFunction fs . f

