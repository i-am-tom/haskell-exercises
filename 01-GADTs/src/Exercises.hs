{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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
  CNil :: CountableList
  CCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CNil        = 0
countList (CCons h t) = count h + countList t


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CNil                       = CNil
dropZero (CCons h t) | count h == 0 = dropZero t
                     | otherwise    = CCons h (dropZero t)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"

-- no, bc ghc does full type erasure, there is no way to do runtime type filtering



{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList = flip inner AnyNil
  where
    inner AnyNil = id
    inner (AnyCons h t) = inner t . AnyCons h

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = error "impossible bc `a` in type sig is fixed, but AnyList can have many types"

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0
lengthAnyList (AnyCons _ t) = 1 + lengthAnyList t

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = error "again impossible bc AnyList permits many types, Monoid does not."

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _    = False

instance Show AnyList where
  show = error "impossible bc a is not constrained by Show"





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

-- `input` bc it is unbound except that arg0 :: (i -> o) accepts it.
-- arg0 is also existential but is also constrained: arg0 :: (->)

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- You would be able to equate output only as its the only constrainable type:
instance Eq a => Eq (TransformableTo a) where
  (TransformWith a b) == (TransformWith x y) = a b == x y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

-- TranformableTo is kind of a "delayed functor" as is:
instance Functor TransformableTo where
  fmap f (TransformWith a b) = TransformWith (f . a) b
  -- alt with eager evaluation:
  -- fmap f (TransformWith a b) = TransformWith f (a b)




{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

eq :: EqPair -> Bool
eq (EqPair a b) = a == b

neq :: EqPair -> Bool
neq = not . eq

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- without RankNTypes or DatatypeContexts it's not possible.
-- newtype EqPair'' a = EqPair'' { p :: Eq a => (a, a) }



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
getInt' (StringBox _ (IntBox n _)) = n

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox        = 0
countLayers (IntBox _ b)    = 1 + countLayers b
countLayers (StringBox _ b) = 1 + countLayers b
countLayers (BoolBox _ b)   = 1 + countLayers b

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- you would need a way to compute what the inner type would be

-------- INCORRECT(-ish)! answer requires an GADT thats tagged nicely:
data Layer a b where
  Int'    :: Layer ()     Int
  String' :: Layer Int    String
  Bool'   :: Layer String Bool

unpeel :: Layer a b -> MysteryBox b -> MysteryBox a
unpeel Int'    (IntBox _ b) = b
unpeel String' (StringBox _ b) = b
unpeel Bool'   (BoolBox _ b) = b




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

head :: HList (h, t) -> h
head (HCons h _) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = error "there's a structural mismatch, a 4-tuple is impossible... right?"

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- doesn't work because you have to unpack the type tuple and add to the end
-- happend HNil = id
-- happend (HCons h t) = HCons h . happend t




{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  Leaf ::                            HTree Empty
  Node :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch left dat right) -> HTree (Branch Empty dat right)
deleteLeft (Node l d r) = Node Leaf d r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  Leaf == Leaf = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (Node tl0 c0 tr0) == (Node tl1 c1 tr1) = c0 == c1 && tl0 == tl1 && tr0 == tr1




{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil  :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts (ACons a rest) = a : getSeconds rest
getFirsts ANil = []

getSeconds :: AlternatingList a b -> [b]
getSeconds (ACons _ rest) = getFirsts rest
getSeconds ANil = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

-- foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
-- foldValues ls = do
--   let as = getFirsts ls
--   let bs = getSeconds ls
--   (mconcat as, mconcat bs)

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues ANil = (mempty, mempty)
foldValues (ACons a rest) = let (bs, as) = foldValues rest in (a <> as, bs)





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
eval (BoolValue b) = b
eval (IntValue i) = i
eval (Equals  a b) = eval a == eval b
eval (Add     a b) = eval a + eval b
eval (If bool a b) = eval $ if eval bool then a else b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

data Typed where
  ExprInt  :: Expr Int  -> Typed
  ExprBool :: Expr Bool -> Typed

tidy :: DirtyExpr -> Maybe Typed
tidy (DirtyIntValue i) = Just . ExprInt $ IntValue i
tidy (DirtyBoolValue i) = Just . ExprBool $ BoolValue i
tidy (DirtyEquals a b) =
  case (tidy a, tidy b) of
    (Just (ExprInt a), Just (ExprInt b)) -> Just . ExprBool $ Equals a b
    _ -> Nothing
tidy (DirtyAdd a b) =
  case (tidy a, tidy b) of
    (Just (ExprInt a), Just (ExprInt b)) -> Just . ExprInt $ Add a b
    _ -> Nothing
tidy (DirtyIf bool a b) =
  case (tidy bool, tidy a, tidy b) of
    (Just (ExprBool bool), Just (ExprInt a),  Just (ExprInt b)) ->
      Just . ExprInt  $ If bool a b
    (Just (ExprBool bool), Just (ExprBool a), Just (ExprBool b)) ->
      Just . ExprBool $ If bool a b
    _ -> Nothing

parse :: DirtyExpr -> Maybe (Expr Int)
parse de = case tidy de of { Just (ExprInt e) -> Just e; _ -> Nothing }

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

-- data Expr a b where
--  ...
--   Func      :: (a -> Expr b) -> Expr (a -> b)
--   Apply     :: Expr (a -> b) -> Expr a -> Expr b
-- It should be doable, though not all outputs would be showable...


{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TALNil  :: TypeAlignedList a a
  TALCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- I don't think any are??
------------- INCORRECT! answer:
-- TALCons's b definitely is

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs = flip appendTAL

appendTAL :: TypeAlignedList a b -> TypeAlignedList b c -> TypeAlignedList a c
appendTAL TALNil = id
appendTAL (TALCons h rest) = TALCons h . appendTAL rest
