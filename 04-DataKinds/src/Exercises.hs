{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Exercises where

import Data.Kind (Type, Constraint)
import Data.Function ((&))

-- added later for fun
import Control.Monad.State





{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.

newtype IntM (m :: IntegerMonoid) = IntM Int

-- | b. Write the two monoid instances for 'Integer'.

instance Semigroup (IntM 'Sum) where IntM a <> IntM b = IntM (a + b)
instance Monoid (IntM 'Sum) where mempty = IntM 0

instance Semigroup (IntM 'Product) where IntM a <> IntM b = IntM (a * b)
instance Monoid (IntM 'Product) where mempty = IntM 1

-- | c. Why do we need @FlexibleInstances@ to do this?

-- We need to use a concrete type binding ('Sum or 'Product)





{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

data VoidKind (a :: Void)

-- | b. What are the possible type-level values of kind 'Maybe Void'?

mvoid :: Maybe Void
mvoid = Nothing

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?

-- It is the 0 of sum types, and 1 of product types, ie
-- it is the identity of algebraic type operations





{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) where
  SAINil :: StringAndIntList 'Z
  SAISCons :: String -> StringAndIntList a -> StringAndIntList ('S a)
  SAIICons :: Int    -> StringAndIntList a -> StringAndIntList a

-- | b. Update it to keep track of the count of strings /and/ integers.

data StringAndIntList' (stringCount :: Nat) (intCount :: Nat) where
  SAINil' :: StringAndIntList' 'Z 'Z
  SAISCons' :: String -> StringAndIntList' a b -> StringAndIntList' ('S a) b
  SAIICons' :: Int    -> StringAndIntList' a b -> StringAndIntList' a ('S b)

-- | c. What would be the type of the 'head' function?

head :: StringAndIntList' a b -> Maybe (Either String Int)
head = undefined




{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  JustShowable :: Show a => a -> MaybeShowable 'True
  NotShowable  :: a -> MaybeShowable 'False

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.

instance Show (MaybeShowable 'True) where
  show (JustShowable x) = "JustShowable " ++ show x

test = do
  print (JustShowable 4)
  -- print (NotShowable Z)


-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.

data Constrainable (c :: Type -> Constraint) where
  Constrained :: c x => x -> Constrainable c
  --             ^ ConstraintKinds



{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (types :: List Type) where
  HNil  :: HList 'Nil
  HCons :: h -> HList tl -> HList ('Cons h tl)

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

tail :: HList ('Cons h tl) -> HList tl
tail (HCons _ rest) = rest

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?


-- we would need to be able to compute the type of the result, which we don't
-- know how to do yet.




{- SIX -}

-- | Here's a boring data type:

data BlogAction
  = AddBlog
  | DeleteBlog
  | AddComment
  | DeleteComment

-- | a. Two of these actions, 'DeleteBlog' and 'DeleteComment', should be
-- admin-only. Extend the 'BlogAction' type (perhaps with a GADT...) to
-- express, at the type-level, whether the value is an admin-only operation.
-- Remember that, by switching on @DataKinds@, we have access to a promoted
-- version of 'Bool'!

data SafeBlogAction (adminOnly :: Bool) where
  AddBlog' :: SafeBlogAction 'False
  AddComment' :: SafeBlogAction 'False
  DeleteComment' :: SafeBlogAction 'True
  DeleteBlog' :: SafeBlogAction 'True

-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

-- data BlogActionList (isSafe :: Bool) where
--   BALNil :: BlogActionList a
--   BALCons :: SafeBlogAction x -> BlogActionList x -> BlogActionList x

------ INCORRECT! answer:

newtype BlogActionList (isSafe :: Bool) = BlogActionList [SafeBlogAction isSafe]


-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?

data Role = User | Mod | Admin
-- data SafeBlogAction (allowed :: [Role]) where
--   AddBlog' :: SafeBlogAction '[User, Mod, Admin]
--   AddComment' :: SafeBlogAction '[User, Mod, Admin]
--   DeleteComment' :: SafeBlogAction '[Mod, Admin]
--   DeleteBlog' :: SafeBlogAction '[Admin]





{- SEVEN -}

-- | When we start thinking about type-level Haskell, we inevitably end up
-- thinking about /singletons/. Singleton types have a one-to-one value-type
-- correspondence - only one value for each type, only one type for each value.
-- A simple example is '()', whose only value is '()'. 'Bool' is /not/ a
-- singleton, because it has multiple values.

-- We can, however, /build/ a singleton type for 'Bool':

data SBool (value :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

-- | a. Write a singleton type for natural numbers:

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat a -> SNat ('S a)

-- | b. Write a function that extracts a vector's length at the type level:

length :: Vector n a -> SNat n
length VNil = SZ
length (VCons _ inner) = SS (Exercises.length inner)

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy

-- INCORRECT! answer:

-- No because Proxy Int and Proxy String have the same representation,
-- to be a proxy, it must have a 1:1 mapping





{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data UnsafeProgram                     result
  = OpenFile'            (UnsafeProgram result)
  | WriteFile'  String   (UnsafeProgram result)
  | ReadFile'  (String -> UnsafeProgram result)
  | CloseFile' (          UnsafeProgram result)
  | Exit'                         result

-- | We could then write a program like this to use our language:

myApp :: UnsafeProgram Bool
myApp
  = OpenFile' $ WriteFile' "HEY" $ ReadFile' (\contents ->
      if contents == "WHAT"
        then WriteFile' "... bug?" $ Exit' False
        else CloseFile'            $ Exit' True)

-- | ... but wait, there's a bug! If the contents of the file equal "WHAT", we
-- forget to close the file! Ideally, we would like the compiler to help us: we
-- could keep track of whether the file is open at the type level!
--
-- - We should /not/ be allowed to open a file if another file is currently
-- open.
--
-- - We should /not/ be allowed to close a file unless a file is open.
--
-- If we had this at the type level, the compiler should have been able to tell
-- us that the branches of the @if@ have different types, and this program
-- should never have made it into production. We should also have to say in the
-- type of 'myApp' that, once the program has completed, the file will be
-- closed.

-- | Improve the 'Program' type to keep track of whether a file is open.  Make
-- sure the constructors respect this flag: we shouldn't be able to read or
-- write to the file unless it's open. This exercise is a bit brain-bending;
-- why? How could we make it more intuitive to write?

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

data Program (fileIsOpen :: Bool) result where
  -- | OpenFile admits a program to run while open
  -- then shuts itself
  OpenFile  :: Program 'True a              -> Program 'False a --- important (fixed) error here!
  WriteFile :: String -> Program 'True a    -> Program 'True a
  ReadFile  :: (String -> Program 'True a)  -> Program 'True a
  -- | Closing the file closes it, and
  -- yields a program that requires an open file
  CloseFile :: Program 'False a             -> Program 'True a  --- another fixed mistake
  Exit      :: a                            -> Program 'False a

mySafeApp :: Program 'False Bool
mySafeApp = OpenFile $ WriteFile "HEY!" $ ReadFile $ \contents ->
  if contents == "WHAT"
    then WriteFile "... lost the data race" $ CloseFile $ Exit False
                                          --  ^ bug fix here!
    else CloseFile $ Exit True

exec :: Program any a -> IO a
exec (Exit x) = return x
exec (OpenFile p) = exec p
exec (CloseFile p) = exec p
exec (WriteFile text p) =
    writeFile "./execed.txt" text >> exec p
exec (ReadFile f) =
  readFile "./execed.txt" >>= exec . f


interpret :: Program any a -> State String a
interpret (OpenFile pro) = interpret pro
interpret (CloseFile pro) = interpret pro
interpret (Exit a) = return a
interpret (WriteFile s pro) = put s >> interpret pro
interpret (ReadFile f) = get >>= interpret . f


{- NINE -}

-- | Recall our vector type:

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | Imagine we want to write the '(!!)' function for this vector. If we wanted
-- to make this type-safe, and avoid 'Maybe', we'd have to have a type that can
-- only hold numbers /smaller/ than some type-level value.

-- | a. Implement this type! This might seem scary at first, but break it down
-- into Z and S cases. That's all the hint you need :)

data SmallerThan (limit :: Nat) where
  -- | Z is smaller than any number
  SmallerThanZ :: SmallerThan ('S a)
  -- | a is one smaller than S a
  SmallerThanS :: SmallerThan a -> SmallerThan ('S a)

  -- Note that limit is never 'Z. This checks out.

-- | b. Write the '(!!)' function:

(!!) :: Vector n a -> SmallerThan n -> a
(!!) (VCons h _) SmallerThanZ      = h
(!!) (VCons _ t) (SmallerThanS st) = (Exercises.!!) t st

-- | c. Write a function that converts a @SmallerThan n@ into a 'Nat'.
toNat :: SmallerThan l -> Nat
toNat SmallerThanZ = Z
toNat (SmallerThanS x) = S (toNat x)