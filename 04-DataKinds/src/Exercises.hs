{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
module Exercises where

import Data.Kind (Constraint, Type)
import Data.Function ((&))
import Prelude hiding ((!!), length)





{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.

newtype IntegerWith (method :: IntegerMonoid)
  = IntegerWith Integer

-- | b. Write the two monoid instances for 'Integer'.

instance Monoid (IntegerWith 'Sum) where
  mempty = IntegerWith 0

instance Semigroup (IntegerWith 'Sum) where
  IntegerWith m <> IntegerWith n = IntegerWith (m + n)

instance Monoid (IntegerWith 'Product) where
  mempty = IntegerWith 1

instance Semigroup (IntegerWith 'Product) where
  IntegerWith m <> IntegerWith n = IntegerWith (m * n)

-- | c. Why do we need @FlexibleInstances@ to do this?

-- Our instance isn't of the form @T a b c@ - its type parameter is fixed!




{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

-- | b. What are the possible type-level values of kind 'Maybe Void'?

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?





{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) where
  StringAndIntNil :: StringAndIntList 'Z
  StringCons :: String -> StringAndIntList n -> StringAndIntList ('S n)
  IntCons :: Int -> StringAndIntList n -> StringAndIntList n

-- | b. Update it to keep track of the count of strings /and/ integers.

data StringAndIntList' (strings :: Nat) (ints :: Nat) where
  StringAndIntNil' :: StringAndIntList' 'Z 'Z
  StringCons' :: String -> StringAndIntList' m n -> StringAndIntList' ('S m) n
  IntCons' :: Int -> StringAndIntList' m n -> StringAndIntList' m ('S n)

-- | c. What would be the type of the 'head' function?

head :: StringAndIntList' m n -> Maybe (Either String Int)
head StringAndIntNil' = Nothing
head (StringCons' x _) = Just (Left x)
head (IntCons' y _) = Just (Right y)





{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  IsShowable :: Show a => a -> MaybeShowable 'True
  Isn'tShowable :: a -> MaybeShowable 'False

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.

instance Show (MaybeShowable 'True) where
  show (IsShowable a) = show a

-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.

data Constrainable (c :: Type -> Constraint) where
  Constrained :: c x => x -> Constrainable c
  --             ^ This needs ConstraintKinds





{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (types :: List Type) where
  HNil  :: HList 'Nil
  HCons :: x -> HList xs -> HList ('Cons x xs)

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

tail :: HList ('Cons x xs) -> HList xs
tail (HCons _ xs) = xs

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?

-- We would have to apply 'take' to the type-level list to figure out the type
-- of the result. We don't yet know how to do this... :)





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

data BlogAction' (isAdmin :: Bool) where
  AddBlog'       :: BlogAction' 'False
  DeleteBlog'    :: BlogAction' 'True
  AddComment'    :: BlogAction' 'False
  DeleteComment' :: BlogAction' 'True

-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

-- Who needs GADTs? :)

newtype BlogActionList (isSafe :: Bool)
  = BlogActionList [BlogAction' isSafe]

-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?

-- What's your permission?
data Role = User | Moderator | Admin

-- Who's allowed to do each thing?
data BlogAction'' (who :: [Role]) where
  AddBlog''       :: BlogAction'' '[User, Moderator, Admin]
  DeleteBlog''    :: BlogAction'' '[Admin]
  AddComment''    :: BlogAction'' '[User, Moderator, Admin]
  DeleteComment'' :: BlogAction'' '[Moderator, Admin]





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
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

-- | b. Write a function that extracts a vector's length at the type level:

length :: Vector n a -> SNat n
length  VNil        = SZ
length (VCons x xs) = SS (length xs)

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy

-- No - @Proxy :: Proxy Int@ and @Proxy :: Proxy String@ have the same
-- value-level representation, so we don't have the necessary one-to-one
-- mapping.




{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data Program                     result
  = OpenFile            (Program result)
  | WriteFile  String   (Program result)
  | ReadFile  (String -> Program result)
  | CloseFile (          Program result)
  | Exit                         result

-- | We could then write a program like this to use our language:

myApp :: Program Bool
myApp
  = OpenFile $ WriteFile "HEY" $ (ReadFile $ \contents ->
      if contents == "WHAT"
        then WriteFile "... bug?" $ Exit False
        else CloseFile            $ Exit True)

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

-- It's a bit weird, as we have to write our code "backwards". Our parameters
-- are the programs that happen /after/ the action. @OpenFile@ is probably the
-- nicest example of this. Note that we'd more usually use a @Free@-like
-- structure to do this as it is more intuitive.

data Program' (fileIsOpen :: Bool) result where
  OpenFile'
    :: Program' 'True result  -- What happens after this has an open file.
    -> Program' 'False result -- Can't open a file if one is already open.

  WriteFile'
    :: String -> Program' 'True result -- Writing doesn't close the file.
    -> Program' 'True result           -- Writing only works if a file is open.

  ReadFile'
    :: (String -> Program' 'True result) -- Reading doesn't close the file.
    -> Program' 'True result             -- Only works when a file is open.

  CloseFile'
    :: Program' 'False result -- Closing a file closes the file.
    -> Program' 'True result  -- Only works if a file is open.
 
  -- This is the important line, really. We explicitly don't allow programs to
  -- end with open file handlers.

  Exit'
    :: result
    -> Program' 'False result -- Exiting is a "closed file" operation.


myApp' :: Program' 'False Bool
myApp'
  = OpenFile' $ WriteFile' "HEY" $ (ReadFile' $ \contents ->
      if contents == "WHAT"
        then WriteFile' "... bug?" $ CloseFile' $ Exit' False
                              -- fix ^
        else CloseFile'            $ Exit' True)

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

-- Actual file IO is not that interesting, so I'm going to cheat with some mock
-- data:

interpret :: Program' any a -> IO a

interpret (OpenFile' next)
  = putStrLn "Opened file..." >> interpret next

interpret (WriteFile' output next)
  = putStrLn ("Writing " <> output <> "...") >> interpret next

interpret (ReadFile' k)
  = interpret (k "Some file contents")

interpret (CloseFile' next)
  = putStrLn "Closing file..." >> interpret next

interpret (Exit' x)
  = putStrLn "Goodbye!" >> pure x

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
  -- Z is smaller than the successor of any number.
  SmallerThanZ :: SmallerThan ('S any)

  -- The successor of a number smaller than X is a number smaller than the
  -- successor of X.
  SmallerThanS :: SmallerThan any -> SmallerThan ('S any)

-- | b. Write the '(!!)' function:

(!!) :: Vector n a -> SmallerThan n -> a
(!!) (VCons x _ )  SmallerThanZ    = x
(!!) (VCons _ xs) (SmallerThanS n) = xs !! n

-- | c. Write a function that converts a @SmallerThan n@ into a 'Nat'.

toNat :: SmallerThan n -> Nat
toNat  SmallerThanZ    = Z
toNat (SmallerThanS n) = S (toNat n)
