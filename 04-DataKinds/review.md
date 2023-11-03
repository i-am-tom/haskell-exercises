# DataKinds review

DataKinds allows you to embed phantom data in your type signatures,
eg the length of a vector. Because it's *phantom* data, it cannot
be accessed at runtime without making a new datatype for tagging
at runtime.

Unfortunately, we do not yet have computation availiable on the
DataKinds. You need to make a GADT to run computation with, eg:

``` haskell
data SmallerThan (limit :: Nat) where
  -- | STZ is smaller than any number
  STZ :: SmallerThan ('S a)
  -- | STS a is one smaller than STS (S a)
  STS :: SmallerThan a -> SmallerThan ('S a)

(!!) :: Vector n a -> SmallerThan n -> a
(!!) (VCons h _) STZ      = h
(!!) (VCons _ t) (STS st) = (Exercises.!!) t st
```

## Interesting Examples

### Associated Computation

Consider that integers have two common monoids: (*, 1) and (+, 0).
We can indicate to GHC with no runtime cost which monoid we
want to invoke with an integer by using a newtype

``` haskell
data IntegerMonoid = Sum | Product

newtype IntM (m :: IntegerMonoid) = IntM Int

-- Addition with 0
instance Semigroup (IntM 'Sum) where IntM a <> IntM b = IntM (a + b)
instance Monoid (IntM 'Sum) where mempty = IntM 0

-- Multiplication with 1
instance Semigroup (IntM 'Product) where IntM a <> IntM b = IntM (a * b)
instance Monoid (IntM 'Product) where mempty = IntM 1
```

### Abilities

Consider this example of who may perform what action on a server
(even though we don't have the computation to easily check it yet):

``` haskell
data Role = User | Mod | Admin
data SafeBlogAction (allowed :: [Role]) where
  AddBlog' :: SafeBlogAction '[User, Mod, Admin]
  AddComment' :: SafeBlogAction '[User, Mod, Admin]
  DeleteComment' :: SafeBlogAction '[Mod, Admin]
  DeleteBlog' :: SafeBlogAction '[Admin]
```

You only wish to run a File-Ops datatype if it properly closes
the file it reads/writes to. You can achieve this with clever
datakinds:

``` haskell
data Program (fileIsOpen :: Bool) result where
  -- | OpenFile admits a program to run while open
  -- then shuts itself
  OpenFile  :: Program 'True a              -> Program 'False a
  WriteFile :: String -> Program 'True a    -> Program 'True a
  ReadFile  :: (String -> Program 'True a)  -> Program 'True a
  -- | Closing the file closes it, and
  -- yields a program that requires an open file
  CloseFile :: Program 'False a             -> Program 'True a
  Exit      :: a                            -> Program 'False a
```

Notice that the paramter types are what the computation allows
and return type is used to express what the continuation
should *eventually* yield:

``` haskell
data Program (fileIsOpen :: Bool) result where
    OpenFile ::
        Program 'True a ->  -- allows continuations that need an open file
        Program 'False a    -- however, continuations must also close file
```
