# GADTs Review


## GADTs
Generalized Algebraic Data Types give us the ability to explicitly define
the types of constructors of our datatypes.

``` haskell
data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
```

They also give us the ability
to assign values to type parameters as a result of constructors...

``` haskell
data Expr a where
     -- reasonable, would be default
    IntLit :: Int -> Expr Int
    BoolLit :: Bool -> Expr Bool
    -- these are different!
    And :: Expr Bool -> Expr Bool -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a
```

As well as strong guarantees on pattern matching. Note how this is
not exhaustive of `Expr a`, but it suffices for `Expr Int`

``` haskell
add1 :: Expr Int -> Expr Int
add1 (IntLit i) = IntLit (i + 1)
```

## Existentials

Certain types only exist within the constructor of a datatype, like
in this example from the exercises:

```haskell
data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input    -- < existential!
    -> TransformableTo output
```

`input` is unlisted in the type params, and is only possible through
an implicit `forall input. ...` after the `::`. The only thing we
may do with `arg1 :: input` is run `arg0 :: (input -> output)` on it.

With more constraints, we could do more (like `Show input` or `Eq input`).

## Interesting Examples

Complex type dependencies:
```haskell
-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right...
data TypeAlignedList a b where
  TALNil  :: TypeAlignedList a a
  TALCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c
```

Making a well-typed system, and coercing into it with tagging:

``` haskell
data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool

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
tidy = ...
```




## Summary

GADTs allow us to write and type check complex types and have control over
which constructors pattern-match certain types. They can be leveraged to
tag data, and work around type erasure.