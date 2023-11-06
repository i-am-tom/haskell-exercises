# Review

RankNTypes allows you to put `forall` anywhere inside your type signatures.
This can be explained by there-exists/for-all mathematical distinctions, but
it is easier to view it as "enabling choice".

Consider the signature of `id :: forall a. a -> a` in this example:

``` haskell
three = id 3
```

In each case, the `a`s are determined to be `Int` by the argument
the caller provides. The "choice" is made by the caller (`three`)
of `id`.

Consider this broken example:

``` haskell
mapTup f (x, y) = (f x, f y)
--     ^ error myIdFunc is either Int -> Int or String -> String
--       this still won't work if we add the annotation f :: x -> m x
```

What we need is for `f` to be able to "choose" at each calling what the
binding for `a,b` in `f :: a -> b`, and have that binding change.
We do this with the following (functioning) code:

``` haskell
mapTup :: forall a b m. (forall x. x -> m x) -> (a, b) -> (m a, m b)
--       ^ forall a b m. is optional here
mapTup f (x, y) = (f x, f y)
```

Written this way, Haskell understands that `f` does not simply operate
over any one type `a`, but every type `x` that could be either `a` or `b`.
We can say that this inner `forall` enables the callee of `mapTup` to
choose what the bindings for `x` ought to be when it is used.

This use is considered rank-2, because there is two levels of nesting
of `forall`. An example of rank-4 types can be found
![here](https://stackoverflow.com/q/8405364).

## Examples and a Common Idiom

A common strategy for using rank-2 types is to gather the qualified
types to an unqualified result type, and return a value of that type.

Recall this example from GADTs:
``` haskell
data Showable where
    Showable :: Show a => a -> Showable
```

It includes the existential type `a` which is not exposed through
its type -- it is unknown to users of that type.
To operate over it, we may call methods of show:

``` haskell
unpack :: Showable -> String
unpack (Showable a) = show a -- the only method of Show
```

Or, we can run a function that accepts *all types* that are
showable and "chooses" what to do with it:

``` haskell
unpack' :: (forall a. Show a => a -> r) -> Showable -> r
unpack' f (Showable x) = f x
```

This pattern is a common idiom - reify the existential to
a known result type `r`, and return a value of type `r`.

Recall another exmaple:

``` haskell
data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

unwrap :: MysteryBox a -> (forall a. MysteryBox a -> r) -> Maybe r
unwrap EmptyBox f = Nothing
unwrap (IntBox _ b) f = Just $ f b
unwrap (StringBox _ b) f = Just $ f b
unwrap (BoolBox _ b) f = Just $ f b
```

In this case, `f` needs to be able to operate over all possible
boxes, and then we, the callee, can choose which version of `f`
to run (providd there's a box to call `f` on).

## Use with DataKinds

A common place where we need to qualify a type is when its a
parameter to a type constructor. This can happen particularly
when using DataKinds. Recall our vector signature

``` haskell
data Vector (n :: Nat) (a :: Type) where { ... }
```

We want to write a filter function with this type:

``` haskell
filterVec :: (a -> Bool) -> Vector n a -> ___ {- something here -}
```

The problem is that we cannot know what the final length of the
vector will be. For this we need something like this:

``` haskell
filterVec :: (a -> Bool) -> Vector n a -> (forall l. Vector l a) -- error!
```

The problem is that Haskell doesn't know what the output of this
function is. In fact, it *can't* know -- by marking `l` as rank-2,
`filterVec` is the only one with the ability to choose `l`, not
callers of `filterVec` that need the result.

To fix this, we fall back on the idiom above:

``` haskell
filterVec :: (a -> Bool) -> Vector n a -> (forall l. Vector l a -> r) -> r
filterVec pred vec f = ___
```

Implemeting this function is not trivial. The intuition is that we
will build up a value for `f` to execute depending on if elements
pass the predicate

``` haskell
filterVec :: (a -> Bool) -> Vector n a -> (forall l. Vector l a -> r) -> r
filterVec _ VNil f = f VNil -- actually runs f!
filterVec pred (VCons h tail) f = filterVec pred tail $ -- recurse on tail with...
    if p h
        then f . VCons h -- if it passes, run f on VCons head recurseResult
        else f           -- if not, just return `f recurseResult`
    -- note that the first call of f :: Vector ('S x) a -> r
    -- but the second is f :: Vector x a -> r, and that
    -- f is not run here, only (potentially) wrapped with cons
```

Note that because Haskell is lazy, `f` will expand to the original `f`
that was passed in followed by only acceptable elements Cons'ed together.
