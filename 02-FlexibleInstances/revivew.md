# Flexible Instances

The extension `FlexibleInstances` allows certain features of the
type system to shine.

The standard instance resolution system requires:
- Up to one concrete name that is not a type synonym...
- Optionally followed by any required type variables...
- That are all completely abstract and unique...
- And no other instances could possibly clash (even if they are more concrete)

Valid:
``` haskell
instance MyClass a  -- would clash with everything else, but is valid itself

instance MyClass Int
instance MyClass [e]
instance MyClass (Either a b)

data Pair a = Pair a a
instance MyClass (Pair a)
```

Invalid (w/o Flexible Instances):
``` haskell
instance MyClass (a, a) -- abstract variables not unique
instance MyClass (Either a (Maybe b)) -- use of inner concrete type Maybe
instance MyClass (a, (b, c)) -- use of inner concrete type (,)
instance MyClass (a -> b -> c) -- use of inner concrete type (->)

type MyClass String -- type synonym (it's [Char])

type Pair' a = (a, a)
instance MyClass (Pair' a) -- type synonym
```
