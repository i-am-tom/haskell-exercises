# KindSignatures Review

KindSignatures lets us put `Kind`s into our declarations, and write classes like
``` haskell
class OfKindTypeToType (a :: Type -> Type)

instance OfKindTypeToType Maybe -- also requires FlexibleInstances
```

Note that this would not be possible otherwise because Maybe is a *type* constructor,
not a *value* constructor.

Also of use is the `Constraint` kind. It is the kind of `(Eq Int)` or `(Show a)`,
basically anything used before the `=>` in a type signature.