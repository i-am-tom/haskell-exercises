{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- After a brief hiatus, we return with a -} module {- whose focus is on
/kind polymorphism/, which is enabled by the -} PolyKinds {- extension. We'll
find out -} where {- to use it, and wonder how we could do without it! -}

{-
  Haskell, in general, /loves/ the notion of type polymorphism. Let's take a
  nice, familiar function:
-}

id :: a -> a
id x = x

{-
  This function is totally polymorphic in its @a@ variable: you can give it
  /any/ type of value, and you'll get a value of the same type back! Now, let's
  write a type family to do the same:
-}

type family Id (x :: a) :: a where
  Id x = x

{-
  src/PolyKinds.hs:21:1-29: error:
      Unexpected kind variable ‘a’ Perhaps you intended to use PolyKinds
      In the declaration for type family ‘Id’
     |
  21 | type family Id (x :: a) :: a where
     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  GHC, being the marvel of modern engineering that it is, tells us exactly what
  to do here: we have a __kind variable__, which means we're trying to make our
  type family /polymorphic/ in its input and output types. In order to do that,
  we're going to need kind polymorphism, and hence @PolyKinds@. Let's look at
  another example: our humble 'Proxy' type.
-}

data Proxy a = Proxy

{-
  If you type this into GHCi and then ask for the kind, you'll get nothing too
  surprising (remembering that @*@ is what GHC calls the @Type@ kind):

  >>> data Proxy a = Proxy
  >>> :k Proxy
  Proxy :: * -> *

  We see that GHC infers the kind of this type to be a type-constructor of one
  type argument, just like 'Maybe' or 'IO'. However, let's turn on @PolyKinds@:

  >>> :set -XPolyKinds
  >>> data Proxy a = Proxy
  >>> :k Proxy
  Proxy :: k -> *

  GHC has now looked into the constructors of the type and noticed that we
  don't use the type parameter as a type (unlike 'Maybe', for example, whose
  parameter is used in the 'Just' constructor). As a result, it has said that
  we can use /any/ kind for our argument - it is now kind-polymorphic!

  There's really not much more to this extension: just as we don't have to
  write this:
-}

idString :: String -> String
idString x = x

idInt :: Int -> Int
idInt x = x

idBool :: Bool -> Bool
idBool x = x

{-
  We now don't have to do this at the type level! The only thing worth
  mentioning, which won't bother us too much in these exercises, is that kind
  polymorphism has a trick that type polymorphism doesn't have: you can pattern
  match on types within kinds in a kind-polymorphic type family. Don't panic,
  we'll take a look:
-}

data Secrets

type family Smuggler (x :: k) :: k where
  Smuggler (IO (Secrets, a)) = IO a
  Smuggler 0                 =    1
  Smuggler a                 =    a

{-
  Huh. It seems that, while we said this function would work for /any/ kind, we
  actually can inspect the kind in our rules. As a result, type families aren't
  necessarily __parametric__: seeing `type family Id (x :: k) :: k` doesn't
  tell you as much about its implementation as `id :: a -> a` does for a value-
  level function:
-}

smuggle :: a -> a
-- smuggle (xs :: IO (Secrets, a)) = fmap snd xs
smuggle a                       = a

{-
  Once you enable the extensions as GHC tells you, the error is that we
  couldn't match  @IO (Secrets, a)@ with @a@. Because we said this function
  will work /for all/ a, we therefore can't know anything about the value! With
  type families, however, we can look at the kinds involved as more
  arguments to the function. The 'Smuggler' family has two inputs: the /type/
  @x@, and the /kind/ @k@.

  This extra power with kind polymorphism makes for some exciting opportunities
  to abuse the type system, as we'll see in the exercises.
-}
