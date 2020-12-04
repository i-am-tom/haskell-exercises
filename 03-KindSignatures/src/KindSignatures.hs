{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module {- 3, in which our heroes discover -} KindSignatures {- and the ways in
which they can help us. We'll see -} where {- we need them, and where they're
just good documentation.. -}

---

{-
  We are, hopefully, familiar with the notion of /types/: @3@ is a value of
  type @Int@, @Nothing@ is a value of @Maybe (IO String)@, and so on. However,
  there is also a notion of types /for types/: kinds.

  In our standard, everyday usage, all the types we use have the same kind:
  '*', which we'll pronounce 'Type'. In fact, we can use 'Type' instead of '*'
  by using the following import.
-}

import Data.Kind -- Type = (*)

{-
  This is a good habit to adopt, as '*' is going to be deprecated soon, in
  favour of the more explicit and (hopefully) clear 'Type'. Now, we know all
  sorts of things of kind 'Type':
-}

class OfKindType a

instance OfKindType  Bool
instance OfKindType  String         -- Which extension did we need
instance OfKindType (Maybe (IO ())) -- to get /these/ to compile?

{-
  We can start with an intuition that the 'Type' kind is inhabited by /things
  with a runtime value/. This isn't 100% accurate, but gives us somewhere to
  start: all our /values/ have a type that has a kind of 'Type'. We can see,
  for example, that @Maybe Int@ is of kind 'Type'. 'Maybe' is a funny one,
  though, right? We could think of it as a "type-level function":

  @
    Maybe :: Type -> Type
  @

  In other words, 'Maybe' on its own isn't a type - it's a type /constructor/.
  "A 'Maybe' of /what/?", the type-checker asks. 'Maybe Int' is a type, and
  'Maybe ()' is a type, but  'Maybe' isn't. We can say that 'Maybe' has kind
  'Type -> Type' because it's like a function that, given a type, gives us a
  type. For example, given 'Int', we get 'Maybe Int'. Given 'Bool', we get
  'Maybe Bool', and so on.

  It is because of this that we can't write the following instance:
-}

-- instance OfKindType Maybe

{-
  • Expecting one more argument to ‘Maybe’
    Expected a type, but ‘Maybe’ has kind ‘* -> *’
  • In the first argument of ‘OfKindType’, namely ‘Maybe’
    In the instance declaration for ‘OfKindType Maybe’

  The big problem here is that, in the absence of a better idea, GHC will
  assume that a typeclass parameter is of kind 'Type'.
  
  How do we tell it otherwise?
  
  One way is to give GHC more information about our type by filling in method
  signatures for our class.
  
  For example, let's take a look at a simplfied definition for the Functor class:
-}

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
{-
  Although @f@ is on its own here in the class definition, if we take a closer
  look at the signature for @fmap@ we can see that @f@ never appears on its
  own, but it sits right in front of another type variable every time it shows
  up: @f a@ and @f b@.
  
  This information alone is enough for GHC to infer that the type of @f@ is
  actually ’* -> *’.
  
  Let's try another example:
-}

class Stuff a where
  thing :: a b c d
  
{-
  Here, we can see @a@ has 3 more type variables right after it: @b@, @c@ and @d@.
  This means that @a@ will be given a kind of ’* -> * -> * -> *’.

  Another way to let GHC know this is by explicitly providing it with a kind
  signature in the class definition itself.
  
  This is exactly what the KindSignatures extension allows us to do:
-}

class OfKindTypeToType (a :: Type -> Type)

{-
  Here, we've said that the /kind/ of the argument to this class must be a
  'Type -> Type' argument. Of course, we actually know plenty of things with
  kind 'Type -> Type', even if we haven't thought about it before:
-}

instance OfKindTypeToType Maybe
instance OfKindTypeToType []
instance OfKindTypeToType (Either e) -- What is the kind of 'Either'?
instance OfKindTypeToType ((,) a)
instance OfKindTypeToType IO

{-
  Try deleting the kind signature from the 'OfKindTypeToType' class, and see
  that we end up with kind errors for every instance above. We'll see later on
  when we discuss ConstraintKinds and DataKinds that this is one of the
  extensions we'll turn on /every time/ we want to do something a little
  complex. I also think it's a nice one simply for the sake of documentation:
-}

class MyFavouriteBifunctor (element :: (Type -> Type -> Type))
instance MyFavouriteBifunctor Either
instance MyFavouriteBifunctor (,)


{-
  So, right now, we can think of all our kinds as being 'Type' or @a -> b@ for
  some kinds @a@ and @b@, and that's it. However, there's one more that is
  worth mentioning: 'Constraint'. This is the kind of constraints:
-}

class Whoa (constraint :: Constraint) -- Also from Data.Kind
instance Whoa (Eq Int)
instance Whoa (Show String)
instance Whoa ()

{-
  That last one might look a bit odd - isn't '()' a type? Shouldn't GHC
  complain that 'Type' and 'Constraint' are different kinds? Well, in true
  Haskell fashion, '()' is actually overloaded: in the 'Constraint' kind, it
  refers to the "empty constraint" - a constraint that is always true. The
  tuple syntax isn't actually that unusual when we think about it:

  @
    f :: (Show a, Eq a) => a -> a -> String
  @

  'Constraint' is the kind of every member on the left side of the fat arrow.
  Sometimes, we just have a single constraint. Sometimes, we have several. In
  order to have several, we use the tuple syntax. In case you're interested:

  @
    g :: () => a -> a
    g = id
  @

  This constraint is always satisfied, so it's not something we see a lot until
  we get to constraint programming (which we'll cover when we get to
  TypeFamilies and ConstraintKinds).
-}
