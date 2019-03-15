{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

{- In Lou Bega's -} module {- number 5, we'll be discussing /higher rank
polymorphism/ using the -} RankNTypes {- extension, and how a function can
change entirely depending on -} where {- you put your quantifiers. -}

{-
  Let's start things off with a function to wrap the values of a 'Tuple' in
  'Maybe' by applying them both to 'Just':
-}

wrapMaybe (x, y) = (Just x, Just y)

example :: (Maybe Int, Maybe String)
example = wrapMaybe (25, "Tom")

{-
  No problems so far: the 'Just' function takes a value of any type @a@, and
  returns a value of @Maybe a@. Now, this function is a bit specific: what we'd
  really like to do is _pass in_ the wrapping function that takes any type @a@
  to a @f a@ for some @f@. No problem, we'll just introduce a couple more
  variables:
-}

wrapAny f (a, b) = (f a, f b)

-- example2 :: (Maybe Int, Maybe String)
-- example2 = wrapAny Just (25, "Tom")

{-
  ... huh. The function type-checks, but the usage doesn't. Specifically, we
  get a type error:

  • Couldn't match type ‘[Char]’ with ‘Int’
    Expected type: (Maybe Int, Maybe String)
      Actual type: (Maybe [Char], Maybe [Char])
  • In the expression: wrapAny Just (25, "Tom")
    In an equation for ‘example2’: example2 = wrapAny Just (25, "Tom")

  Well, that's weird. Let's look at the type of 'wrapAny':

  >>> :t wrapAny
  wrapAny :: (a -> b) -> (a, a) -> (b, b)

  What happened?!

  The GHC type-checker looks in the body of the function, and sees that the
  same @f@ is used for both @a@ and @b@, and deduces that they must therefore
  be the same type. After all, how else could it work?

  ... The problem is, of course, we /know/ it could work - 'Just' /should/
  work. The problem, it turns out, is a little bit subtle.

  By turning on RankNTypes*, we have also enabled ExplicitForAll, which allows
  us to write some syntax that may be familiar to PureScript users:
-}

id :: forall a. a -> a
id x = x

{-
  This definition says that 'id' will work /for all/ types that @a@ could
  possibly be. This is nothing special – GHC internally writes this @forall@
  for us when we write a function like @id :: a -> a@ – so what's the big deal?
  Well, let's look at 'Just':
-}

just :: forall a. a -> Maybe a
just = Just

{-
  Cool. Nothing too surprising yet. Now, let's look at that 'wrapAny' again:
-}

wrapAny' :: forall a b. (a -> b) -> (a, a) -> (b, b)
wrapAny' f (x, y) = (f x, f y)

{-
  This is obviously not what we want: we want to introduce a new type
  parameter - @x@!
-}

-- wrapAny'' :: forall a b f x. (x -> f x) -> (a, b) -> (f a, f b)
-- wrapAny'' f (x, y) = (f x, f y)

{-
  Another type error!

  • Couldn't match expected type ‘x’ with actual type ‘a’
    ‘a’ is a rigid type variable bound by
      the type signature for:
        wrapAny'' :: forall a b (f :: * -> *) x.
                     (x -> f x) -> (a, b) -> (f a, f b)
      at src/RankNTypes.hs:83:1-63
    ‘x’ is a rigid type variable bound by
      the type signature for:
        wrapAny'' :: forall a b (f :: * -> *) x.
                     (x -> f x) -> (a, b) -> (f a, f b)
      at src/RankNTypes.hs:83:1-63
  • In the first argument of ‘f’, namely ‘x’
    In the expression: f x
    In the expression: (f x, f y)

  This definition /still/ doesn't type-check! Specifically, it's saying we
  can't unify @x@ with @a@, which means we can't call an @x -> f x@ function
  with an @a@ value. This makes sense. Imagine the following user call:
-}

-- wrapAnyExample :: (Maybe Int, Maybe String)
-- wrapAnyExample = wrapAny (\x -> Just (x + 1)) (25, "Tom")

{-
  Here, our users have chosen @f@ to be @Maybe@, @a@ to be @Int@, @b@ to be
  @String@, and @x@ to be @Int@ as well. There's the problem, though: we can't
  use an @Int -> Maybe Int@ function on a @String@! What /we/ need is a way of
  saying that the function must work /for all/ types that we put in. Wait...
-}

wrapAnyNew :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
wrapAnyNew f (x, y) = (f x, f y)

{-
  Well, this seems to type-check, but... why? Well, let's think through it.
  Consider the following signatures:
-}

id' :: forall a. a -> a
id' x = x

const' :: forall a b. a -> b -> a
const' x _ = x

map' :: forall a b f. Functor f => (a -> b) -> (f a -> f b)
map' = fmap

{-
  In all these examples, the signatures say that they will work /for all/
  types. In the case of map', we can give it an @a -> b@ and get back a
  function  @f a -> f b@ /for all/ @a@, @b@, and @f@ types. What this means in
  practice is that we, as the caller, get to /choose/ what those types are.

  The function can't know anything more about them than what's in the context - in
  the case of @map'@, that it's a functor - because, beyond that, it could be
  /anything/.

  The problem with this is that the function never gets to choose one of the
  values for us. In the case of 'wrapAny', what it wants from /us/ is a
  polymorphic function that will, /for all/ types, lift them into some context
  like 'Maybe'.

  We need to give it a function that works /for all/ types so that /it/ can
  choose which types to pass in. That's why the argument has a @forall@!

  This who-gets-to-choose is a really good starting intuition for how ranks
  work:

  rank-0   Int -> String                            No one gets to choose.
  rank-1   forall a. a -> a                         Caller gets to choose.
  rank-2   forall a. a -> (forall b. b -> b) -> a   Called gets to choose.

  This actually generalises, although we won't see many examples of functions
  with higher-ranked than 2. Use cases /do/ exist, however, if you're curious:

  https://stackoverflow.com/q/8405364

  For one last thought, let's think back to our existential GADTs:
-}

data Showable where
  Showable :: Show a => a -> Showable

{-
  We said, to do anything with a 'Showable', we'd have to supply a function
  that worked /for all/ types implementing 'Show'. In other words, we could
  write an unpacking function:
-}

unpack :: Showable -> String
unpack (Showable a) = show a

{-
  Or, we could generalise it to take any relevant function. What might the type
  look like, you ask? Exactly as we'd imagine:
-}

unpack' :: (forall a. Show a => a -> r) -> Showable -> r
unpack' f (Showable x) = f x

{-
  We don't know what's in the 'Showable', we just know it has a 'Show'
  instance. So, in order to unpack it, we need a function that can express
  that! In reality, the first thing this function is going to do is 'show', but
  it won't always be that simple. The point is that we can use rank-2 to
  require that the given function works for /any/ type with a particular class'
  interface.

  This is the pattern you'll see in the overwhelming majority of examples with
  rank-2 types: we don't know what the value /is/, but we know about some
  constraint that it has. Get used to seeing signatures that look something
  like @X -> (forall a. Constraint a => a -> r) -> r@, because we'll be seeing
  a /lot/ of them.
-}
