{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

{- I'd quite like a -} module {- please,
on -} FunctionalDependencies {-
On the left I'll write types
to determine the right,
and infer them -} where{-ever I please. -}

{-
  In the last session, we looked at multi-parameter type classes as a method of
  defining relationships between types. This was great, but we ran into a few
  tricky problems.

  Let's start with this friendly little typeclass:
-}

class Transform a b where
  transform :: a -> b

{-
  Everything looks fine, so let's write an instance!
-}

instance Transform Int String where
  transform = show

{-
  All good, now let's try to use it...
-}

-- test :: IO ()
-- test = print (transform (3 :: Int))

{-
  • Ambiguous type variable ‘a0’ arising from a use of ‘print’
    prevents the constraint ‘(Show a0)’ from being solved.
    Probable fix: use a type annotation to specify what ‘a0’ should be.

  Hm. This isn't ideal. Let's break down what happened:

  - @transform (3 :: Int)@ has type @forall b. Transform Int b => b@
  - 'print' has type @forall a. Show a => a -> IO ()@

  GHC, it seems, has no idea what should come out of the @transform@ call, and
  no strong opinions on what should go /in/ the 'print' call - it could be
  anything 'Show'able!

  Not to worry, though: we've dealt with this before!
-}

instance b ~ String => Transform Bool b where
  transform = show

tested :: IO ()
tested = print (transform True)

{-
  Phew! GHC sees @transform :: forall b. Transform Bool b => b@, and then
  notices an instance head that exactly says @Transform Bool b@! "Huzzah", it
  exclaims, and deduces that the output type be 'String'.

  Let's make a little upgrade to our typeclass:
-}

-- class HelpfulTransformer a b where
--   f    :: a -> b
--   help :: a

{-
  We now have both a transformation function, /and/ a get-out-of-jail-free
  function to grab us an @a@ if we forgot to bring one.  It's contrived, sure,
  but it illustrates something we'll see a /lot/ in the exercises.

  Uncomment this, and you'll get a hefty complaint from GHC:

  src/FunctionalDependencies.hs:75:3-11: error:
    • Could not deduce (HelpfulTransformer a b0)
      from the context: HelpfulTransformer a b
        bound by the type signature for:
                   help :: forall a b. HelpfulTransformer a b => a
        at src/FunctionalDependencies.hs:75:3-11
      The type variable ‘b0’ is ambiguous
    • In the ambiguity check for ‘help’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes

  Hmm. If we read this a couple times, it actually starts to make sense: our
  @help@ function doesn't mention @b@, so we can't tell which pair of @a@ and
  @b@ we should choose in order to find an appropriate instance. GHC words this
  as, "@b@ is an /ambiguous type/".

  Now, we could turn on @AllowAmbiguousTypes@, but we've already seen that this
  probably isn't a particularly helpful extension given only what we've seen so
  far. It certainly doesn't get us any closer to being able to /use/ the @help@
  function! What we /need/ is a way of saying:
  
  /Each @a@ has a unique @b@ value. If you know @a@, there will be /at most
  one/ instance of the class in scope, and that will tell you what @b@ is./

  This is just like regular functions: each possible input has a single output.
  It doesn't have to be a /unique/ output, but knowing the input to a function
  is enough to calculate the output. If this is getting too abstract, let's
  code some more:
-}

-- The functional dependency syntax --v
class MoreHelpfulTransformer a b | a -> b where
  moreF    :: a -> b
  moreHelp :: a

instance MoreHelpfulTransformer Int String where
  moreF    = show
  moreHelp = 25

instance MoreHelpfulTransformer String String where
  moreF    = id
  moreHelp = "Tom"

{-
  No complaints! GHC is now happy in the knowledge that it won't find two
  instances with the same left-hand type, and so it can /infer/ the right-hand
  type for you. As well as helping our @help@ function, this also helps with
  our earlier problem, too!
-}

testest :: IO ()
testest = print (moreF (25 :: Int))

{-
  No fancy equality constraint tricks here! GHC sees that call to @moreF@, sees
  that @a ~ Int@, and uses the functional dependency to deduce that @b ~
  String!@ it really is quite magical.

  As a general starter intuition, we can think of a functional dependency like
  @a -> b@ as saying, "you'll never find two instances where the @a@s are the
  same and the @b@s aren't".

  Of course, you might be wondering: what happens if the left-hand variable
  /isn't/ unique? What happens if I write this:
-}

-- instance MoreHelpfulTransformer String Bool where
--   moreF    = (== "Tom")
--   moreHelp = True

{-
  src/FunctionalDependencies.hs:120:10-45: error:
    Functional dependencies conflict between instance declarations:
      instance MoreHelpfulTransformer String String
        -- Defined at src/FunctionalDependencies.hs:120:10
      instance MoreHelpfulTransformer String Bool
        -- Defined at src/FunctionalDependencies.hs:128:10

  The compiler's onto your tricks! The functional dependency tells GHC that it
  can both /infer/ the variables on the right-hand side of the functional
  dependency, and that it can /enforce/ uniqueness on the left-hand side.

  In the exercises, we'll see that you can actually put any number of variables
  on the left-hand side of a functional dependency arrow. You can also put
  _more than one_ on the right-hand side. You can even write several functional
  dependencies!
-}

class Example a b c d
  | a b c -> d -- Many left-hand things - every @a@/@b@/@c@ combination must
               -- have the same @d@!

  , a d -> b c -- Many right-hand things - every @a@/@d@ combination must have
               -- the same @b@/@c@!

  , c -> a    -- Commas separate multiple functional dependencies.

{-
  The only thing that is important is that there cannot be any
  overlap between combinations of the left-hand variables in your instances. If
  you have @class C a b c d | a b -> c d@, your instances must all have unique
  pairs of @a@ and @b@ types!
-}
