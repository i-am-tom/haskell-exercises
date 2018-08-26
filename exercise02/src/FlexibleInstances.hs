{- Now that we're experts in GADTs, today's -} module {- will be a /very/ short
rationale for -} FlexibleInstances {- and -} where {- we need them. -}

---

{-
  Here is a /really/ underwhelming class with no functions or goodies. It has
  one parameter, @a@, and that's it.
-}

class SomeClass a

{-
  Now, it will come as no surprise that we can write an instance for any type
  we can imagine:
-}

instance SomeClass Bool
instance SomeClass Int
-- instance SomeClass String

{-
  If you uncomment that last one, you'll see that GHC gets upset. Specifically,
  it's upset because we are writing an instance for 'String', but 'String' is a
  type synonym for '[Char]'. However, it has a solution to suggest to us:

    {-# LANGUAGE TypeSynonymInstances #-}

  This extension lets us write instances for type synonyms, which GHC can then
  expand for us, meaning that our instance for 'String' will be expanded to
  '[Char]'. Problem solved, right? Well, not quite...

  Remove the extension*, re-comment that instance, and let's imagine we gave up
  and wrote an instance for [Char]:

  * It just so happens that turning on 'FlexibleInstances' will also turn on
  'TypeSynonymInstances' as an /implied extension/, so you'll probably never
  actually need this extension explicitly.
-}

-- instance SomeClass [Char]

{-
  If you uncomment /this/ line, you'll see the error we saw with 'String' once
  we enabled @TypeSynonymInstances@ (although now the error says '[Char]'
  instead of 'String'. Let's take a look:

      • Illegal instance declaration for ‘SomeClass [Char]’
          (All instance types must be of the form (T a1 ... an)
           where a1 ... an are *distinct type variables*,
           and each type variable appears at most once in the instance head.
           Use FlexibleInstances if you want to disable this.)
      • In the instance declaration for ‘SomeClass [Char]’

  This is actually one of GHC's more helpful extension errors. What this says
  is that our instances must all be for types, which it calls 'T'. If those
  types have type parameters, all the parameters must be /variables/ – none of
  them can be concrete values – and all of them must be different.

  Below are a few examples that work. Notice that they're all some type
  constructor applied to a number of variables:
-}

instance SomeClass [a]          -- One variable
instance SomeClass (Either e a) -- Two variables
instance SomeClass ()           -- No variables!

{-
  Conversely, here are some that don't work:
-}

-- The 'Maybe' type has a parameter that isn't a variable!

-- instance SomeClass (Maybe Bool)


-- The variables are not unique.

-- instance SomeClass (Either e e)


-- The variables are unique, but one of 'Either''s parameters isn't a variable:
-- @Maybe a@ is a type that /contains/ a variable, which fails GHC's check. It
-- has to be a type variable, with no exceptions.

-- instance SomeClass (Either e (Maybe a))

{-
  That literally is it. The 'FlexibleInstances' extension lifts these
  restrictions for us:

  - We can now use type synonyms in instance definitions. This can be super
    helpful when you're writing an instance for a big, ugly type.

  - We can write instances that enforce equality between types (such as writing
    an instance for 'Either e e' - we'll only have an instance when the left
    and right types are the same!)

  - We can specify arguments to types that /aren't/ type variables. In other
    words, we can write instances for types with other types in their
    parameters. This is super useful, as we can now write different instances
    for, say, 'Either Int a' and 'Either String a', whereas we were unable
    without 'FlexibleInstances' - both would just be 'Either e a'!
-}
