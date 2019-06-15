{-# LANGUAGE GADTs #-}

module {- 1, in which we'll cover -} GADTs {- in isolation. This is very basic,
and we'll see -} where {- they really shine when we come to things like
ConstraintKinds and DataKinds. -}

---

{-
  In standard Haskell, we have the notion of a homogeneous list. That is to
  say, a list in which all the values are of the same type:
-}

data List a = Nil | Cons a (List a)

{-
  This is fine, and useful enough. We can, for example, write a 'Show'
  instance for this:
-}

instance Show a => Show (List a) where
  show  Nil             = "[]"
  show (Cons head tail) = show head ++ " : " ++ show tail

{-
  With this, we can show a list of values as long as the type of its values
  can itself be shown. The example below, for example, yields "2 : 3 : []".
-}

example1 :: String
example1 = show (Cons 2 (Cons 3 Nil))

{-
  What we get from a standard algebraic data type declaration, such as
  'List', is a /function/ for each of its constructors. By writing the 'List'
  declaration above, we have brought the following functions into scope:

  Nil  :: List a
  Cons :: a -> List a -> List a

  OK, 'Nil' isn't really a function, but you get the picture: we take the
  constructors' arguments as inputs, and produce a value of the type as the
  output. However, we should notice some restrictions:

  - We can't mention type variables in the arguments unless they're also in
    the result.

  - We can't /constrain/ the values of the input using any typeclasses.

  Usually, this is fine, but sometimes it's inconvenient. For example, if we
  know we're going to be 'show'ing everything in a list, why do we need all
  members of that list to be of the same type? Surely, all we actually need is
  a guarantee that the members all have a 'Show' instance:

  ShowNil  :: ShowList
  ShowCons :: Show a => a -> ShowList -> ShowList

  Notice here that we've broken both of the above rules:

  - We mention a variable, @a@, that doesn't exist in the result type,
    @ShowList@.

  - We constraint that @a@ value with the 'Show' class.

  This is a rather contrived example, and better ones will emerge as we get
  further through the exercises, so bear with me. In order to build a data type
  that accurately captures our requirement, we need to /generalise/ the notion
  of an algebraic data type. Luckily, a well-named solution exists: GADTs, or
  generalised algebraic data types!
-}

data ShowList where
  ShowNil  :: ShowList
  ShowCons :: Show a => a -> ShowList -> ShowList

{-
  Notice here that we've written /exactly/ the functions we wanted. We've said
  that a 'ShowList' is either a 'ShowNil', /or/ a 'ShowCons' of some @a@ type
  and some other 'ShowList'.

  When we come to write an instance for 'Show', we'll pattern-match just as we
  did before. However, something is different this time: when we pattern-match
  on @a@, we actually have no idea what its type is! Because it's not in the
  result type, we have no information to figure it out. We can know the @a@
  inside a @List a@ by looking at the type, but this type doesn't have @a@ in
  it!

  In fact, from the available information, all we can actually know is that it
  has a 'Show' instance. Beyond that, it could be /anything/. Luckily, that's
  the only thing we actually need:
-}

instance Show ShowList where
  show  ShowNil             = "[]"
  show (ShowCons head tail) = show head ++ " : " ++ show tail

{-
  Note that the implementation is /identical/ to 'List'! The only difference
  here is that our list's inhabitants don't have to be the same type. As long
  as our type has a 'Show' instance, we can add it to the list:
-}

example2 :: String
example2 = show (ShowCons "Tom" (ShowCons 25 (ShowCons True ShowNil)))
--              This is the "do you like dogs?" flag :) ^

{-
  This time, we get "\"Tom\" : 25 : True : []". All our types are different, but
  they all have 'Show' instances, and that's enough to write a 'Show' instance
  for the whole list.

  In more technical terms, we say the @a@ in our 'ShowCons' constructor is an
  /existential/ variable: it only exists within the scope of the constructing
  function. Outside of that, we have no idea what it could be! All we know
  about it is what's included in the context of the GADT. When you pattern-
  match on the constructor, as we did in its 'Show' instance, we get two
  variables in scope:

  @
    head :: Show a => a
    tail :: ShowList
  @

  We don't know what @a@ is. If you try to do something with it like (+1),
  you'll get a type error saying that we can't deduce a 'Num' instance for @a@.
  If you try to call @head == head@, you'll get an error saying we can't deduce
  an 'Eq' instance. We know /nothing/ about this type, other than that it has
  an instance of 'Show'. As a result, we can only call it with things that work
  for any 'Show' type. In practical terms, this boils down to one function (and
  any function built on top of it): 'show'. All we can do with this type is
  'show' it. Once we've done that, we have a String, and we can do anything we
  could normally do to a string, but that's it! To work with an /existential/
  variable, we have to use functions that work /for all/ its possible types.
-}

-- showListHead :: Show a => ShowList -> Maybe a
-- showListHead  ShowNil          = Nothing
-- showListHead (ShowCons head _) = Just head

{-
  Uncomment the above function - can you see why it doesn't work?

  Here, we're saying something /slightly/ different to what we want: we're
  saying that the caller gets to "pick" what @a@ is. For example, I could write
  something like:

  @
    test :: ShowList -> Maybe Int
    test = showListHead
  @

  Note that 'Int' is a type that implements 'Show', so, if 'showListHead'
  compiled, this would work - as the person calling this function, I've picked
  @a@ to be 'Int'. This idea of who "picks" types will be covered later on when
  we cover higher-rank polymorphism. For now, all we can do is show the head:
-}

showListHead' :: ShowList -> Maybe String
showListHead'  ShowNil          = Nothing
showListHead' (ShowCons head _) = Just (show head)
