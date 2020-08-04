# GHC exercises 🚀

_For solutions, please see the `Exercises.hs` modules inside [the `answers`
branch](https://github.com/i-am-tom/haskell-exercises/tree/answers)._

OK, you know your **monoids** from your **monads**. You know how to write a
terminal application or two. What's next? What are these language extensions
that keep being mentioned? How "type-safe" can you _really_ be?

This repository, I hope, will provide some **stepping stones**. We'll go
through extensions one at a time, and build up a richer vocabulary for talking
about Haskell programs, and look to move our assertions up into types, where
they can be verified at compile time.

---

## What this _isn't_.

This is a deep dive into GHC extensions, the power that each one gives us, and
how we can combine extensions to achieve very strong guarantees at
compile-time. This is _not_ based around concepts; there won't be sections on
"dependently-typed programming", or "generic programming", though these
concepts will turn up throughout as we dig deeper into the extensions.

> If you're interested in something more project-based, I absolutely, 1000%
> recommend [Thinking with Types](https://leanpub.com/thinking-with-types), 
> written by [Sandy Maguire](https://github.com/isovector). It is a
> **fantastic** resource, and one on which I already rely when explaining
> concepts to others.

## Contents

1.  `GADTs`
2.  `FlexibleInstances`
3.  `KindSignatures`
4.  `DataKinds`
5.  `RankNTypes`
6.  `TypeFamilies`
7.  `ConstraintKinds`
8.  `PolyKinds`
9.  `MultiParamTypeClasses`
10. `FunctionalDependencies`

## Setup

Assuming you have [Cabal](https://www.haskell.org/cabal/) or
[Stack](https://docs.haskellstack.org/en/stable/README/) setup, you should be
able to navigate to any of the `exercise*` directories, and run your usual
commands:

### Repl

```
$ stack repl
$ cabal repl
```

### Build

```
$ stack build
$ cabal build
```

It's going to make it a lot easier to iterate through the exercises if you
`cabal install ghcid` or `stack install ghcid`. Just as above, once this is
done, you can navigate to the exercise directory and run it with your preferred
repl command:

```
$ ghcid -c "stack repl"
$ ghcid -c "cabal repl"
```
