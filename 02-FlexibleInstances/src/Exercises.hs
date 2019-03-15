module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

instance PopQuiz Bool
-- instance PopQuiz [Bool]
instance PopQuiz [a]
instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)]
instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)
instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a)
-- instance PopQuiz (r -> IO a)
instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- instance PopQuiz (a -> b -> c)
instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c))
instance PopQuiz ()
-- instance PopQuiz (a, b, c, a)

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)
instance PopQuiz (Pair a)
-- instance PopQuiz (Pair' a)
