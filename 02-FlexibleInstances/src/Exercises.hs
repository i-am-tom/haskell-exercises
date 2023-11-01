module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- instance PopQuiz [Bool]          -- < this one (inner concrete)
-- instance PopQuiz [a]
-- instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)]        -- < this one (inner concrete)
-- instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a)
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)     -- < this one (inner concrete)
-- instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a)      -- < this one (type synonym)
-- instance PopQuiz (r -> IO a)     -- < this one (inner concrete)
-- instance PopQuiz (a -> b)
-- instance PopQuiz (a -> b -> c)   -- < this one (inner concrete)
-- instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c))     -- < this one (inner concrete)
-- instance PopQuiz ()
-- instance PopQuiz (a, b, c, a)    -- < this one (eq constraint)

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)          -- < this one (eq constraint)
-- instance PopQuiz (Pair a)
-- instance PopQuiz (Pair' a)       -- < this one (type synonym)

-- | All correct!! :)