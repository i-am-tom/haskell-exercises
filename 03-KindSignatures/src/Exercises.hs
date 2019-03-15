{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Fix the following classes by annotating them with kind signatures. GHC
-- will tell you exactly what the problem is, so be sure to try /before/
-- uncommenting!





{- ONE -}

class Question1 a
-- instance Question1 Maybe





{- TWO -}

class Question2 a
-- instance Question2 Either





{- THREE -}

class Question3 a
-- instance Question3 (,,)





{- FOUR -}

class Question4 a
-- instance Question4 (->)





{- FIVE -}

class Question5 a
-- instance Question5 [[Int]]





{- SIX -}

class Question6 a
-- instance Question6 (Either () (Maybe (IO String)))





{- SEVEN -}

class Question7 a
-- instance Question7 (Eq Int) -- This instance looks flexible...





{- EIGHT -}

class Question8 a
-- instance Question8 Show





{- NINE -}

class Question9 a
-- instance Question9 (Functor Maybe)





{- TEN -}

-- A kind error! Assume two out of three of these have the correct kind; what
-- should it be? Which part is broken? What's wrong with it?

class Question10 a
-- instance Question10 (Monad m, Eq m, Show a)
