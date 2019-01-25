{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Fix the following classes by annotating them with kind signatures. GHC
-- will tell you exactly what the problem is, so be sure to try /before/
-- uncommenting!





{- ONE -}

class Question1 (a :: Type -> Type)
instance Question1 Maybe





{- TWO -}

class Question2 (a :: Type -> Type -> Type)
instance Question2 Either





{- THREE -}

class Question3 (a :: Type -> Type -> Type -> Type)
instance Question3 (,,)





{- FOUR -}

class Question4 (a :: Type -> Type -> Type)
instance Question4 (->)





{- FIVE -}

class Question5 (a :: Type)
instance Question5 [[Int]]





{- SIX -}

class Question6 (a :: Type)
instance Question6 (Either () (Maybe (IO String)))





{- SEVEN -}

class Question7 (a :: Constraint)
instance Question7 (Eq Int) -- This instance looks flexible...





{- EIGHT -}

class Question8 (a :: Type -> Constraint)
instance Question8 Show





{- NINE -}

class Question9 (a :: Constraint)
instance Question9 (Functor Maybe)





{- TEN -}

-- A kind error! Assume two out of three of these have the correct kind; what
-- should it be? Which part is broken? What's wrong with it?

class Question10 (a :: Constraint)
instance Question10 (Monad m, Eq (m a), Show a)
