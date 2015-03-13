{-# OPTIONS_GHC -Wall #-}

module Mat where

data Mat a b = Mat { getDomainR :: (S.Set a)
                   , getDomainC :: (S.Set b)
                   , getFun :: (M.Map (a,b) Integer)
                   }
             deriving (Show)




