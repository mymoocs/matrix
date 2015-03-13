{-# OPTIONS_GHC -Wall #-}

module GF2 where

data GF2 = One | Zero deriving (Show, Eq)


add :: GF2 -> GF2 -> GF2
add One One = Zero
add Zero a = a
add One _  = One


mul :: GF2 -> GF2 -> GF2
mul Zero _  = Zero
mul One a   = a

(</>) :: GF2 -> GF2 -> Maybe GF2
(</>)  _ Zero = Nothing
(</>) a _    = Just a 

one :: GF2
one = One

zero :: GF2
zero = Zero

infix 5 </>

instance Num GF2 where
  (+) = add
  (*) = mul
  (-) = add
  fromInteger i |i==0 = Zero
                |otherwise = One
  
