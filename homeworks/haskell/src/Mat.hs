{-# OPTIONS_GHC -Wall #-}

module Mat where

import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Test.HUnit       as T
import qualified Test.HUnit.Util  as U

data Mat a b = Mat { getDomainR :: (S.Set a)
                   , getDomainC :: (S.Set b)
                   , getFun :: (M.Map (a,b) Integer)
                   }
             deriving (Show)

------------------------------------------------------------
--  1. equal of Matrix
------------------------------------------------------------
-- | Returns true iff A is equal to B.
-- >>> Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('a',1),0)]) == Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('b',1),0)])
-- True
-- >>> let a = Mat (S.fromList ['a','b']) (S.fromList [0,1])  (M.fromList [(('a',1),2), (('b',0),1)])
-- >>> let b  = Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('a',1),2), (('b',0),1), (('b',1),0)])
-- >>> let c = Mat (S.fromList ['a','b']) (S.fromList [0,1])  (M.fromList [(('a',1),2), (('b',0),1), (('b',1),5)])
-- >>> a == b
-- True
-- >>> a == c
-- False
-- >>> a == Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('a',1),2), (('b',0),1)])
-- True

equal mat1 mat2 =
  and [getItem mat1 i j == getItem mat2 i j | (i,j) <- allKeys]
  where
    k1 = getFun mat1
    k2 = getFun mat2
    allKeys = S.toList $ S.union (M.keysSet k1) (M.keysSet k2)      


ex1 :: T.Test
ex1 = T.TestList
      [
        U.teq "ex10" (Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('a',1),0)])
                      == Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('b',1),0)])) True
      
       
      ]


------------------------------------------------------------
--  2. getItem for Matrix
------------------------------------------------------------
-- |
-- Returns the value of entry i and j in m, where i is row and j is column
-- >>>let m = Mat (S.fromList [1,3,5]) (S.fromList ['a']) (M.fromList [((1,'a'),4), ((5,'a'),2)])
-- >>>getItem m 1 'a'
-- 4
-- >>>getItem m 3 'a' 
-- 0

getItem :: (Ord a, Ord b) => Mat a b -> a -> b -> Integer
getItem m i j = let dict = getFun m
                in case M.lookup (i,j) dict of
                    Nothing -> 0
                    Just x  -> x
mat :: Mat Integer Char 
mat = Mat (S.fromList [1,3,5])
          (S.fromList ['a'])
          (M.fromList [((1,'a'),4), ((5,'a'),2)])
ex2 :: T.Test
ex2 = T.TestList
      [
        U.teq "ex20" (getItem mat 1 'a') 4
      , U.teq "ex21" (getItem mat 3 'a') 0
      ]

------------------------------------------------------------
--  3. setItem for Matrix
------------------------------------------------------------
-- |
-- Set entry k of Mat M to val, where k is a 2-tuple.
-- >>>let m = Mat (S.fromList ['a','b','c']) (S.fromList [5]) (M.fromList [(('a', 5),3), (('b', 5),7)])
-- >>>let m1 =  setItem m 'b'  5 9
-- >>>let m2 =  setItem m1 'c'  5 13
-- >>> m2 == Mat (S.fromList ['a','b','c']) (S.fromList [5]) (M.fromList [(('a', 5),3), (('b', 5),9), (('c',5),13)])
-- True
-- >>> let n = Mat (S.fromList ["((),)", "7"]) (S.fromList [True, False]) (M.fromList [])
-- >>> let n1 = setItem n "7" False 1
-- >>> let n2 = setItem n1 "((),)" True 2
-- >>> n2 == Mat (S.fromList ["((),)", "7"]) (S.fromList [True, False]) (M.fromList [(("7",False),1), (("((),)", True),2)])
-- True

setItem :: (Ord a, Ord b) => Mat a b -> a -> b -> Integer -> Mat a b
setItem mat i j value = Mat domainR domainC (M.insert (i,j) value dict)
  where
    dict = getFun mat
    domainR = getDomainR mat
    domainC = getDomainC mat

mat3 :: Mat Char Integer
mat3 = Mat (S.fromList ['a','b','c']) (S.fromList [5]) (M.fromList [(('a', 5),3), (('b', 5),7)])

mat31 :: Mat String Bool
mat31 = Mat (S.fromList ["((),)", "7"]) (S.fromList [True, False]) (M.fromList [])

ex3 :: T.Test
ex3 = T.TestList
      [
        U.teq "ex30" (setItem (setItem mat3 'b' 5 9) 'c' 5 13)
        (Mat (S.fromList ['a','b','c']) (S.fromList [5]) (M.fromList [(('a', 5),3), (('b', 5),9), (('c',5),13)]))
      , U.teq "ex31" (setItem (setItem mat31 "7" False 1) "((),)" True 2)
        (Mat (S.fromList ["((),)", "7"]) (S.fromList [True, False]) (M.fromList [(("7",False),1), (("((),)", True),2)]))
      ]





instance (Ord a, Ord b) => Eq (Mat a b) where
  (==) = equal




matrixTests :: IO T.Counts
matrixTests = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex3
