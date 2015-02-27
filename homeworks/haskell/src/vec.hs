
{-# OPTIONS_GHC -Wall #-}

module Vec where

import qualified Data.Set             as S
import qualified Data.Map             as M
import qualified Data.Vector          as V
import           Data.Vector (Vector)
import qualified Test.HUnit           as T
import qualified Test.HUnit.Util      as U

-- import qualified Test.HUnit      as T
-- import qualified Test.HUnit.Util as U

-- [[https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-map]]

import Prelude hiding (replicate, enumFromTo, enumFromThenTo, length, null)

data Vec a = Vec {getDomain :: (S.Set a), getFun :: (M.Map a Int)}
           deriving (Show, Eq)

--instance Show (Vec ) where
--  show v = "Vec({"++ show ((S.toList $ d v)::[Char]) ++ "}," ++ show (M.toList $ f v) ++  ")"

-- Vec({8, 2, 4, 6},{8: -4, 2: -1, 4: -2, 6: -3})

phoneBook :: M.Map Integer [Char]
phoneBook = M.fromList [(1234, "Erik"), (5678, "Patrik")]

vector :: Vector Integer
vector = V.fromList [1..10]

set :: S.Set Char
set  = S.fromList "erik salaj"

set1 :: S.Set Char
set1 = S.fromList "aaaaabbbb"

------------------------------------------------------------
--  1. getItem
------------------------------------------------------------
-- | Return the value of entry k in v.
-- | Be sure getitem v k  returns 0 if k is not represented in v.f.
-- | v = Vec ({'a','b','c','d'},{'a':2,'c':1,'d':3})
-- >>> let v = Vec (S.fromList "abcd") (M.fromList [('a', 2), ('c', 1), ('d', 3)])
-- >>> getItem v 'd'
-- 3
-- >>> getItem v 'b'
-- 0


getItem :: Ord k => Vec k -> k -> Int
getItem v k = let dict = getFun v
              in case M.lookup k dict of
                  Nothing -> 0
                  Just x  -> x
getItem' :: (Ord k) => Vec k -> k -> Int
getItem' v k = let dict = getFun v
              in M.findWithDefault 0 k dict  

vec :: Vec Char
vec = Vec (S.fromList "abcd") (M.fromList [('a', 2), ('c', 1), ('d', 3)])

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e10" (getItem vec 'c') 1
    , U.teq "e11" (getItem vec 'a') 2
    , U.teq "e12" (getItem vec 'b') 0
    ]

------------------------------------------------------------
--  2. setItem
------------------------------------------------------------
-- | Set the element of v with label d to be val.
-- | setitem(v,d,val) should set the value for key d even if d
-- | is not previously represented in v.f.

-- >>> let v = Vec({'a', 'b', 'c'}, {'b':0})
-- >>> let v = setItem v 'b' 5
-- >>> getItem v 'b'
-- 5
-- >>> getItem (setItem v 'a' 1) 'a'
-- 1

setItem ::Ord a =>  Vec a -> a -> Int -> Vec a
setItem vec k value = Vec domain (M.insert k value dict)
  where
    dict = getFun vec
    domain = getDomain vec
  
ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (getItem (setItem vec 'a' 1) 'a') 1
    ]


equal :: Ord k => Vec k -> Vec k -> Bool
equal v1 v2 = and [getItem v1 k == getItem v2 k | k <- allKeys]
  where
    m1 = getFun v1
    m2 = getFun v2
    allKeys = S.toList $ S.union (M.keysSet m1) (M.keysSet m2)      
  

hw1 :: IO T.Counts
hw1 = do
  _ <- T.runTestTT ex1
  T.runTestTT ex2

  

--main = do
--  _ <- ex_set
--  _ <- ex_vector
--  ex_map
  
{- ex_set = do
    print "---Set example"
    print set
    print set1
    print $ S.null set
    print $ S.size set
    print $ S.member 'a' set

ex_vector  = do
    print "-----------------------------------"

    print $ (V.empty :: Vector Char)
    print $ V.singleton 'a'
    print $ V.replicate 10 'b'
    print $ V.generate 10 (* 2)
    print $ V.iterateN 10 (+ 1) 100
    print $ V.enumFromN 10 5
    print $ V.enumFromStepN 2 3 10
    print $ V.enumFromTo 10 20
    print $ V.enumFromThenTo 10 12 20

    print $ V.fromList [1..5]
    print $ V.fromListN 3 [1..5]
    print $ V.toList $ V.replicate 10 'c'

    

    print $ V.length vector
    print $ V.null vector
    print $ V.null $ V.fromList []
    print $ vector V.! 0
    print $ vector V.!? 0
    print $ vector V.!? 10




ex_map = do
    print phoneBook
    print $ M.lookup 1234 phoneBook
    print $ (M.empty :: M.Map Int Int)
    print $ M.singleton 3 5
    print $ M.insert 1 "abc" M.empty
    print $ M.null phoneBook
    print $ M.size phoneBook
    print $ M.toList phoneBook
    print $ M.keys phoneBook
    print $ M.elems phoneBook


-}

