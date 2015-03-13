  
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

data Vec a = Vec {getDomain :: (S.Set a), getFun :: (M.Map a Integer)}
           deriving (Show)

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

vec :: Vec Char
vec = Vec (S.fromList "abcd") (M.fromList [('a', 2), ('c', 1), ('d', 3)])
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


getItem :: Ord k => Vec k -> k -> Integer
getItem v k = let dict = getFun v
              in case M.lookup k dict of
                  Nothing -> 0
                  Just x  -> x
getItem' :: (Ord k) => Vec k -> k -> Integer
getItem' v k = let dict = getFun v
              in M.findWithDefault 0 k dict  



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

setItem ::Ord a =>  Vec a -> a -> Integer -> Vec a
setItem vec k value = Vec domain (M.insert k value dict)
  where
    dict = getFun vec
    domain = getDomain vec
  
ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (getItem (setItem vec 'a' 1) 'a') 1
    ]

------------------------------------------------------------
--  3. equal
------------------------------------------------------------

-- | check if two vectors are equal
-- >>> equal  (Vec (S.fromList "abc") (M.fromList [('a', 0)])) (Vec (S.fromList "abc") (M.fromList [('b', 0)]))
-- True
-- >>> equal (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('x', 2)])) (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('z', 0)]))
-- False
-- >>> Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1)]) == Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1), ('b', 4)])
-- False
-- >>> Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1), ('b', 4)]) == Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1)])
-- False
-- >>> Vec (S.fromList "ab") (M.fromList [('a', 1)]) ==  Vec (S.fromList "ab") (M.fromList [('b', 1)])
-- False
-- >>> Vec (S.fromList "ab") (M.fromList [('a', 1)]) ==  Vec (S.fromList "ab") (M.fromList [('a', 2)])
-- False

equal :: Ord k => Vec k -> Vec k -> Bool
equal v1 v2 = and [getItem v1 k == getItem v2 k | k <- allKeys]
  where
    m1 = getFun v1
    m2 = getFun v2
    allKeys = S.toList $ S.union (M.keysSet m1) (M.keysSet m2)      


instance Ord  a => Eq (Vec a) where
  (==) = equal

ex3 :: T.Test
ex3 = T.TestList
    [
        U.teq "e30" (equal vec vec) True
      , U.teq "e31" (equal (setItem vec 'a' 1) vec) False
      , U.teq "e32" (vec == vec) True
      , U.teq "e33" ((setItem vec 'a' 1) == vec) False
      , U.teq "e34" (equal (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('x', 2)]))
                     (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('z', 0)]))) False
      , U.teq "e35" (equal  (Vec (S.fromList "abc") (M.fromList [('a', 0)]))
                     (Vec (S.fromList "abc") (M.fromList [('b', 0)]))) True
      , U.teq "e36" ((==) (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('x', 2)]))
                     (Vec (S.fromList "xyz") (M.fromList [('y', 1), ('z', 0)]))) False
        -- doctests
      , U.teq "e37" ((==) (Vec (S.fromList "abc") (M.fromList [('a', 0)]))
                     (Vec (S.fromList "abc") (M.fromList [('b', 0)]))) True
      , U.teq "e38" (Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1)])
                     == Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1), ('b', 4)])) False
      , U.teq "e39" (Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1), ('b', 4)])
                     == Vec (S.fromList "abc") (M.fromList [('a', 0), ('c', 1)])) False
      , U.teq "e40" (Vec (S.fromList "ab") (M.fromList [('a', 1)]) 
                     ==  Vec (S.fromList "ab") (M.fromList [('b', 1)])) False
      , U.teq "e41" (Vec (S.fromList "ab") (M.fromList [('a', 1)])
                     ==  Vec (S.fromList "ab") (M.fromList [('a', 2)])) False
    ]
    

------------------------------------------------------------
--  4. +, *, - operator overloading
------------------------------------------------------------

applyVecOperator 
  :: Ord k =>
     (Integer -> Integer -> Integer)
     -> Vec k
     -> Vec k
     -> Vec k
applyVecOperator op v1 v2 =
  Vec (getDomain v1)  (M.fromList [(k, op  (getItem v1 k) (getItem v2 k)) | k <- allKeys])
  where
    f1 = getFun v1
    f2 = getFun v2
    allKeys = S.toList $ S.union (M.keysSet f1) (M.keysSet f2)

(<*>) :: Ord a => Vec a -> Integer -> Vec a
(<*>) v alpha =
  Vec (getDomain v) (M.fromList [(k, getItem v k * alpha ) | k <- S.toList $ M.keysSet (getFun v)])

infixr 5 <*>   

instance  Ord a => Num (Vec a) where
   negate v =  v <*> (-1)
--   fromInteger i = Vec (S.fromList (show i)) (M.fromList [('1', fromInteger i)])
   v1 + v2 = applyVecOperator (+) v1 v2
   v1 * v2 = applyVecOperator (*) v1 v2
   v1 - v2 = v1 + (-v2)
   -- abs = sqrt . sum . map (\(k,v)->v) . M.toList . getFun
     
--   v1 / v2 = scalarMul v (-1)
-- TODO make high leve function and use for +, * on vectors, equal
       
--   Pair (a,b) * Pair (c,d) = Pair (a*c,b*d)
  -- Pair (a,b) - Pair (c,d) = Pair (a-c,b-d)
  -- abs    (Pair (a,b)) = Pair (abs a,    abs b) 
  -- signum (Pair (a,b)) = Pair (signum a, signum b) 
  -- fromInteger i = Pair (fromInteger i, fromInteger i)

vectorTests :: IO T.Counts
vectorTests = do
  _ <- T.runTestTT ex1
  _ <- T.runTestTT ex2
  T.runTestTT ex3

  

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

