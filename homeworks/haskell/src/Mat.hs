{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

equal :: (Ord a, Ord b) => Mat a b -> Mat a b -> Bool
equal mat1 mat2 =
  and [getItem mat1 i j == getItem mat2 i j | (i,j) <- allKeys]
  where
    k1 = getFun mat1
    k2 = getFun mat2
    allKeys = S.toList $ S.union (M.keysSet k1) (M.keysSet k2)      


ex1 :: T.Test
ex1 = T.TestList
      [
        U.teq "ex10" (m1 == m2) True
      ]
      where
        m1 = Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('a',1),0)])
        m2 = Mat (S.fromList ['a','b']) (S.fromList [0,1]) (M.fromList [(('b',1),0)]) 


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


------------------------------------------------------------
--  4. add two matrix
------------------------------------------------------------
-- |
-- Return the sum of Mats A and B.
-- >>>let a1 = Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'x'),-2), ((6,'y'),3)])
-- >>>let a2 = Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'y'),4)])
-- >>>let b =  Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'x'),-2), ((3,'y'),4), ((6,'y'),3)])
-- >>> add a1 a2 == b
-- True
-- >>> add a2 a1 == b
-- True
-- >>> a1 == Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'x'),-2), ((6,'y'),3)])
-- True
-- >>> let zero = Mat (S.fromList [3,6]) (S.fromList ['x','y']) (M.fromList [])
-- >>> add b zero == b
-- True
-- >>>let c1 = Mat (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,2),2), ((3,4),3)])
-- >>>let c2 = Mat (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,4),1), ((1,2),4)])
-- >>>let d = Mat  (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,2),6), ((1,4),1), ((3,4),3)])
-- >>>add c1 c2 == d
-- True

add mat1 mat2 =
  Mat (getDomainR mat1) (getDomainC mat1) (M.fromList [((i,j), (getItem mat1 i j) + (getItem mat2 i j)) | (i,j) <- allKeys])
  where
    f1 = getFun mat1
    f2 = getFun mat2
    allKeys = S.toList $ S.union (M.keysSet f1) (M.keysSet f2)

ex4 :: T.Test
ex4 = T.TestList
      [
        U.teq "ex40" (a1 + a2) b
      , U.teq "ex41" (a2 + a1) b
      , U.teq "ex42" (b + zero) b
      , U.teq "ex43" (c1 + c2) d
      , U.teq "ex44" (c2 + c1) d
      ]
      where
        a1 = Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'x'),-2), ((6,'y'),3)])
        a2 = Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'y'),4)])
        b =  Mat (S.fromList [3, 6]) (S.fromList ['x','y']) (M.fromList [((3,'x'),-2), ((3,'y'),4), ((6,'y'),3)])
        zero = Mat (S.fromList [3,6]) (S.fromList ['x','y']) (M.fromList [])
        c1 = Mat (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,2),2), ((3,4),3)])
        c2 = Mat (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,4),1), ((1,2),4)])
        d = Mat (S.fromList [1,3]) (S.fromList [2,4]) (M.fromList [((1,2),6), ((1,4),1), ((3,4),3)]) 


------------------------------------------------------------
--  5. mul two matrix
------------------------------------------------------------

-- |
-- Returns the result of the matrix-matrix multiplication, A*B.
-- >>>let a = Mat(({0,1,2}, {0,1,2}), {(1,1):4, (0,0):0, (1,2):1, (1,0):5, (0,1):3, (0,2):2})
-- >>>let b = Mat(({0,1,2}, {0,1,2}), {(1,0):5, (2,1):3, (1,1):2, (2,0):0, (0,0):1, (0,1):4})
-- >>> a*b == Mat(({0,1,2}, {0,1,2}), {(0,0):15, (0,1):12, (1,0):25, (1,1):31})
-- True
-- >>> let c = Mat(({0,1,2}, {'a','b'}), {(0,'a'):4, (0,'b'):-3, (1,'a'):1, (2,'a'):1, (2,'b'):-2})
-- >>> D = Mat(({'a','b'}, {'x','y'}), {('a','x'):3, ('a','y'):-2, ('b','x'):4, ('b','y'):-1})
-- >>> C*D == Mat(({0,1,2}, {'x','y'}), {(0,'y'):-5, (1,'x'):3, (1,'y'):-2, (2,'x'):-5})
-- True
-- >>> M = Mat(({0, 1}, {'a', 'c', 'b'}), {})
-- >>> N = Mat(({'a', 'c', 'b'}, {(1, 1), (2, 2)}), {})
-- >>> M*N == Mat(({0,1}, {(1,1), (2,2)}), {})
-- True
-- >>> E = Mat(({'a','b'},{'A','B'}), {('a','A'):1,('a','B'):2,('b','A'):3,('b','B'):4})
-- >>> F = Mat(({'A','B'},{'c','d'}),{('A','d'):5})
-- >>> E*F == Mat(({'a', 'b'}, {'d', 'c'}), {('b', 'd'): 15, ('a', 'd'): 5})
-- True
-- >>> F.transpose()*E.transpose() == Mat(({'d', 'c'}, {'a', 'b'}), {('d', 'b'): 15, ('d', 'a'): 5})
-- True
mul :: (Ord a, Ord b, Ord c) => Mat a b -> Mat b c -> Mat a c
-- mul :: (Ord a, Ord b) => Mat a b -> Mat b a -> Mat a a         
mul mat1 mat2 =
  Mat (getDomainR mat1) (getDomainC mat2)
  (M.fromList sumM)
  where
    f1 = getFun mat1
    f2 = getFun mat2
    rows = S.toList $ getDomainR mat1
    cols = S.toList $ getDomainC mat2
    krows = S.toList $ getDomainR mat2
    sumM = [((i,j), sum [(getItem mat1 i k) * (getItem mat2 k j)
                        | k <- krows]) | i <- rows, j <- cols]


-- type Pair a = (a,a)
ex5 :: T.Test
ex5 = T.TestList
      [
        U.teq "ex50" (mul a b) ab
      , U.teq "ex51" (mul c d) cd
      , U.teq "ex52" (mul m n) mn
      , U.teq "ex53" (mul e f) ef
      ]
      where
        a = Mat (S.fromList [0,1,2]) (S.fromList [0,1,2]) (M.fromList [((1,1),4), ((0,0),0), ((1,2),1), ((1,0),5), ((0,1),3), ((0,2),2)])
        b = Mat (S.fromList [0,1,2]) (S.fromList [0,1,2]) (M.fromList [((1,0),5), ((2,1),3), ((1,1),2), ((2,0),0), ((0,0),1), ((0,1),4)])
        ab = Mat(S.fromList [0,1,2]) (S.fromList [0,1,2]) (M.fromList [((0,0),15), ((0,1),12), ((1,0),25), ((1,1),31)])
        c = Mat (S.fromList [0,1,2]) (S.fromList ['a','b']) (M.fromList [((0,'a'),4), ((0,'b'),-3), ((1,'a'),1), ((2,'a'),1), ((2,'b'),-2)])
        d = Mat (S.fromList ['a','b']) (S.fromList ['x','y']) (M.fromList [(('a','x'),3), (('a','y'),-2), (('b','x'),4), (('b','y'),-1)])
        cd = Mat(S.fromList [0,1,2]) (S.fromList ['x','y']) (M.fromList [((0,'y'),-5), ((1,'x'),3), ((1,'y'),-2), ((2,'x'),-5)])
        m = Mat(S.fromList [0, 1]) (S.fromList ['a', 'c', 'b']) (M.fromList [])
       --  n :: Mat Char (Pair Integer)  
        n = Mat(S.fromList ['a', 'c', 'b']) (S.fromList [(1,1), (2,2)]) (M.fromList [])
        mn = Mat(S.fromList [0,1]) (S.fromList [(1,1), (2,2)]) (M.fromList [])
        e = Mat(S.fromList  ['a','b']) (S.fromList ['A','B']) (M.fromList [(('a','A'),1),(('a','B'),2),(('b','A'),3),(('b','B'),4)])
        f = Mat(S.fromList ['A','B']) (S.fromList ['c','d']) (M.fromList [(('A','d'),5)])
        ef = Mat(S.fromList ['a', 'b']) (S.fromList ['d', 'c']) (M.fromList [(('b', 'd'),15), (('a', 'd'), 5)])

-- >>> F.transpose()*E.transpose() == Mat(({'d', 'c'}, {'a', 'b'}), {('d', 'b'): 15, ('d', 'a'): 5})
-- True


instance (Ord a, Ord b) => Eq (Mat a b) where
  (==) = equal

instance (Ord a, Ord b) => Num (Mat a b) where
  (+) = add
--  (*) = undefined
--  abs = undefined
--  signum = undefined
--  fromInteger = undefined

-- http://en.wikibooks.org/wiki/Haskell/Advanced_type_classes  
-- look at this http://en.literateprograms.org/Matrix_multiplication_(Haskell)
-- http://stackoverflow.com/questions/7656948/matrix-constructor-and-method-in-haskell  
matrixTests :: IO T.Counts
matrixTests = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex3
  T.runTestTT ex4
  T.runTestTT ex5  
