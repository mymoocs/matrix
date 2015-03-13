
module Main where

import    Test.HUnit.Base
import    Vec
import    GF2
import Mat

main :: IO Counts
main = do
  print "Vector tests"
  vectorTests
  print "Matrix tests"
  matrixTests

    
   
-- End of file.
