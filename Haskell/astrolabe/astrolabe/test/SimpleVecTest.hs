module SimpleVecTest(tests) where

import Vector
import Control.Exception
import Test.HUnit

vecBinopTest :: (Show c, Eq c) => String -> a -> b -> (a -> b -> c) -> c -> Test
vecBinopTest msg v1 v2 op vres = TestCase $ assertEqual msg vres (v1 `op` v2) 

normTest :: Test
normTest = TestCase $ assertEqual "Norm" 3  (norm $ Vec 1 2 2)

--testException :: String -> a -> Test
--testException msg ex f = TestCase $ assertRaises msg (ErrorCall ex) (evaluate f)

tests :: Test
tests = TestList [ vecBinopTest "Simple vector addition" (Vec 1 2 3) (Vec 2 2 2) (^+^) (Vec 3 4 5)
                 , vecBinopTest "Negative vector addition" (Vec 1 (-4) (-8)) (Vec 10 12 2) (^+^) (Vec 11 8 (-6))
                 , vecBinopTest "Simple vector subtraction" (Vec 0 0 0) (Vec 5 5 5) (^-^) (Vec (-5) (-5) (-5))
                 , vecBinopTest "Simple cross product" (Vec 3 (-3) 1) (Vec 4 9 2) (><) (Vec (-15) (-2) 39)
                 , vecBinopTest "Left scalar mult" 5 (Vec 1 2 1) (*^) (Vec 5 10 5)
                 , vecBinopTest "Right scalar mult" (Vec 1 2 1) 5 (^*) (Vec 5 10 5)
                 , vecBinopTest "Valid scalar division" (Vec 5 10 5) 5 (^/) (Vec 1 2 1)
                 , vecBinopTest "Dot product" (Vec 10 0 2) (Vec 4 103 3) (<.>) 46
                 , normTest
                 ]  

