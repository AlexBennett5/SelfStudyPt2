module UnitTest(tests) where

import Units
import Test.HUnit

unitComp :: (Show a, Eq a) => a -> Quantity -> Test
unitComp msg a q = Testcase $ assertEqual msg a q

tests :: Test
tests = Test [ unitComp "Simple unit test" (1 # (meter, [sec, sec])) (Quantity 1 (Unit meter [sec, sec]))
             , unitComp "Diffless multiplication" (2 # (diffless) * 1 # (meter, [sec, sec])) (Quantity 2 (Unit meter [sec, sec]))
             , unitComp "Multiplication changes unit" (5 # (meter, sec) * 5 # (meter, sec)) (Quantity 25 (Unit [meter, meter] [sec, sec]))
             , unitComp "Multiplication cancels unit" (5 # (meter, sec) * 5 # (sec, meter)) (Quantity 25 (Unit [] []))
             , unitComp "Division changes unit" (3 # (meter, sec) / 3 # (sec, meter)) (Quantity 1 (Unit [meter, meter] [sec, sec]))
             , unitComp "Division cancels unit" (3 # (meter, sec) / 3 # (meter, sec)) (Quantity 1 (Unit [] []))
             , unitComp "Newton test" (5 # (newton)) (Quantity 5 (Unit [kilo, gram, meter] [sec, sec]))
             , unitComp "Addition preserves unit" (4 # (meter) + 6 # (meter)) (Quantity 10 (Unit meter []))
             ]
