import Test.HUnit
import SimpleVecTest

allTests :: Test
allTests = SimpleVecTest.tests

main :: IO Counts
main = do runTestTT allTests
