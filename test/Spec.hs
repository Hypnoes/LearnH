-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

 import Test.Hspec
 import Lib

 main :: IO ()
 main = hspec $ 
    describe "How to write a test" $ 
        it "Should be able to run tests" $ 
            testString `shouldBe` "someFunc"
