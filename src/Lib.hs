module Lib
    ( someFunc
    , testString
    ) where

someFunc :: IO ()
someFunc = putStrLn testString

testString :: String
testString = "someFunc"
