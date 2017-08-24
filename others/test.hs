main :: IO ()
main = putStrLn (greet "world")

greeting = "Hello"

greet :: String -> String
greet who = greeting ++ ", " ++ who

add :: (Num a) => a -> a -> a
add a b = a + b

data Compass = North | South | East | West
    deriving (Eq, Enum, Show)

data MyType = Record {} | Algebraic
