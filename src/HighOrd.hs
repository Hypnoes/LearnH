add1 :: Int -> Int
add1 x = x + 1

f :: (Int -> Int) -> Int
f x = 3

g :: Int -> (Int -> Int)
g x = add1
