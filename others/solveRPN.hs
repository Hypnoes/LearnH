solveRPN :: String -> Double
solveRPN expression = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (y * x):ys
            foldingFunction (x:y:ys) "+" = (y + x):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ^ x):ys
            foldingFunction xs numberString = read numberString:xs
