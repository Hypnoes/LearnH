import Data.List
import Data.Maybe

-- The empty board
emptyBoard = ["...","...","..."]

-- Returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
    | a == b && b == c && a /= '.'   = a
    | d == e && e == f && d /= '.'   = d
    | g == f && f == i && g /= '.'   = g
    | a == d && d == g && a /= '.'   = a
    | b == e && e == c && b /= '.'   = b
    | c == f && f == i && c /= '.'   = c
    | a == e && e == i && a /= '.'   = a
    | c == e && e == g && c /= '.'   = c
    | '.' `elem` [a,b,c,d,e,f,g,h,i] = '?' -- Game not over
    | otherwise                      = '_' -- Tie

-- Replaces the ith element of a list
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace i a (x:xs) = x: replace (i - 1) a xs
        
-- Replaces the character at position r, c in board
play :: Int -> Int -> Char -> [[Char]] -> [[Char]]
play r c a board = replace r (replace c a (board !! r)) board

-- Returns the value of board with player to play
value :: Char -> [[Char]] -> Int
value player board
    | w == 'X'      = 1
    | w == 'O'      = -1
    | w == '-'      = 0
    | player == 'X' = maximum [value 'O' (play r c 'X' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    | otherwise     = minimum [value 'X' (play r c 'O' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    where w = winner board

-- Returns, from a list of boards where X has just moved, the one with the max value
bestOf :: [[[Char]]] -> [[Char]]
bestOf [x] = x
bestOf (x:xs)
    | value 'O' x > value 'O' bxs = x
    | otherwise                   = bxs
    where bxs = bestOf xs

-- Returns board after X's best move
bestMove :: [[Char]] -> [[Char]]
bestMove board = bestOf [play r c 'X' board | r <- [0..2], c <- [0..2], board !! r !! c == '.']
