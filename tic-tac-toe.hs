-- λ

import Data.List (transpose)
import Data.Char (isDigit)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

size :: Int
size = 3

data Player = O | B | X
            deriving (Eq, Ord, Show)
type Grid = [[Player]]

next :: Player -> Player
next X = O
next O = X
next B = B

empty :: Grid
empty = replicate size $ replicate size B

g :: Grid
g = [[X, O, B], [O, X, O], [O, B, X]]

isFull :: Grid -> Bool
isFull = all (notElem B)

turn :: Grid -> Player
turn g = if xs < os then X else O where
  xs = length $ filter (== X) ps
  os = length $ filter (== O) ps
  ps = concat g

hasWon :: Grid -> Player -> Bool
hasWon g p = any line (rows ++ cols ++ dias) where
  line = all (== p)
  rows = g
  cols = transpose g
  diag g' = [g' !! n !! n | n <- [0..size-1]]
  dias = [diag g, diag $ map reverse g]

won :: Grid -> Bool
won g = hasWon g X || hasWon g O

showPlayer :: Player -> [String]
showPlayer X = ["   ", " X ", "   "]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", " B ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

validIndex :: Grid -> Int -> Bool
validIndex g i = i >= 0 && i < size^2  && concat g !! i == B

move :: Grid -> Int -> Player -> Maybe Grid
move g i p = let ri = i `div` size
                 ci = i `mod` size in
  if validIndex g i then
    Just $ replaceAt g ri $ replaceAt (g !! ri) ci p
  else Nothing
  where
    replaceAt :: [a] -> Int -> a -> [a]
    replaceAt xs i x = take i xs ++ [x] ++ drop (i+1) xs

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else do putStrLn "ERROR: Invalid number"
                           getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move\nλ> "

run' :: Grid -> Player -> IO ()
run' g p | hasWon g O  = putStrLn "Player O wins!\n"
         | hasWon g X  = putStrLn "Player X wins!\n"
         | isFull g    = putStrLn "It's a draw!\n"
         | otherwise = do
             i <- getNat $ prompt p
             case move g i p of
               Nothing -> do putStrLn "ERROR: Invalid move"
                             run' g p
               Just g' -> run g' (next p)

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1, 1)
             putGrid g
             run' g p

tictactoe :: IO ()
tictactoe = run empty O

main :: IO ()
main = tictactoe
