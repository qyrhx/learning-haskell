-- λ

import Data.List (transpose)
import Data.Char (isDigit)
import Data.Maybe (fromJust, mapMaybe)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

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

-- MinMax

validMoves :: Grid -> Player -> [Grid]
validMoves g p
  | won g     = []
  | isFull g  = []
  | otherwise = mapMaybe (\i -> move g i p) [0..size^2]

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- validMoves g p]

depth :: Int
depth = 9

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minmax :: Tree Grid -> Tree (Grid, Player)
minmax (Node g [])
  | hasWon g O  = Node (g, O) []
  | hasWon g X  = Node (g, X) []
  | otherwise   = Node (g, B) []
minmax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where
                    ts' = map minmax ts
                    ps  = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minmax tree

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | hasWon g O = putStrLn "Player O wins!"
  | hasWon g X = putStrLn "Player X wins!"
  | isFull g     = putStrLn "Draw!"
  | p == X     = do putStr "Player X is thinking... "
                    (play $! bestmove g p) $ next p
  | p == O     = do i <- getNat $ prompt p
                    case move g i p of
                      Nothing -> do putStrLn "ERROR: Invalid move"
                                    play' g p
                      Just g' -> play g' $ next p

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O
