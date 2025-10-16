-- λ
-- A simple game where there is a board, with a number of rows
-- each row has a number of stars.
-- At each turn, a player takes a certain number of stars
-- The one to remove the last remaining rows wins

import System.IO (hFlush, stdout)

type NimBoard = [Int]

newNimBoard :: Int -> NimBoard
newNimBoard l = reverse [1 .. l]

isValidMove :: NimBoard -> Int -> Int -> Bool
isValidMove g l n =
  l > 0 && l <= length g
  && n > 0 && n <= g !! (l - 1)

playTurn :: NimBoard -> Int -> Int -> Maybe NimBoard
playTurn g l n
  | isValidMove g l n = let i = l-1 in
      Just $ take i g ++ [(g !! i) - n] ++ drop l g
  | otherwise = Nothing

checkEndGame :: NimBoard -> Bool
checkEndGame g = 0 == sum g

printNimBoard :: NimBoard -> IO ()
printNimBoard g =
  let aux n g' =
        case g' of
          [] -> return ()
          x:xs -> do
            putStrLn $ show n ++ ": " ++ concat (replicate x "* ")
            aux (n+1) xs
  in aux 1 g

play :: NimBoard -> Int -> IO ()
play g p =
  let switchPlayer p' = if p' == 1 then 2 else 1
  in do
    printNimBoard g
    putStr $ "Player " ++ show p ++ ", pick a line\n> "
    hFlush stdout
    l <- readLn :: IO Int
    putStr "Pick the number of stars to remove\n> "
    hFlush stdout
    n <- readLn :: IO Int
    case playTurn g l n of
      Just g' -> if checkEndGame g' then
        do putStrLn $ "-- Player " ++ show p ++ " has won!"
                 else play g' $ switchPlayer p
      Nothing -> do
        putStrLn "-- Error: wrong arguments\n"
        play g p

main :: IO ()
main = play (newNimBoard 5) 1
