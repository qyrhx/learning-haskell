import Control.Concurrent (threadDelay)
-- λ
-- Game of Life

-- (1, 1) = top left
type Pos = (Int, Int)
type Board = [Pos]

width :: Int
width = 10

height :: Int
height = 10

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

writeat :: Pos -> String -> IO ()
writeat p s = do goto p
                 putStr s

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x, y) = (1 + (x-1) `mod` width
              , 1 + (y-1) `mod` height
              )

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [(x+1, y), (x-1, y)
                            , (x, y-1), (x+1, y-1), (x-1, y-1)
                            , (x, y+1), (x+1, y+1), (x-1, y+1)
                            ]

aliveNeighbors :: Board -> Pos -> Int
aliveNeighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, aliveNeighbors b p `elem` [2, 3]]

-- births :: Board -> [Pos]
-- births b = [(x, y) | x <- [1..width]
--                    , y <- [1..height]
--                    , isEmpty b (x, y)
--                    , aliveNeighbors b (x, y) == 3
--                    ]

births :: Board -> [Pos]
births b = [p | p <- rmdups $ concatMap neighbors b
              , isEmpty b p
              , aliveNeighbors b p == 3
              ]
  where
    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = births b ++ survivors b

life :: Board -> IO ()
life b = do cls
            showcells b
            threadDelay 500000
            life $ nextgen b

main :: IO ()
main = life glider
