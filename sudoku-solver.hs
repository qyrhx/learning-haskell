-- λ

import Data.List (transpose)

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

emptyGrid :: Grid
emptyGrid = replicate 9 $ replicate 9 '.'
-- emptyGrid = replicate 9 [1..9]

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose
--cols m = map (\i -> map (!! i) m) [0..8]

boxes :: Matrix a -> [Row a]
boxes m = concatMap boxesOfTriplet $ triplets m where
  -- Note: could easily use `chunksOf` from `split` package
  -- but I don't want to add external deps lol
  triplets :: [a] -> [[a]]
  triplets [] = []
  triplets l = triplet : triplets rest where (triplet, rest) = splitAt 3 l
  -- Takes three rows, returns three boxes
  boxesOfTriplet :: [Row a] -> [Row a]
  boxesOfTriplet threeRs = [concatMap (take 3 . drop (i*3)) threeRs | i <- [0..2]]

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g)
  where nodups [] = True
        nodups (x:xs) = notElem x xs && nodups xs

type Choices = [Value]
solve :: Grid -> [Grid]
solve = filter valid . genPossibles . choices where
  choices :: Grid -> Matrix Choices
  choices = map . map $ choice where
    choice p = if p == '.' then ['1'..'9'] else [p]

  genPossibles :: Matrix [a] -> [Matrix a]
  genPossibles m = cartProd (map cartProd m)

  -- Cartesian Product
  cartProd :: [[a]] -> [[a]]
  cartProd [] = [[]]
  cartProd (xs:xss) = [y:ys | y <- xs, ys <- cartProd xss]
