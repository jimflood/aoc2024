module Lib
    ( slurpLines,
      Coordinate,
      BoundedGrid,
      Grid,
      parseGrid,
      drawGrid,
      chunk,
      opchunk -- order preserved
    ) where

import qualified Data.Map as Map

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Char

type BoundedGrid = ((Int, Int), Grid)

parseGrid :: [String] -> BoundedGrid
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

drawGrid :: BoundedGrid -> String
drawGrid ((mx, my), m) = unlines [draw' y | y <- [0..my - 1]]
    where
        draw' :: Int -> String
        draw' y = [m Map.! (x, y) | x <- [0..mx - 1]]

chunk :: [String] -> [[String]]
chunk = chunk' [[]]
    where
        chunk' (a : acc) (x : xs)
            | x == "" = chunk' ([] : (a : acc)) xs
            | otherwise = chunk' ((x : a) : acc) xs
        chunk' acc [] = acc

opchunk :: [String] -> [[String]]
opchunk = opchunk' [] []
    where
        opchunk' acc t (x : xs)
            | x == "" = opchunk' (reverse t : acc) [] xs
            | otherwise = opchunk' acc (x : t) xs
        opchunk' acc t [] = reverse (reverse t : acc)
        opchunk _ _ _ = error "bad input"
