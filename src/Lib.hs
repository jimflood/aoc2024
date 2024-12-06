module Lib
    ( slurpLines,
      Coordinate,
      Grid,
      parseGrid,
      drawGrid
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
