
module Day20
    ( day20
    ) where

import Lib (gridFind, parseGrid, slurpLines, Coordinate, Grid)
import qualified Data.Map as Map
import Data.Maybe

type Picoseconds = Int

type TimeMap = Map.Map Coordinate Picoseconds

fourDirections :: Int -> Coordinate -> [Coordinate]
fourDirections d (x, y) = [(x, y - d), (x, y + d), (x - d, y), (x + d, y)]

circularArea :: Int -> Coordinate -> [Coordinate]
circularArea d (x, y) = [(x + dx, y + dy) | dx <- [(-d)..d], dy <- [(-d)..d], abs dx + abs dy <= d]

timeMap :: Grid -> TimeMap
timeMap g = timeMap' Map.empty 0 (Just start)
    where
        timeMap' tm _ Nothing = tm
        timeMap' tm n (Just p) = timeMap' (Map.insert p n tm) (n + 1) np
            where
                np = listToMaybe $ filter ( \ a -> (Map.notMember a tm) && (Map.lookup a g /= (Just '#'))) (fourDirections 1 p)
        start = gridFind 'S' g

cheats :: TimeMap -> (Coordinate -> [Coordinate]) -> [Int]
cheats tm f = foldl cheats' [] (Map.toList tm)
    where
        cheats' cs (p@(x, y), t) = cs ++ (mapMaybe maybeTime (f p))
            where
                maybeTime np@(nx, ny) = fmap delta (Map.lookup np tm)
                    where
                        delta nt = nt - (t + abs (x - nx) + abs (y - ny))

solve :: TimeMap -> (Coordinate -> [Coordinate]) -> Int
solve tm f = length $ filter ( \ a -> a >= 100) $ cheats tm f

day20 :: IO ()
day20 = do
    xs <- slurpLines "day20.txt"
    let (_, g) = parseGrid xs
    let tm = timeMap g
    let answer1 = solve tm (fourDirections 2)
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve tm (circularArea 20)
    print $ "part 2: " ++ (show answer2)
