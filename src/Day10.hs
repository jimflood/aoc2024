
module Day10
    ( day10
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (groupBy, nub, partition, sort)
import qualified Data.Map as Map

type Trail = [Coordinate]

trailheads :: Grid -> [Trail]
trailheads g = map (:[]) $ Map.keys $ Map.filter (=='0') g

uphill :: Char -> Char
uphill c = chr ((ord c) + 1)

hike :: Grid -> [Trail] -> [Trail]
hike g = hike' []
    where
        hike' acc [] = acc
        hike' acc (t@(p : _) : ts) = thresh $ partition endpoint paths
            where
                thresh (hs, cs) = hike' (hs ++ acc) (cs ++ ts)
                endpoint a = (g Map.! (head a)) == '9'
                paths = filter passable $ map (:t) (fan p)
                passable a = Map.lookup (head a) g == Just (uphill (g Map.! p))
                fan (x, y) = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
        hike' _ _ = error "cannot occur"

solve :: (Trail -> [Coordinate]) -> [Trail] -> Int
solve f ts = sum $ map length $ groupBy ((==) `on` last) $ nub $ sort $ map f ts

forScore :: Trail -> [Coordinate]
forScore t = [head t, last t]

forRating :: Trail -> [Coordinate]
forRating = id

day10 :: IO ()
day10 = do
    xs <- slurpLines "day10.txt"
    let (_, g) = parseGrid xs
    let ts = hike g $ trailheads g
    let answer1 = solve forScore ts
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve forRating ts
    print $ "part 2: " ++ (show answer2)

