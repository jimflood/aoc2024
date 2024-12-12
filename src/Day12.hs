
module Day12
    ( day12
    ) where

import Lib (slurpLines, Coordinate, Grid, parseGrid)
import qualified Data.Map as Map
import Data.Function (on)
import Data.List (partition)
import Data.Maybe
import Data.Tuple.Ops (appF, appPoly)

type Plot = (Coordinate, Char)

type Region = [Plot]

type FenceSection = (Coordinate, (Int, Int))

regionsOf :: Grid -> [Region]
regionsOf g = cluster samePlot (Map.toList g)
    where
        samePlot :: Plot -> Plot -> Bool
        samePlot a b = ((adjacent `on` fst) a b) && (((==) `on` snd) a b)

adjacent :: Coordinate -> Coordinate -> Bool
adjacent (x1, y1) (x2, y2) = ((x1 == x2) && (abs (y1 - y2) == 1)) || ((abs (x1 - x2) == 1) && (y1 == y2))

area :: Region -> Int
area = length

perimeter :: (FenceSection -> FenceSection -> Bool) -> Grid -> Region -> Int
perimeter f g = length . (cluster f) . (concatMap fences)
    where
        fences ((x, y), a) = mapMaybe side [(1, 0), (-1, 0), (0, 1), (0, -1)]
            where
                side (dx, dy)
                    | Map.lookup (x + dx, y + dy) g /= Just a = Just ((x, y), (dx, dy))
                    | otherwise = Nothing

notSameFence :: FenceSection -> FenceSection -> Bool
notSameFence _ _ = False

sameFence :: FenceSection -> FenceSection -> Bool
sameFence a b = ((adjacent `on` fst) a b) && (((==) `on` snd) a b)

cluster :: (a -> a -> Bool) -> [a] -> [[a]]
cluster f = foldl ( \ a b -> appF (:) $ appPoly ((b:) . concat) $ partition (any (f b)) a) []

day12 :: IO ()
day12 = do
    xs <- slurpLines "day12.txt"
    let (_, g) = parseGrid xs
    let rs = regionsOf g
    let answer1 = sum $ zipWith (*) (map (\ r -> perimeter notSameFence g r) rs) (map area rs)
    print $ "part 1: " ++ (show answer1)
    let answer2 = sum $ zipWith (*) (map (\ r -> perimeter sameFence g r) rs) (map area rs)
    print $ "part 2: " ++ (show answer2)
