
module Day13
    ( day13
    ) where

import Lib (chunk, slurpLines)
import Data.List.Split (splitOneOf)
import Data.Maybe

type Machine = ((Int, Int), (Int, Int), (Int, Int))

parse :: [String] -> [Machine]
parse ys = foldl parse' [] (chunk ys)
    where
        parse' acc (a : b : c : _) = (f c, f b, f a) : acc
        parse' _ _ = error "nope"
        f xs = f' $ drop 1 $ filter ((/=0) . length) (splitOneOf (" :=,ABXY+") xs)
        f' [x, y] = (read x, read y)
        f' _ = error "ruh-roh"

tokens :: Machine -> Maybe Int
tokens ((ax, ay), (bx, by), p@(px, py))
    | t == p = Just ((3 * a) + b)
    | otherwise = Nothing
    where
        b = ((ax * py) - (ay * px)) `div` ((ax * by) - (bx * ay))
        a = (px - (b * bx)) `div` ax
        t = ((a * ax) + (b * bx), (a * ay) + (b * by))

solve :: (Machine -> Machine) -> [Machine] -> Int
solve f ms = sum $ mapMaybe (tokens . f) ms

tr :: Machine -> Machine
tr (a, b, (px, py)) = (a, b, (px + 10000000000000, py + 10000000000000))

day13 :: IO ()
day13 = do
    xs <- slurpLines "day13.txt"
    let ms = parse xs
    let answer1 = solve id ms
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve tr ms
    print $ "part 2: " ++ (show answer2)
