
module Day25
    ( day25
    ) where

import Lib (opchunk, parseGrid, slurpLines)
import qualified Data.Map as Map

parse :: [String] -> ([[Int]], [[Int]])
parse xs = parse' [] [] $ opchunk xs
    where
        parse' ls ks [] = (ls, ks)
        parse' ls ks (ys : yss) = parse'' $ snd (parseGrid ys)
            where
                parse'' g
                    | all ((==) '#') [g Map.! (x, 0) | x <- [0..4]] = parse' ((counts 1 6) : ls) ks yss
                    | all ((==) '#') [g Map.! (x, 6) | x <- [0..4]] = parse' ls ((counts 0 5) : ks) yss
                    where
                        counts n m = reverse $ foldl ( \ a b -> (length $ filter (((==) '#')) [g Map.! (b, i) | i <- [n..m]]) : a) [] [0..4]
                parse'' _ = error "ruh-roh"

solve :: [[Int]] -> [[Int]] -> Int
solve ls ks = length $ filter ( \ xs -> all ( \ h -> h <= 5) xs) [zipWith (+) a b| a <- ls, b <- ks]

day25 :: IO ()
day25 = do
    xs <- slurpLines "day25.txt"
    let (ls, ks) = parse xs
    let answer1 = solve ls ks
    print $ "part 1: " ++ (show answer1)
