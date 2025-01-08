
 module Day22
    ( day22
    ) where

import Lib (slurpLines)
import qualified Data.Map as Map
import Data.List (groupBy, sortBy)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Function (on)

parse :: [String] -> [Int]
parse = map read

next :: Int -> Int
next n = next' $ (xor (n * 64) n) `mod` 16777216
    where
        next' nn = next'' $ (xor (nn `div` 32) nn) `mod` 16777216
            where
                next'' nnn = (xor (nnn * 2048) nnn) `mod` 16777216

gen :: Int -> Int -> ([(Int, Int)], Int)
gen c n = gen' $ foldl ( \ (acc, nn) _ -> (nn : acc, next nn)) ([], n) [1..c]
    where
        gen' (a, b) = (diff (reverse (map ( \ i -> i `mod` 10) a)), b)
        diff ys = map ( \ t -> ((fst t) - (snd t), fst t)) (zip (tail ys) ys)

trc :: ([(Int, Int)], Int) -> Map.Map (Int, Int, Int, Int) Int
trc r = trc' Map.empty (fst r)
    where
        trc' m ((d1, _) : ts@((d2, _) : (d3, _) : (d4, p) : _))
            | Map.notMember (d1, d2, d3, d4) m = trc' (Map.insert (d1, d2, d3, d4) p m) ts
            | otherwise = trc' m ts
        trc' m _ = m

solve1 :: [([(Int, Int)], Int)] -> Int
solve1 rs = sum (map snd rs)

solve2 :: [([(Int, Int)], Int)] -> Int
solve2 rs = maximum $ map (sum . (map snd)) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ concatMap (Map.toList . trc) rs

day22 :: IO ()
day22 = do
    xs <- slurpLines "day22.txt"
    let ns = parse xs
    let rs = map (gen 2000) ns
    let answer1 = solve1 rs
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 rs
    print $ "part 2: " ++ (show answer2)
