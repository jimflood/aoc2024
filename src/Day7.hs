
module Day7
    ( day7
    ) where

import Lib (slurpLines)
import qualified Data.List.Split as Split

parse :: [String] -> [(Int, [Int])]
parse xs = map (parse' . ( \ x -> Split.splitOneOf ": " x)) xs
    where
        parse' (a : _ : bs) = (read a, map read bs)
        parse' _ = error "bad input"

valid :: [(Int -> Int -> Int)] -> (Int, [Int]) -> Bool
valid fs (r, v : vs) = valid' v vs
    where
        valid' x [] = x == r
        valid' x (y : ys) = any ( \ f -> (valid' (f x y) ys)) fs

solve :: [(Int -> Int -> Int)]  -> [(Int, [Int])] -> Int
solve fs eqs = sum $ map fst $ filter ( \ e -> valid fs e) eqs

cat :: Int -> Int -> Int
cat a b = (read (show a ++ show b))

day7 :: IO ()
day7 = do
    xs <- slurpLines "day7.txt"
    let eqs = parse xs
    let answer1 = solve [(*), (+)] eqs
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve [(*), (+), cat] eqs
    print $ "part 2: " ++ (show answer2)
