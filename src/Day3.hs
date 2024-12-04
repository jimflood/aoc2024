
module Day3
    ( day3
    ) where

import Lib (slurpLines)
import Data.Char
import Data.List
import Data.Maybe

parse :: (String -> Maybe Bool) -> [String] -> ([(Int, Int)], (Int, Int), Char, Bool)
parse f css = foldl parse' ([], (0, 0), 'm', True) css
    where
        parse' :: ([(Int, Int)], (Int, Int), Char, Bool) -> String -> ([(Int, Int)], (Int, Int), Char, Bool)
        parse' (acc, t, s, d) [] = (acc, t, s, d)
        parse' (acc, t, s, d) (c : cs)
            | s == 'm' && isJust (f (c : cs)) = parse' (acc, t, 'm', fromJust (f (c : cs))) cs
            | s == 'm' && c == 'm' = parse' (acc, t, 'u', d) cs
            | s == 'u' && c == 'u' = parse' (acc, t, 'l', d) cs
            | s == 'l' && c == 'l' = parse' (acc, t, '(', d) cs
            | s == '(' && c == '(' = parse' (acc, t, ',', d) cs
            | s == ',' && (isDigit c) = parse' (acc, (fst t * 10 + (read [c] :: Int), snd t), ',', d) cs
            | s == ',' && c == ',' = parse' (acc, t, ')', d) cs
            | s == ')' && (isDigit c) = parse' (acc, (fst t, snd t * 10 + (read [c] :: Int)), ')', d) cs
            | s == ')' && c == ')' && d = parse' (t : acc, (0, 0), 'm', d) cs
            | otherwise = parse' (acc, (0, 0), 'm', d) cs

solve :: [(Int, Int)] -> Int
solve ts = sum $ map ( \ (a, b) -> a * b) ts

doOf :: String -> Maybe Bool
doOf cs
    | isPrefixOf "do()" cs = Just True
    | isPrefixOf "don't()" cs = Just False
    | otherwise = Nothing

day3 :: IO ()
day3 = do
    xs <- slurpLines "day3.txt"
    let (ts1, _, _, _) = parse ( \ _ -> Nothing) xs
    let answer1 = solve ts1
    print $ "part 1: " ++ (show answer1)
    let (ts2, _, _, _) = parse doOf xs
    let answer2 = solve ts2
    print $ "part 2: " ++ (show answer2)
