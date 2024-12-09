
module Day9
    ( day9
    ) where

import Lib (slurpLines)
import Data.Maybe

parse :: [String] -> [(Int, Maybe Int)]
parse [x] = reverse $ parse' [] 0 x
    where
        parse' acc i (a : b : cs) = parse' ((read [b], Nothing) : (read [a], Just i) : acc) (i + 1) cs
        parse' acc i (a : []) = (read [a], Just i) : acc
        parse' _ _ _ = error "bad input chars"
parse _ = error "bad input lines"

compact :: [(Int, Maybe Int)] -> [(Int, Maybe Int)]
compact = compact' []
    where
        compact' acc [] = reverse acc
        compact' acc xs
            | (isNothing . snd . last) xs = compact' acc (init xs)
            | (isJust . snd . head) xs = compact' (head xs : acc) (tail xs)
            | otherwise = compact'' (head xs) ((init . tail) xs) (last xs)
            where
                compact'' (n, Nothing) ys (m, Just z)
                    | n == m = compact' ((m, Just z) : acc) ys
                    | n > m = compact' ((m, Just z) : acc) ((n - m, Nothing) : ys)
                    | otherwise = compact' ((n, Just z) : acc) (ys ++ [(m - n, Just z)])
                compact'' _ _ _ = error "cannot occur"

compact2 :: [(Int, Maybe Int)] -> [(Int, Maybe Int)]
compact2 x = compact2' x (reverse x)
    where
        compact2' xs [] = xs
        compact2' xs (y : ys)
            | isJust (snd y) = compact2' (move [] xs) ys
            | otherwise = compact2' xs ys
            where
                move acc (a : bs)
                    | snd a == snd y = xs
                    | isJust (snd a) = move (a : acc) bs
                    | isNothing (snd a) && fst a < fst y = move (a : acc) bs
                    | isNothing (snd a) && fst a == fst y = (reverse acc) ++ [y] ++ (map edit bs)
                    | isNothing (snd a) && fst a > fst y = (reverse acc) ++ [y, (fst a - fst y, Nothing)] ++ (map edit bs)
                move _ _ = error "cannot occur"
                edit a
                    | a == y = (fst y, Nothing)
                    | otherwise = a

checksum :: [(Int, Maybe Int)] -> Int
checksum = checksum' 0 0
    where
        checksum' _ acc [] = acc
        checksum' pos acc ((n, Nothing) : xs) = checksum' (pos + n) acc xs
        checksum' pos acc ((n, Just x) : xs) = checksum' (pos + n) (acc + delta) xs
            where
                delta = sum $ map (x*) [pos..(pos + n - 1)]

day9 :: IO ()
day9 = do
    xs <- slurpLines "day9.txt"
    let dm = parse xs
    let answer1 = checksum $ compact dm
    print $ "part 1: " ++ (show answer1)
    let answer2 = checksum $ compact2 dm
    print $ "part 2: " ++ (show answer2)
