
module Day17
    ( day17
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Bits (xor)
import Data.Function (on)

adv :: Int
adv = 0

bxl :: Int
bxl = 1

bst :: Int
bst = 2

jnz :: Int
jnz = 3

bxc :: Int
bxc = 4

out :: Int
out = 5

bdv :: Int
bdv = 6

cdv :: Int
cdv = 7

data Computer = Computer { ra :: Int
                         , rb :: Int
                         , rc :: Int
                         , ip :: Int
                         , pgm :: Map.Map Int Int
                         , output :: [Int]
                         } deriving (Show)

parse :: [String] -> Computer
parse [a, b, c, _, m] = Computer (reg a) (reg b) (reg c) 0 (pgmOf m) []
    where
        reg x = read $ dropWhile (not . isDigit) x
        pgmOf x = Map.fromList $ zip [0..] $ map read $ splitOn "," $ last $ splitOn " " x
parse _ = error "bad input"

run :: Computer -> Computer
run cc
    | Map.notMember (ip cc) (pgm cc) = cc { output = reverse (output cc) }
    | otherwise = run (exe ((pgm cc) Map.! (ip cc)) ((pgm cc) Map.! ((ip cc) + 1)))
    where
        exe :: Int -> Int -> Computer
        exe opcode operand
            | opcode == adv = cc { ra = (ra cc) `div` (2 ^ combo), ip = (ip cc) + 2 }
            | opcode == bxl = cc { rb = (rb cc) `xor` literal, ip = (ip cc) + 2 }
            | opcode == bst = cc { rb = combo `mod` 8, ip = (ip cc) + 2 }
            | opcode == jnz && (ra cc) == 0 = cc { ip = (ip cc) + 2 }
            | opcode == jnz = cc { ip = literal }
            | opcode == bxc = cc { rb = (rb cc) `xor` (rc cc), ip = (ip cc) + 2 }
            | opcode == out = cc { output = (combo `mod` 8) : (output cc), ip = (ip cc) + 2 }
            | opcode == bdv = cc { rb = (ra cc) `div` (2 ^ combo), ip = (ip cc) + 2 }
            | opcode == cdv = cc { rc = (ra cc) `div` (2 ^ combo), ip = (ip cc) + 2 }
            | otherwise = error ("CRASH " ++ show cc)
           where
                combo
                    | operand >= 0 && operand <= 3 = operand
                    | operand == 4 = ra cc
                    | operand == 5 = rb cc
                    | operand == 6 = rc cc
                    | operand == 7 = error "RESERVED OPERAND"
                    | otherwise = error "BAD OPERAND"
                literal = operand

-- I cracked it by hand and then used what I learned to tailor this solution:
solve2 :: Computer -> Int
solve2 = solve2' 1 (8 ^ (15 :: Int)) 15
    where
        solve2' :: Int -> Int -> Int -> Computer -> Int
        solve2' m a b cc = solve2'' $ run cc { ra = a }
            where
                solve2'' ncc
                    | output ncc == unPgm cc = a
                    | ((==) `on` (drop b)) (output ncc) (unPgm cc) = solve2' (m + 1) a (b - 1) cc
                    | otherwise = solve2' (m + 1) (a + (8 ^ b)) b cc
        unPgm :: Computer -> [Int]
        unPgm cc = (map snd (Map.toAscList (pgm cc)))

day17 :: IO ()
day17 = do
    xs <- slurpLines "day17.txt"
    let cc = parse xs
    let answer1 = output (run cc)
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 cc
    print $ "part 2: " ++ (show answer2)
