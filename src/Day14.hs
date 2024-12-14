
module Day14
    ( day14
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOneOf)
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

type Bounds = (Int, Int)

type Coordinate = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Coordinate, Velocity)

type RobotSet = Set.Set Robot

parse :: [String] -> RobotSet
parse = parse' []
    where
        parse' acc [] = Set.fromList acc
        parse' acc (cs : css) = parse' (robot : acc) css
            where
                robot = robot' $ map read $ filter ((/=0) . length) (splitOneOf "p=, v" cs)
                robot' [x, y, vx, vy] = ((x, y), (vx, vy))
                robot' _ = error "ruh-roh"

step :: Bounds -> Robot -> Robot
step (bx, by) ((x, y), v@(vx, vy)) = (((x + vx) `mod` bx, (y + vy) `mod` by), v)

run :: Bounds -> Int -> RobotSet -> (RobotSet, Maybe Int)
run b m = run' m
    where
        run' 0 rs = (rs, Nothing)
        run' n rs
            | pic rs = trace ((draw b rs) ++ "\n") (rs, Just (m - n))
            | otherwise = run' (n - 1) $ Set.map (step b) rs

draw :: Bounds -> RobotSet -> String
draw (mx, my) rs = unlines [draw' y | y <- [0..my - 1]]
    where
        draw' :: Int -> String
        draw' y = [draw'' (x, y) | x <- [0..mx - 1]]
            where
                draw'' :: (Int, Int) -> Char
                draw'' a = draw''' $ Set.size $ Set.filter ( \ (b, _) -> b == a) rs
                draw''' 0 = '.'
                draw''' n = last $ show n

quadrants :: Bounds -> RobotSet -> [RobotSet]
quadrants (bx, by) rs = map ( \ q -> Set.filter q rs) [tl, bl, tr, br]
    where
        mx = bx `div` 2
        my = by `div` 2
        tl ((x, y), _) = x < mx && y < my
        bl ((x, y), _) = x < mx && y > my
        tr ((x, y), _) = x > mx && y < my
        br ((x, y), _) = x > mx && y > my

pic :: RobotSet -> Bool
pic a = pic' $ Set.map fst a
    where
        pic' rs = any tree (Set.toList rs)
            where
                -- Look for this pattern at position @:
                -- ...@...
                -- ..XXX..
                -- .XXXXX.
                -- XXXXXXX
                -- Low tech but fast.
                tree (x, y)
                    | Set.member (x - 3, y) rs = False
                    | Set.member (x - 2, y) rs = False
                    | Set.member (x - 1, y) rs = False
                    -- @
                    | Set.member (x + 1, y) rs = False
                    | Set.member (x + 2, y) rs = False
                    | Set.member (x + 3, y) rs = False
                    --
                    | Set.member (x - 3, y + 1) rs = False
                    | Set.member (x - 2, y + 1) rs = False
                    | Set.notMember (x - 1, y + 1) rs = False
                    | Set.notMember (x, y + 1) rs = False
                    | Set.notMember (x + 1, y + 1) rs = False
                    | Set.member (x + 2, y + 1) rs = False
                    | Set.member (x + 3, y + 1) rs = False
                    --
                    | Set.member (x - 3, y + 2) rs = False
                    | Set.notMember (x - 2, y + 2) rs = False
                    | Set.notMember (x - 1, y + 2) rs = False
                    | Set.notMember (x, y + 2) rs = False
                    | Set.notMember (x + 1, y + 2) rs = False
                    | Set.notMember (x + 2, y + 2) rs = False
                    | Set.member (x + 3, y + 2) rs = False
                    --
                    | Set.notMember (x - 3, y + 3) rs = False
                    | Set.notMember (x - 2, y + 3) rs = False
                    | Set.notMember (x - 1, y + 3) rs = False
                    | Set.notMember (x, y + 3) rs = False
                    | Set.notMember (x + 1, y + 3) rs = False
                    | Set.notMember (x + 2, y + 3) rs = False
                    | Set.notMember (x + 3, y + 3) rs = False
                    --
                    | otherwise = True

day14 :: IO ()
day14 = do
    xs <- slurpLines "day14.txt"
    let b = (101, 103)
    let rs = parse xs
    let answer1 = product $ map Set.size $ quadrants b $ fst (run b 100 rs)
    print $ "part 1: " ++ (show answer1)
    let answer2 = fromJust $ snd (run b 10000 rs) -- 10000 is an empirical guess
    print $ "part 2: " ++ (show answer2)
