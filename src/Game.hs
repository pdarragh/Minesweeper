module Game where

import Control.Monad.Random

data BoardCell = Empty
               | Flag Int
               | Mine
               deriving (Show, Eq)

data ChoiceCell a = Chosen a
                  | Unchosen a
                  deriving (Show, Eq)

type Cell = ChoiceCell BoardCell

type BoardRow = [Cell]
type Board    = [BoardRow]

data Game = Game { board    :: Board
                 , finished :: Bool
                 , won      :: Bool
                 }
                 deriving (Show, Eq)

--createGame :: Int -> Int -> Int -> Game
--createGame x y mines = Game (createBoard x y mines) False False

--createBoard :: Int -> Int -> Int -> Board
--createBoard x y mines = setFlags (iterate createRow y)

--createRow :: Int -> 

--minePositions :: Int -> Int -> Int -> [(Int, Int)]
--minePositions x y mines = minePositions' x y [] mines

--getPos :: Int -> IO Int
getPos :: MonadRandom m => Int -> m Int
getPos n = getRandomR (0, n - 1)

minePositions :: MonadRandom m => Int -> Int -> Int -> m [(Int, Int)]
minePositions x y m
    | m > (x * y) = error "too many mines for board size"
    | otherwise = minePositions' x y m

minePositions' :: MonadRandom m => Int -> Int -> Int -> m [(Int, Int)]
minePositions' _ _ 0 = return []
minePositions' x y m = do
    ps <- minePositions' x y (m - 1)
    x_pos <- getPos x
    y_pos <- getPos y
    let pos = (x_pos, y_pos)
    if elem pos ps
       then minePositions' x y m
       else return (pos : ps)

--unique :: (Eq a) => [a] -> [a] -> [a]
--unique seen (x : xs)
--    | x `elem` seen = unique seen xs
--    | otherwise = x : unique (x : seen) xs

--minePositions' x y m = do
--    ps <- getRandomRs (x, y)
--    return (take m (unique [] ps))
