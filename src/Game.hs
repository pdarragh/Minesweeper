{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Game
    ( makeBoard
    , perspShow
    , Perspective(..)
    )
where

import           Data.List
import           Data.List.Split
import           Control.Monad.Random
import           System.Random.Shuffle

{-@ measure llen @-}
llen :: [a] -> Int
llen []       = 0
llen (x : xs) = 1 + llen xs

-- {-@ type Coord = { p : (Int, Int) | fst p >= 0 && snd p >= 0 } @-}
type Coord = (Int, Int)

{-@ type MineCount = { v : Int | 0 <= v && v <= 8 } @-}
type MineCount = Int

{-@ type Size = { v : Int | 0 < v } @-}
type Size = Int

data CellFill
    = Mine
    | Proximity MineCount
    deriving (Eq)

instance Show CellFill where
    show Mine          = "*"
    show (Proximity c) = if c == 0 then " " else show c

-- getRandomCellValue :: MonadRandom m => m CellValue
-- getRandomCellValue = getRandomR (0, 9)

-- newCell :: MonadRandom m => m Cell
-- newCell = do
--     value <- getRandomCellValue
--     return Cell { getValue = value, getState = Hidden }

-- {-@ generateCoords :: w : Size -> h : Size -> { cs : [Coord] | llen cs == (w * h) } @-}
generateCoords :: Size -> Size -> [Coord]
generateCoords w h = [ (x, y) | x <- [0 .. w], y <- [0 .. h] ]

{-@ chooseMines :: MonadRandom m => Size -> [Coord] -> m [Coord] @-}
chooseMines :: MonadRandom m => Size -> [Coord] -> m [Coord]
chooseMines n coords = sort . take n <$> shuffleM coords

class PerspShow a where
    perspShow :: Perspective -> a -> String

{-
ALGORITHM FOR BUILDING BOARD

mines  :: [(Int, Int)] <- sorted list of mine locations
coords :: [(Int, Int)] <- sorted list of all coordinates

1. select mines from coordinates
2. make board with optional mines
3. make new board with mines and real proximities

-}

data CellState
    = Undisturbed   -- no interaction
    | Flagged       -- player flag
    | Revealed      -- revealed mine or number
    deriving (Eq)

data Cell = Cell { fill  :: CellFill
                 , state :: CellState }
                 deriving (Eq)

data Perspective = Player | Computer

instance PerspShow Cell where
    perspShow persp Cell {..} = case state of
        Undisturbed -> case persp of
            Player   -> "#"
            Computer -> show fill
        Flagged  -> "^"
        Revealed -> show fill

instance Show Cell where
    show = perspShow Player

type Row = [Cell]

instance PerspShow Row where
    perspShow persp = concatMap (perspShow persp)

type Board = [Row]

instance PerspShow Board where
    perspShow persp rows = unlines (map (perspShow persp) rows)

mkMineCell :: Cell
mkMineCell = Cell { fill = Mine, state = Undisturbed }

{-@ mkProxCell :: MineCount -> Cell @-}
mkProxCell :: MineCount -> Cell
mkProxCell c = Cell { fill = Proximity c, state = Undisturbed }

{-@ mkMaybeCell :: [Coord] -> Coord -> Maybe Cell @-}
mkMaybeCell :: [Coord] -> Coord -> Maybe Cell
mkMaybeCell []       coord = Nothing
mkMaybeCell (c : cs) coord = if
    | c == coord -> Just mkMineCell
    | otherwise  -> mkMaybeCell cs coord

{-@ groupN :: w : Size -> h : Size -> { xs : [a] | llen xs == w * h } -> [[a]] @-}
groupN :: Int -> Int -> [a] -> [[a]]
groupN w _ = chunksOf w

-- mkBoardFromList :: Size -> [a] -> [[a]]
-- mkBoardFromList w xs =

-- mkBoardFromList' :: Size -> [a] -> [[a]] -> [[a]]
-- mkBoardFromList' _ [] xss = xss
-- mkBoardFromList' w xs xss = (take w xs) : xss

{-@ mkMineBoard :: Size -> Size -> [Coord] -> [Maybe Cell] @-}
mkMineBoard :: Size -> Size -> [Coord] -> [Maybe Cell]
mkMineBoard w h mines = map (mkMaybeCell mines) (generateCoords w h)

{-@ makeBoard :: { w : Int | w > 0 } -> { h : Int | h > 0 } -> [[Cell]] @-}
makeBoard :: Int -> Int -> [[Cell]]
makeBoard w h = replicate w $ replicate h mkMineCell
