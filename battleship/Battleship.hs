{-# LANGUAGE ScopedTypeVariables #-}

module Battleship where

import Control.Applicative              ((<|>))
import Control.Monad                    (replicateM)
import Control.Monad.Trans.State.Strict
import Data.Bool                        (bool)
import System.Random                    (randomRIO)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
    deriving (Bounded, Eq, Ord, Enum, Show)

data Col = Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj
    deriving (Bounded, Eq, Ord, Enum, Show)

data Coord = Coord Row Col deriving (Eq, Show, Ord)

data Ship = Patrol | Cruiser | Submarine | BattleShip | Carrier
    deriving (Eq, Ord, Show, Enum)

data Move = Miss | Hit deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

type Coords = S.Set Coord

type Board = M.Map Ship Coords

data Status = Won Player | InPlay deriving (Show)

data Game = Game
    { player1Board :: Board
    , player1Track :: Coords
    , player2Board :: Board
    , player2Track :: Coords
    } deriving (Eq, Show)

safeToEnum :: forall a. (Enum a, Bounded a) => Int -> Maybe a
safeToEnum from = bool (Just $ toEnum from) Nothing
    $ from < fromEnum (minBound :: a) || from > fromEnum (maxBound :: a)

safeFromTo :: (Enum a, Bounded a) => Int -> Int -> Maybe [a]
safeFromTo from to = traverse safeToEnum [from..to]

makeHorizontal :: (Int, Int) -> Ship -> Maybe Coords
makeHorizontal (x, y) ship = let
    len  = fromEnum ship + 1
    rows = replicateM len (safeToEnum y)
    cols = safeFromTo x (x+len)
    in S.fromList . map (uncurry Coord) <$> (zip <$> rows <*> cols)

makeVertical :: (Int, Int) -> Ship -> Maybe Coords
makeVertical (x, y) ship = let
    len  = fromEnum ship + 1
    rows = safeFromTo y (y+len)
    cols = replicateM len (safeToEnum x)
    in S.fromList . map (uncurry Coord) <$> (zip <$> rows <*> cols)

place :: Ship -> Coords -> Board -> Board
place = M.insert

allCoords :: Board -> Coords
allCoords = S.unions . M.elems

pair :: IO (Int, Int)
pair = (,) <$> r <*> r where r = randomRIO (0,9)

genBoard' :: [Ship] -> Board -> IO Board
genBoard' []     board = return board
genBoard' (s:ss) board = pair >>= \p ->
    case makeHorizontal p s <|> makeVertical p s of
        Just cs | S.null $ S.intersection cs (allCoords board) ->
             genBoard'    ss $ place s cs board
        _ -> genBoard' (s:ss)             board

genBoard :: IO Board
genBoard = genBoard' [Carrier, BattleShip, Submarine, Patrol, Cruiser] M.empty

makeAttackingMove :: Player -> Coord -> State Game Move
makeAttackingMove Player1 coord = get >>= \game -> let
    move = bool Miss Hit (coord `S.member` allCoords (player2Board game))
    p1t  = S.insert coord $ player1Track game
    in put game {player1Track = p1t} >> return move
makeAttackingMove Player2 coord = get >>= \game -> let
    move = bool Miss Hit (coord `S.member` allCoords (player1Board game))
    p2t  = S.insert coord $ player2Track game
    in put game {player2Track = p2t} >> return move

status :: State Game Status
status = get >>= \game -> let
    p1b = allCoords $ player1Board game
    p2t = S.intersection (player2Track game) p1b
    p2b = allCoords $ player2Board game
    p1t = S.intersection (player2Track game) p2b
    in return $ case () of
        _ | p1t == p2b -> Won Player1
          | p2t == p1b -> Won Player2
          | otherwise  -> InPlay

emptyGame :: Game
emptyGame = Game M.empty S.empty M.empty S.empty
