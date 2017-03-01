{-# LANGUAGE ScopedTypeVariables #-}

module Battleship where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict
import Data.Bool (bool)
import System.Random
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
    deriving (Bounded, Eq, Ord, Enum, Show)

data Col = Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj
    deriving (Bounded, Eq, Ord, Enum, Show)

data Coord = Coord Row Col
    deriving (Eq, Show, Ord)

data Ship = Patrol | Cruiser | Submarine | BattleShip | Carrier
    deriving (Eq, Ord, Show, Enum)

data Move = Miss | Hit deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

type Board = M.Map Ship (S.Set Coord)
type Track = S.Set Coord

data Status = Won Player | InPlay deriving (Show)

data Game = Game
    { player1Board :: Board
    , player1Track :: Track
    , player2Board :: Board
    , player2Track :: Track
    }
    deriving (Eq, Show)

safeToEnum :: forall a . (Enum a, Bounded a) => Int -> Maybe a
safeToEnum from =
    if from < fromEnum (minBound :: a) || from > fromEnum (maxBound :: a)
        then Nothing
        else Just $ toEnum from

safeFromTo :: forall a . (Enum a, Bounded a) => Int -> Int -> Maybe [a]
safeFromTo from to = mapM safeToEnum [from..to]

makeHorizontal :: (Int, Int) -> Ship -> Maybe (S.Set Coord)
makeHorizontal (x, y) ship = let
    len    = fromEnum ship + 1
    rows   = replicateM len (safeToEnum y :: Maybe Row)
    cols   = safeFromTo x (x+len) :: Maybe [Col]
    coords = zip <$> rows <*> cols
    in S.fromList . map (uncurry Coord) <$> coords

makeVertical :: (Int, Int) -> Ship -> Maybe (S.Set Coord)
makeVertical (x, y) ship = let
    len    = fromEnum ship + 1
    rows   = safeFromTo y (y+len) :: Maybe [Row]
    cols   = replicateM len (safeToEnum x :: Maybe Col)
    coords = zip <$> rows <*> cols
    in S.fromList . map (uncurry Coord) <$> coords

place :: Ship -> S.Set Coord -> Board -> Board
place = M.insert

pair :: IO (Int, Int)
pair = (,) <$> r <*> r
    where r = randomRIO (0,9)

genBoard' :: [Ship] -> Board -> IO Board
genBoard' []     board = return board
genBoard' (s:ss) board = do
    p   <- pair
    let cs' = makeHorizontal p s <|> makeVertical p s
    let pr  = S.unions $ M.elems board
    case cs' of
        Just cs | S.intersection cs pr == S.empty -> let
            board' = M.insert s cs board
            in genBoard' ss board'
        _ -> genBoard' (s:ss) board

genBoard :: IO Board
genBoard = genBoard' [Carrier, BattleShip, Submarine, Patrol, Cruiser] M.empty

makeAttackingMove :: Player -> Coord -> State Game Move
makeAttackingMove Player1 coord = do
    game <- get
    let p2b  = S.unions $ M.elems $ player2Board game
    let p1t  = player1Track game
    let p1t' = S.insert coord p1t
    let move = bool Miss Hit (coord `S.member` p2b)
    put game {player1Track = p1t'}
    return move
makeAttackingMove Player2 coord = do
    game <- get
    let p1b  = S.unions $ M.elems $ player1Board game
    let p2t  = player2Track game
    let p2t' = S.insert coord p2t
    let move = bool Miss Hit (coord `S.member` p1b)
    put game {player2Track = p2t'}
    return move

status :: State Game Status
status = do
    game <- get
    let p1b  = S.unions $ M.elems $ player1Board game
    let p2t  = S.intersection (player2Track game) p1b
    let p2b  = S.unions $ M.elems $ player2Board game
    let p1t  = S.intersection (player2Track game) p2b
    case () of
        _
            | p1t == p2b -> return $ Won Player1
            | p2t == p1b -> return $ Won Player2
            | otherwise  -> return InPlay

emptyGame :: Game
emptyGame = Game M.empty S.empty M.empty S.empty
