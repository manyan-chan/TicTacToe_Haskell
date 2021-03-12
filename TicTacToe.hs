module TicTacToe (tictactoe) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Text.Printf

-- | Problem 1.
data Move = X | O

data Cell = Occupied Move | Null

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

instance Eq Cell where
  Occupied X == Occupied X = True
  Occupied O == Occupied O = True
  Null == Null             = True
  _ == _                   = False

instance Show Move where
  show X = "X"
  show O = "O"

instance Show Cell where
  show (Occupied X) = "X"
  show (Occupied O) = "O"
  show Null = " "

renderRow :: [Cell] -> String
renderRow row = "| " ++ intercalate " | " (fmap show row) ++ " |"

line :: String
line = ".---.---.---."

renderBoard :: [Cell] -> IO ()
renderBoard board = do
  putStrLn line
  putStrLn $ renderRow firstRow
  putStrLn line
  putStrLn $ renderRow secondRow
  putStrLn line
  putStrLn $ renderRow thirdRow
  putStrLn line
  where firstRow = take 3 board
        secondRow = drop 3 (take 6 board)
        thirdRow = drop 6 board

getBoardIndex :: String -> Maybe Int
getBoardIndex "1 1" = Just 0
getBoardIndex "1 2" = Just 1
getBoardIndex "1 3" = Just 2
getBoardIndex "2 1" = Just 3
getBoardIndex "2 2" = Just 4
getBoardIndex "2 3" = Just 5
getBoardIndex "3 1" = Just 6
getBoardIndex "3 2" = Just 7
getBoardIndex "3 3" = Just 8
getBoardIndex _    = Nothing

data CellTransform = Success [Cell] | Fail String [Cell]

verifyIsFree ::  [Cell] -> Int -> Maybe Int
verifyIsFree board ix = if board !! ix == Null then Just ix else Nothing

assignCell :: String -> Move -> [Cell] -> CellTransform
assignCell location move board =
  case getBoardIndex location >>= verifyIsFree board of
    Nothing -> Fail "INVALID POSITION" board
    Just i -> Success ((take i board) ++ [Occupied move] ++ (drop (i+1) board))

playRound :: Move  -> [Cell] -> IO ()
playRound move board = do
  renderBoard board
  putStrLn $ (show move) ++ " MOVE"
  cellManip move board

cellManip :: Move -> [Cell] -> IO ()
cellManip move board = do
  cell <- getLine
  case assignCell cell move board of
    Fail err board -> do
      putStrLn err
      cellManip move board
    Success newBoard -> do
      if isThereAWinner move newBoard then do
        renderBoard newBoard
        putStrLn $ ((show move) ++ " WINS")
        return ()
      else if isDraw newBoard then do
        renderBoard newBoard
        putStrLn "DRAW"
        return ()
      else playRound (nextMove move) newBoard


isThereAWinner :: Move -> [Cell] -> Bool
isThereAWinner move board =
  or [
    -- check top row
    board !! 0 == (Occupied move) && board !! 1 == (Occupied move) && board !! 2 == (Occupied move),
    -- check middle row
    board !! 3 == (Occupied move) && board !! 4 == (Occupied move) && board !! 5 == (Occupied move),
    -- check bottom row
    board !! 6 == (Occupied move) && board !! 7 == (Occupied move) && board !! 8 == (Occupied move),
    -- check left column
    board !! 0 == (Occupied move) && board !! 3 == (Occupied move) && board !! 6 == (Occupied move),
    -- check middle column
    board !! 1 == (Occupied move) && board !! 4 == (Occupied move) && board !! 7 == (Occupied move),
    -- check right column
    board !! 2 == (Occupied move) && board !! 5 == (Occupied move) && board !! 8 == (Occupied move),
    -- check top left -> bottom right
    board !! 0 == (Occupied move) && board !! 4 == (Occupied move) && board !! 8 == (Occupied move),
    -- check bottom left -> top right
    board !! 6 == (Occupied move) && board !! 4 == (Occupied move) && board !! 2 == (Occupied move)
  ]

isDraw :: [Cell] -> Bool
isDraw board = foldr (&&) True (map (\x -> board !! x /= Null) [0..8])

tictactoe :: IO ()
tictactoe = do
  let newBoard = replicate 9 Null
  playRound O newBoard