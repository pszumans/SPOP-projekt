module Board where

import Square
import Neighbours
import Conditions

-- game board
data Board = Board [[Square]] [Int] [Int] deriving (Eq)

-- show number of tanks in a rows, next to specific row
showRowStats (s:squares) x = show s ++ showRowStats squares x
showRowStats [] x = show x ++ "\n"

-- show number of tanks in a column, beneath specific columns
showColStats (y:ys) = " " ++ show y ++ " " ++ showColStats ys 
showColStats [] = "\n"

-- how to show game board
instance Show Board where
    show (Board (s:squares) (x:xs) y) = showRowStats s x ++ show (Board squares xs y)
    show (Board [] x y) = showColStats y

-- print all tanks coordinates
printTanks (Board squares _ _) = [[writeCoor s | s <- ss, isTank s] | ss <- squares]

-- solve board
solve board = solveBoard (initRejectionsBoard board)                      

-- repeat adding tanks and rejecting until board is solved
solveBoard b = if b == addTanksAndReject b then b else (solveBoard . addTanksAndReject) b

-- check if board is solved
isSolved (Board squares [] _) = True
isSolved (Board (s:squares) (x:xs) y) = count isTank s == x && isSolved (Board squares xs y)

-- set single square - Empty or House type
setSquare row col h = if elem (row, col) h then Square House (Coor row col) else Square Empty (Coor row col)

-- init all squares for board from dimensions a, b and houses array
initSquares a b houses = [[setSquare row col houses | col <- [0..b]] | row <- [0..a]]

-- init Board from init squares and number of tanks stats
initBoard x y h = Board (initSquares (length x - 1) (length y - 1) h) x y

-- set Board with firstly rejected squares
initRejectionsBoard (Board squares rowStats colStats) = Board (initRejections squares rowStats colStats) rowStats colStats

-- set Board for squares with added tanks
addTanksBoard (Board squares rowStats colStats) = Board (addTanks squares rowStats colStats) rowStats colStats

-- set Board for rejected squares
rejectSquaresBoard (Board squares rowStats colStats) = Board (rejectSquares squares rowStats colStats) rowStats colStats

-- change Rejected type to Empty
releaseRejection s = if isRejected s then changeType s Empty else s

-- clear squares from Rejected type squares
releaseRejections squares = [[releaseRejection s | s <- ss] | ss <- squares]

-- clear Board from Rejected type squares
releaseRejectionsBoard (Board squares rowStats colStats) = Board (releaseRejections squares) rowStats colStats

-- adding tanks and rejecting appropriate squares
addTanksAndReject b = (rejectSquaresBoard . addTanksBoard) b

-- get squares of board
getSquares (Board squares _ _) = squares