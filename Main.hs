module Main where

import System.IO
import System.Environment

import Board

readBoard fileName = do
   contents <- readFile fileName
   let linesArray = lines contents
       rows = read (linesArray !! 0) :: [Int]
       columns = read (linesArray !! 1) :: [Int]
       houses = read (linesArray !! 2) :: [(Int,Int)] in 
       return (initBoard rows columns houses)

runSolution fileName = do
   board <- readBoard fileName
   putStrLn "LOADED BOARD"
   putStrLn (show board)
   let solvedBoard = solve board
   putStrLn (show solvedBoard)
   let releasedBoard = releaseRejectionsBoard solvedBoard
   putStrLn "SOLVED BOARD"
   putStrLn (show releasedBoard)

solveAndSave fileName outputFile = do
   board <- readBoard fileName
   putStrLn (show board)
   let solvedBoard = solve board
   putStrLn (show solvedBoard)
   let releasedBoard = releaseRejectionsBoard solvedBoard
   putStrLn "SOLVED BOARD"
   save releasedBoard outputFile
   putStrLn (show releasedBoard)

save board outputFile = do writeFile outputFile ("[" ++ addCommas (concat (printTanks board)) ++  "]")

addCommas [] = []
addCommas [x] = x
addCommas (x:xs) = x ++ ", " ++ (addCommas xs)

run args progName
   | length args == 1 = runSolution (args !! 0)
   | length args == 3 && (args !! 1) == "-save" = solveAndSave (args !! 0) (args !! 2)
   | otherwise = putStrLn ("usage: " ++ progName ++ " [input_file] [-save output_file]")

-- main function
main = do
   args <- getArgs
   progName <- getProgName
   run args progName