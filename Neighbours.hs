module Neighbours where

import Square

-- get squares which are neighbours for a chosen one
getNeighbours s squares = (getNeighboursInCol s squares) ++ (getNeighboursInRow s squares)

-- get squares which are neighbours for a chosen one in a column
getNeighboursInCol (Square _ (Coor row col)) squares =
   (if row > 0                    then [getSquare (row-1) col squares] else []) ++
   (if row < (length squares) - 1 then [getSquare (row+1) col squares] else [])

-- get squares which are neighbours for a chosen one in a row
getNeighboursInRow (Square _ (Coor row col)) squares =
   (if col > 0                             then [getSquare row (col-1) squares] else []) ++
   (if col < (length (squares !! row)) - 1 then [getSquare row (col+1) squares] else [])

-- get squares which are neighbours for a chosen one and are of a certain type 
getNeighboursByType s typ squares = filter (isType typ) (getNeighbours s squares)

-- get squares which are neighbours for a chosen one and are of a certain type in a column
getNeighboursInColByType s typ squares = filter (isType typ) (getNeighboursInCol s squares)

-- get squares which are neighbours for a chosen one and are of a certain type in a row
getNeighboursInRowByType s typ squares = filter (isType typ) (getNeighboursInRow s squares)

-- check if specific square has a neighbour of a certain type in a row
hasNeighbourInCol (Square _ (Coor row col)) typ squares =
   (row > 0                    && checkNearType (-1) 0) ||
   (row < (length squares) - 1 && checkNearType   1  0)
      where checkNearType toRow toCol = getType (getSquare (row + toRow) (col + toCol) squares) == typ

-- check if specific square has a neighbour of a certain type in a column
hasNeighbourInRow (Square _ (Coor row col)) typ squares =
   (col > 0                             && checkNearType 0 (-1)) ||
   (col < (length (squares !! row)) - 1 && checkNearType 0   1 )
      where checkNearType toRow toCol = getType (getSquare (row + toRow) (col + toCol) squares) == typ

-- check if specific square has a neighbour of a certain type
hasNeighbour s typ squares = hasNeighbourInCol s typ squares || hasNeighbourInRow s typ squares

-- count neighbours of a certain type 
countNeighbours s typ squares = countNeighboursInCol s typ squares + countNeighboursInRow s typ squares

-- count neighbours of a certain type in a row
countNeighboursInRow (Square _ (Coor row col)) typ squares =
   (if col > 0                             && checkNearType 0 (-1) then 1 else 0) +
   (if col < (length (squares !! row)) - 1 && checkNearType 0   1  then 1 else 0)
      where checkNearType toRow toCol = getType (getSquare (row + toRow) (col + toCol) squares) == typ

-- count neighbours of a certain type in a column
countNeighboursInCol (Square _ (Coor row col)) typ squares =
   (if row > 0                    && checkNearType (-1) 0 then 1 else 0) +
   (if row < (length squares) - 1 && checkNearType   1  0 then 1 else 0)
      where checkNearType toRow toCol = getType (getSquare (row + toRow) (col + toCol) squares) == typ

-- check if specific square has a neighbour of a certain type, which is connected to it by theirs corners
hasCornerNeighbour (Square _ (Coor row col)) typ squares =
    (row > 0                    && col > 0                             && checkNearType (-1) (-1)) ||
    (row < (length squares) - 1 && col < (length (squares !! row)) - 1 && checkNearType   1    1 ) ||
    (row < (length squares) - 1 && col > 0                             && checkNearType   1  (-1)) ||
    (row > 0                    && col < (length (squares !! row)) - 1 && checkNearType (-1)   1)
      where checkNearType toRow toCol = getType (getSquare (row + toRow) (col + toCol) squares) == typ

getCornerNeighbours (Square _ (Coor row col)) squares =
   (if row > 0                    && col > 0                             then [getSquare (row-1) (col-1) squares] else []) ++
   (if row < (length squares) - 1 && col < (length (squares !! row)) - 1 then [getSquare (row+1) (col+1) squares] else []) ++
   (if row < (length squares) - 1 && col > 0                             then [getSquare (row+1) (col-1) squares] else []) ++
   (if row > 0                    && col < (length (squares !! row)) - 1 then [getSquare (row-1) (col+1) squares] else [])

getCornerNeighboursByType s typ squares = filter (isType typ) (getCornerNeighbours s squares)
