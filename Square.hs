module Square where

-- square as a smallest part of board
data Square = Square SquareType Coor deriving (Eq)

-- all types of squares - Empty as possible and Rejected as impossible place for tank, House and Tank as squares with house and tank respectively
data SquareType = Empty | Rejected | House | Tank deriving (Eq)

-- coordinates of square
data Coor = Coor Int Int deriving (Eq)

-- how to show coordinates
instance Show Coor where
    show (Coor x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
 
-- how to show squares of certain type 
instance Show Square where
    show (Square Empty _) = "[ ]"
    show (Square Rejected _) = "[X]"
    show (Square House _) = "[H]"
    show (Square Tank _) = "[T]"

-- print single square coordinates
writeCoor (Square typ c) = show c

-- get Empty type square which is x rows down and y columns right than specific square
nearEmptySquare (Square Empty coor) x y = Square Empty (moveCoor coor x y)

-- move to another coordinates by x rows down and y columns right
moveCoor (Coor row col) x y = Coor (row+x) (col+y)

-- get specific column of squares
getCol [] _ = []
getCol (s:squares) y = s !! y : getCol squares y

-- get specific row of squares
getRow squares x = squares !! x

-- returns true if square's type equals certain type
isType typ (Square sTyp _) = typ == sTyp

-- returns true if square's type is Empty             
isEmpty (Square typ _) = typ == Empty

-- returns true if square's type is Tank
isTank (Square typ _) = typ == Tank

-- returns true if square's type is Rejected
isRejected (Square typ _) = typ == Rejected

-- returns true if square's type is House
isHouse (Square typ _) = typ == House

-- change type of squares
changeType (Square _ c) typ = Square typ c

-- get square from table of squares
getSquare row col squares = squares !! row !! col

-- get type of square
getType (Square typ _) = typ