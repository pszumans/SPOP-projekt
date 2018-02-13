module Conditions where

import Square
import Neighbours

-- set rejection of having a tank for appropriate Empty type square - change its type to Rejected - at the beginning
initRejection (Square prevType (Coor row col)) squares rowStats colStats =
   if prevType == Empty && (
      noTanksInRow || noTanksInCol ||
      not (hasNeighbour s House squares)
   ) then changeType s Rejected
   else s
      where s = Square prevType (Coor row col)
            noTanksInRow = (rowStats !! row) == 0
            noTanksInCol = (colStats !! col) == 0

-- set init rejections for all appropriate squares in board
initRejections squares rowStats colStats = [[initRejection s squares rowStats colStats | s <- ss] | ss <- squares]

-- add tank on appropriate empty square - change type Empty to Tank for single square
addTank (Square prevType (Coor row col)) squares rowStats colStats =
   if prevType == Empty && (
      isEdgeOfOddCntEmptyPacketRow s (getRow squares row) (rowStats !! row) squares ||
      isEdgeOfOddCntEmptyPacketCol s (getCol squares col) (colStats !! col) squares ||
      checkIfNeighboursHousesMustTank hs squares
   ) then changeType s Tank
   else s
      where s = Square prevType (Coor row col)
            hs = getNeighboursByType s House squares

-- change to Tank type all appropriate Empty type squares
addTanks squares rowStats colStats = [[addTank s squares rowStats colStats | s <- ss]| ss <- squares]

-- reject appropriate empty square - change its Type from Empty to Rejected, so it is impossible to place tank on it
rejectSquare (Square prevType (Coor row col)) squares rowStats colStats =
   if prevType == Empty && (
      areEnoughTanksInRow row rowStats squares ||
      areEnoughTanksInCol col colStats squares ||
      hasNeighbour s Tank squares || hasCornerNeighbour s Tank squares ||
      hasNeighbourInDoubleEmptyPacket s squares rowStats colStats ||
      doNeighboursHousesHaveTanks hs squares ||
      willTankBeCornerNeighbour s squares rowStats colStats ||
      willTankBeNeighbour s (getCornerNeighboursByType s House squares) squares
   ) then changeType s Rejected
   else s
      where s = Square prevType (Coor row col)
            hs = getNeighboursByType s House squares

-- check if tank will be neighbour of a square, so should be rejected
willTankBeNeighbour s (h:hs) squares = (length sEmptcolStats > 1 && length hEmptcolStats == 2 && not (hasNeighbour h Tank squares)
   && contains hEmptcolStats sEmptcolStats) || willTankBeNeighbour s hs squares
   where sEmptcolStats = getNeighboursByType s Empty squares
         hEmptcolStats = getNeighboursByType h Empty squares
willTankBeNeighbour _ [] _ = False

-- list contains elements from another list
contains [] _ = True
contains (x:xs) list = elem x list && contains xs list

-- count squares filtered by f
count f squares = length (filter f squares)

-- check if tank will be corner neighbour of a square, so should be rejected
willTankBeCornerNeighbour (Square _ (Coor row col)) squares rowStats colStats =
   row > 0               && count (\x -> checkRow x (-1)) (neighboursRow (-1)) == 2 ||
   row < (length rowStats) - 1 && count (\x -> checkRow x   1 ) (neighboursRow   1 ) == 2 ||
   col > 0               && count (\x -> checkCol x (-1)) (neighboursCol (-1)) == 2 ||
   col < (length colStats) - 1 && count (\x -> checkCol x   1 ) (neighboursCol   1 ) == 2
      where s = Square Empty (Coor row col)
            checkRow x toRow = isEdgeOfOddCntEmptyPacketRow x (getRow squares (row + toRow)) (rowStats !! (row + toRow) + 1) squares
            checkCol x toCol = isEdgeOfOddCntEmptyPacketCol x (getCol squares (col + toCol)) (colStats !! (col + toCol) + 1) squares
            neighboursRow toRow = getNeighboursInRowByType (nearEmptySquare s toRow 0) Empty squares 
            neighboursCol toCol = getNeighboursInColByType (nearEmptySquare s 0 toCol) Empty squares 

-- reject all appropriate squares
rejectSquares squares rowStats colStats = [[rejectSquare s squares rowStats colStats | s <- ss] | ss <- squares]

-- check if number of tanks in row or column from stats equals to those calculated for Empty squares packets and those already with tank
checkTanks list cnt = count isTank list + countMaxTanks emptcolStats == cnt
    where emptcolStats = split isEmpty list

-- calculate maximum number of tanks in a Empty type squares packet - its ceiling of packet size divided by 2
countMaxTanks list = sum (map (\x -> ceiling (fromIntegral (length x) / 2)) list)

-- used to split row or column into Empty packets type - row is divided to packets when there is square of another type between them
split _ [] = []
split f (x:xs) = if as == [] then split f bs else as : split f bs
     where (as, bs) = if f x then span f (x:xs) else span f xs

-- check if square s is a part of an Empty packet of odd size and calculated maximum of tanks is equals to those in stats, co there could be tank on the edge of Empty packet
isInOddCntEmptyPacket s packets cnt = checkTanks packets cnt && containsInPackets s (oddLengthPackets (split isEmpty packets))

-- check if square is on the edge of an Empty packet of odd size in a row and calculated maximum of tanks is equals to those in stats
isEdgeOfOddCntEmptyPacketRow s packets cnt squares = isInOddCntEmptyPacket s packets cnt && (countNeighboursInRow s Empty squares < 2)

-- check if square is on the edge of an Empty packet of odd size in a column and calculated maximum of tanks is equals to those in stats
isEdgeOfOddCntEmptyPacketCol s packets cnt squares = isInOddCntEmptyPacket s packets cnt && (countNeighboursInCol s Empty squares < 2)

-- check if square s is an element of an Empty packet
containsInPackets s (p:packets) = elem s p || containsInPackets s packets
containsInPackets s [] = False

-- filter odd size packets
oddLengthPackets packets = filter (oddLength) packets

-- get true if packet size is odd excluding single packets
oddLength packet = odd (length packet) -- && length packet > 1

-- check if one of chosen houses has to have tank on single nearby Empty type square
checkIfNeighboursHousesMustTank [] _ = False
checkIfNeighboursHousesMustTank (h:hs) squares =
   if countNeighbours h Empty squares == 1 && not (hasNeighbour h Tank squares) then True
   else if countNeighbours h Empty squares == 1 && countNeighbours h Tank squares == 1 then checkIfHouseMustTank h t squares
   else checkIfNeighboursHousesMustTank hs squares
      where t:_ = getNeighboursByType h Tank squares

-- check if specific house has to have tank on single nearby Empty type square
checkIfHouseMustTank h t squares =
   if countNeighbours t House squares == 1 then False
   else if not (hasNeighbour nh Empty squares) && countNeighbours nh Tank squares == 1 && countNeighbours t House squares == 2 then True
   else if countNeighbours nh Tank squares == 1 then False
   else if countNeighbours nt House squares == 1 && countNeighbours nh Tank squares == 2 then False
   else
   checkIfHouseMustTank nh nt squares
      where nh:_ = reduceList (getNeighboursByType t House squares) [h]
            nt:_ = reduceList (getNeighboursByType nh Tank squares) [t]

-- reduce list of chosen elements
reduceList [] _ = []
reduceList (el:list) toRemove | elem el toRemove = reduceList list toRemove
                              | otherwise        = el : reduceList list toRemove

-- check if there are all expected tanks in a row
areEnoughTanksInRow row rowStats squares = count isTank (getRow squares row) == (rowStats !! row)

-- check if there are all expected tanks in a column
areEnoughTanksInCol col colStats squares = count isTank (getCol squares col) == (colStats !! col)

-- check if there is a neighbour in double Empty packet and calculated number of tanks equals to those from stats, so chosen square could be rejected; checked for neighbour rows and columns
hasNeighbourInDoubleEmptyPacket (Square Empty (Coor row col)) squares rowStats colStats =
   row > 0               && checkRow (-1) ||
   row < (length rowStats) - 1 && checkRow   1  ||
   col > 0               && checkCol (-1) ||
   col < (length colStats) - 1 && checkCol   1
      where s = Square Empty (Coor row col)
            checkRow toRow = isInDoubleEmptyPacket (nearEmptySquare s toRow 0) (getRow squares (row + toRow)) (rowStats !! (row + toRow))
            checkCol toCol = isInDoubleEmptyPacket (nearEmptySquare s 0 toCol) (getCol squares (col + toCol)) (colStats !! (col + toCol))

-- check if there is a neighbour in double Empty packet and calculated number of tanks equals to those from stats, so chosen square could be rejected
isInDoubleEmptyPacket s packets cnt = checkTanks packets cnt && containsInPackets s (twoLengthPackets (split isEmpty packets))

-- filter two size packets
twoLengthPackets packets = filter (twoLength) packets

-- get true if packet has two elements
twoLength packet = length packet == 2

-- check if at least one of chosen houses have his own tank, so another neighbour squares could be rejected 
doNeighboursHousesHaveTanks [] _ = True
doNeighboursHousesHaveTanks (h:hs) squares =
   if (countNeighbours h Tank squares > 0) then (
      countNeighbours h Tank squares == 1 &&
      (countNeighbours t House squares == 1 || checkNextHouseTank h t squares) &&
      doNeighboursHousesHaveTanks hs squares
   ) else False
      where t = (getNeighboursByType h Tank squares) !! 0

-- check if certainly next house has his own tank
checkNextHouseTank h t squares =
   if (not (hasNeighbour nh Empty squares)    && countNeighbours nh Tank squares == 1 && countNeighbours t House squares == 2) then False
   else if (countNeighbours nh Tank squares == 1) then False
   else if (countNeighbours nt House squares == 1 && countNeighbours nh Tank squares == 2) then True
   else checkNextHouseTank nh nt squares
      where nh:_ = reduceList (getNeighboursByType t House squares) [h]
            nt:_ = reduceList (getNeighboursByType nh Tank squares) [t]
