-- file: Minesweeper/main.hs

import Data.Random

type Row = [Char]
type Board = [Row]
type VisibleBoard = Board
type HiddenBoard = Board
type Game = (HiddenBoard,VisibleBoard)
type Position = (Int, Int)
type Size = (Int, Int)

mine = 'X'


-- width -> row, empty char is space 
row :: Int -> Row
row 0 = []
row x = ' ':(row (x-1))


-- height width -> board
board :: Int -> Int -> Board
board 0 _ = []
board h w = (row w) : (board (h-1) w)



getMinePositions :: [Position] -> []


-- row number, position 
getRowPositions :: Int -> Int -> [Position]
getRowPositions row 1 = [(row-1, 0)]
getRowPositions row y = (row-1, y-1) : (getRowPositions row (y-1))

getPositions :: Int -> Int -> [Position]
getPositions 0 _ = []
getPositions y width = (getPositions (y-1) width) ++ (getRowPositions y width)

flag :: Position -> VisibleBoard -> VisibleBoard
flag = setPosition 'F'


setRowPosition :: Int -> Char -> Row -> Row
setRowPosition 0 c (r:rs) = c : rs
setRowPosition y c (r:rs) = r : (setRowPosition (y-1) c rs)

-- place a character at a position on the board
setPosition :: Char -> Position -> Board -> Board
setPosition c (0,y) (b:bs) = (setRowPosition y c b) : bs
setPosition c (x,y) (b:bs) = b : (setPosition c (x-1,y) bs)

-- place a character at a list of positions on the board
setPositions :: Char -> [Position] -> Board -> Board
setPositions _ [] board = board
setPositions char (p:ps) board = setPosition char p (setPositions char ps board)



peekRowPosition :: Int -> Row -> Char
peekRowPosition 0 (r:_) = r
peekRowPosition y (_:rs) = peekRowPosition (y-1) rs

-- look at a charactera at a position on the board
peekPosition :: Position -> Board -> Char
peekPosition (0,y) (b:_) = peekRowPosition y b
peekPosition (x,y) (_:bs) = peekPosition (x-1,y) bs

getSurroundingPositions :: Position -> [Position]
getSurroundingPositions (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)] --  (x,y),

filterPositions :: Size -> [Position] -> [Position]
filterPositions _ [] = []
filterPositions (sx,sy) ((x,y):ps) =
    if (x < sx) && (y < sy) && (x > -1) && (y > -1)
        then (x,y) : (filterPositions (sx,sy) ps)
        else filterPositions (sx,sy) ps

getSizeFilteredSurroundingPositions :: Position -> Size -> [Position]
getSizeFilteredSurroundingPositions position size = filterPositions size (getSurroundingPositions position)

countMines :: [Position] -> Board -> Int
countMines [] _ = 0
countMines (p:ps) board = 
    if char == mine
        then 1 + countMines ps board
        else countMines ps board
    where char = peekPosition p board


countMinesSurrounding :: Position -> Size -> Board -> Int
countMinesSurrounding position size board = countMines minepositions board
    where minepositions = getSizeFilteredSurroundingPositions position size

mySample :: StdGen -> Int -> [a] -> [a]
mySample g n xs = evalState (runRVar (sample n xs) StdRandom) g

