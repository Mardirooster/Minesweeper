-- file: Minesweeper/main.hs

type Row = [Char]
type Board = [Row]
type VisibleBoard = Board
type HiddenBoard = Board
type Game = (HiddenBoard,VisibleBoard)
type Position = (Int, Int)


-- width -> row, empty char is space 
row :: Int -> Row
row 0 = []
row x = ' ':(row (x-1))


-- height width -> board
board :: Int -> Int -> Board
board 0 _ = []
board h w = (row w) : (board (h-1) w)


-- row number, position 
getRowPositions :: Int -> Int -> [Position]
getRowPositions row 1 = [(row-1, 0)]
getRowPositions row y = (row-1, y-1) : (getRowPositions row (y-1))

getPositions :: Int -> Int -> [Position]
getPositions 0 _ = []
getPositions y width = (getPositions (y-1) width) ++ (getRowPositions y width)