-- file: Minesweeper/main.hs

type Row = [Char]
type Board = [Row]
type VisibleBoard = Board
type HiddenBoard = Board
type Game = (HiddenBoard,VisibleBoard)
type Position = (Int, Int)

width = 5
height = 5

testboard = [['O','-','O','-','O'],['-','-','-','-','O'],['O','-','-','-','-'],['O','-','-','-','O'],['O','-','O','-','O']]
testplayer = [['#','#','#','#','#'],['#','#','#','#','#'],['#','#','#','#','#'],['#','#','#','#','#'],['#','#','#','#','#']]
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

flagRow :: Int -> Row -> Row
flagRow 0 (r:rs) = 'F' : rs
flagRow y (r:rs) = r : (flagRow (y-1) rs)

flag :: Position -> VisibleBoard -> VisibleBoard
flag (0,y) (b:bs) = (flagRow y b) : bs
flag (x,y) (b:bs) = b : (flag (x-1,y) bs)


