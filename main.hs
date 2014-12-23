-- file: Minesweeper/main.hs

module Minesweeper where

import System.Random
import Data.List
import Data.Char
import Data.Text

type Row = [Char]
type Board = [Row]
type VisibleBoard = Board
type HiddenBoard = Board
type Game = (HiddenBoard,VisibleBoard)
type Position = (Int, Int)
type Size = (Int, Int)

mine = 'X'

width = 5
height = 5

lastof :: [a] -> [a]
lastof [] = []
lastof xs = [last xs]

initof :: [a] -> [a]
initof [] = []
initof xs = init xs


shuffle' :: [Int] -> [a]  -> Int -> [a]
shuffle' _ xs 0 = xs
shuffle' (i:is) xs len = let (firsts, rest) = splitAt ((i `mod` (length xs))) xs
                     in (lastof firsts) ++ shuffle' is (initof firsts ++ rest) (len-1)

shuffle :: StdGen -> [a] -> [a]
shuffle g xs = shuffle' (randoms g) xs (length xs) 


-- initialising functions, create board filled with a character
-- width -> row, empty char is space 
row :: Int -> Char -> Row
row 0 _ = []
row x char = char:(row (x-1) char)

-- height width -> board
board :: Int -> Int -> Char -> Board
board 0 _ _ = []
board h w char = (row w char) : (board (h-1) w char)

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


--genBoard :: StdGen -> Size -> Int -> Board -> Board


minePositions :: StdGen -> Size -> Int -> [Position]
minePositions gen (h,w) mines = let shuffledposition = shuffle gen (getPositions h w)
                                in take mines shuffledposition

rowNumbers :: Board -> Row -> Int -> Int -> Size -> Row
rowNumbers _ [] _ _ _ = []
rowNumbers board (r:rs) currx curry size = 
    if r /= 'X'
        then intToDigit (countMinesSurrounding (currx,curry) size board) : (rowNumbers board rs currx (curry+1) size)
        else r : (rowNumbers board rs currx (curry+1) size)

rowIterNumbers :: Board -> Board -> Int -> Size -> Board
rowIterNumbers _ [] _ _ = []
rowIterNumbers board (b:bs) currx size = (rowNumbers board b currx 0 size) : (rowIterNumbers board bs (currx+1) size) 

setNumbers :: Board -> Size -> HiddenBoard
setNumbers b (x,y) = rowIterNumbers b b 0 (x,y)



rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))


printBoard :: Board -> String
printBoard [] = "\n"
printBoard (b:bs) = b ++ "\n" ++ printBoard bs

revealSpot ::  HiddenBoard -> [Position] -> [Position] -> VisibleBoard -> Size-> VisibleBoard
revealSpot hiddenboard [] traversed board size = board
revealSpot hiddenboard (p:ps) traversed board size = 
    if spot == '0'
        then revealSpot hiddenboard toreveal (p:traversed) (setPosition '-' p board) size
        else revealSpot hiddenboard ps (p:traversed) (setPosition spot p board) size
        where
            spot = peekPosition p hiddenboard
            toreveal = union ps minustraversed
                where minustraversed = deleteFirstsBy (==) (getSizeFilteredSurroundingPositions p size) traversed

cleanUserInput :: Size -> IO [[Char]]
cleanUserInput (x,y) = do
    input <- getLine

    let splitline = splitOn " " input

    if length splitline < 3
        then 
            putStrLn "2 short shawty try agan"
            cleanUserInput (x,y)
        else if (((splitline !! 1) :: Int) < x) && (((splitline !! 2 ) :: Int) < y)
            then return splitline
            else
                putStrLn "way outa bounds son"
                cleanUserInput (x,y)

game :: HiddenBoard -> VisibleBoard -> Size -> String
game hiddenboard visibleboard size = do
    putStrLn "don hate tha playa"
    putStrLn "mak yo move"


main :: IO()
main = do
    randgen <- newStdGen
    let emptyboard = board 10 10 '-'
    let size = (10,10)
    let positions = minePositions randgen size 10
    let minedboard = setPositions 'X' positions emptyboard
    let hiddenboard = setNumbers minedboard size
    let playerboard = board 10 10 '#'



    --print minedboard
    putStrLn $ printBoard hiddenboard
    putStrLn $ printBoard playerboard
    putStrLn $ printBoard $ revealSpot hiddenboard [(4,4)] [] playerboard size