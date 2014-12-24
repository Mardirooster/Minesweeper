-- file: Minesweeper/main.hs

module Minesweeper where

--import System.Random
--import Data.List
--import Data.Char
--import Data.Text (splitOn)

import System.Random
import Data.Array.IArray
import Data.Char
import Data.List.Split
import Data.Function (on)
import Data.List

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
board :: Size -> Char -> Board
board (0,_) _ = []
board (h,w) char = (row w char) : (board (h-1,w) char)

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


characterFilterPositions :: Board -> [Char] -> [Position] -> [Position]
characterFilterPositions board char (p:ps) = 
    if elem (peekPosition p board) char
        then p : characterFilterPositions board char ps
        else characterFilterPositions board char ps


getSizeFilteredSurroundingPositions :: Position -> Size -> [Position]
getSizeFilteredSurroundingPositions position size = filterPositions size (getSurroundingPositions position)

countChars :: [Position] -> [Char] -> Board -> Int
countChars [] _ _ = 0
countChars (p:ps) character board = 
    if elem char character
        then 1 + countChars ps character board
        else countChars ps character board
    where char = peekPosition p board


countCharsSurrounding :: Position -> [Char] -> Size -> Board -> Int
countCharsSurrounding position char size board = countChars charpositions char board
    where charpositions = getSizeFilteredSurroundingPositions position size


--genBoard :: StdGen -> Size -> Int -> Board -> Board


minePositions :: StdGen -> Size -> Int -> [Position]
minePositions gen (h,w) mines = let shuffledposition = shuffle gen (getPositions h w)
                                in take mines shuffledposition

rowNumbers :: Board -> Row -> Int -> Int -> Size -> Row
rowNumbers _ [] _ _ _ = []
rowNumbers board (r:rs) currx curry size = 
    if r /= 'X'
        then intToDigit (countCharsSurrounding (currx,curry) ['X'] size board) : (rowNumbers board rs currx (curry+1) size)
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

lost :: VisibleBoard -> Bool
lost [] = False
lost (b:bs) = elem 'X' b || (lost bs)

rowWon :: Row -> Row -> Bool
rowWon [] [] = True
rowWon (v:vs) (h:hs) = 
    if (v == '#' && h /= 'X')
        then False
        else rowWon vs hs

won :: VisibleBoard -> HiddenBoard -> Bool
won [] [] = True
won (v:vs) (h:hs) = (rowWon v h) && (won vs hs)

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
cleanUserInput size = do
    input <- getLine

    let splitinput = splitOn " " input

    let (maxx, maxy) = size
    let x = read (splitinput !! 1)
    let y = read (splitinput !! 2)
    if length splitinput > 3
        then do
            putStrLn "2 long shawty try agan"
            cleanUserInput size
        else if (x > (maxx-1) || x < 0 || y > (maxy-1) || y < 0) 
            then do 
                putStrLn "way outa bounds"
                cleanUserInput size
            else return splitinput

makeMove :: HiddenBoard -> VisibleBoard -> Size -> Position -> String -> VisibleBoard
makeMove hiddenboard visibleboard size position movetype =
    if movetype == "fleg"
        then
            flag position visibleboard
        else if movetype == "demine"
            then revealSpot hiddenboard [position] [] visibleboard size
            else autoMove hiddenboard visibleboard size

game :: HiddenBoard -> VisibleBoard -> Size -> IO [Char]
game hiddenboard visibleboard size = do
    putStrLn $ printBoard visibleboard
    putStrLn "\ndon hate tha playa"
    putStrLn "mak yo move"
    putStrLn "demine to deal with tha real shit"
    putStrLn "fleg if ur a fkn wimp\n"

    input <- cleanUserInput size

    let movetype = input !! 0
--    let (maxx, maxy) = size
    let x = read (input !! 1)
    let y = read (input !! 2)
--    if (x < maxx && x >= 0 && y < maxy && y >= 0)
    let position = (x,y)

    let newboard = makeMove hiddenboard visibleboard size (x,y) movetype

    if (lost newboard)
        then return "we fukning losst"
        else if (won newboard hiddenboard)
            then do
                putStrLn $ printBoard newboard
                return "con fuckn gratys u can play minwseppep"
            else game hiddenboard newboard size
    --if movetype == "demine"
    --    then revealSpot hiddenboard [((input !! 1) :: Int), ((input !! 2) :: Int)] [] visibleboard size
    --    else return "fk u dis ain't done yet"

flagall :: [Position] -> VisibleBoard -> VisibleBoard
flagall [] board = board
flagall (p:ps) board = flagall ps (flag p board)

findBasicSafeMoves :: [Position] -> VisibleBoard -> Size -> [Position]
findBasicSafeMoves [] _ _ = []
findBasicSafeMoves (p:ps) board size =
    if (countCharsSurrounding p ['#', 'F'] size board) == digitToInt (peekPosition p board)
        then characterFilterPositions board ['#', 'F'] (getSizeFilteredSurroundingPositions p size) `union` findBasicSafeMoves ps board size
        else findBasicSafeMoves ps board size

autoMove :: HiddenBoard -> VisibleBoard -> Size -> VisibleBoard
autoMove hiddenboard visibleboard (x,y) = revealSpot hiddenboard (lastof moves) [] visibleboard (x,y)
    where moves = findBasicSafeMoves (getPositions x y) visibleboard (x,y)




main :: IO()
main = do


    randgen <- newStdGen
    let emptyboard = board (10,10) '-'
    let size = (10,10)
    let positions = minePositions randgen size 1
    let minedboard = setPositions 'X' positions emptyboard
    let hiddenboard = setNumbers minedboard size
    let playerboard = board (10,10) '#'


    gameresult <- game hiddenboard playerboard size
    putStrLn gameresult

    --print minedboard
    --putStrLn $ printBoard hiddenboard
    --putStrLn $ printBoard playerboard
    --putStrLn $ printBoard $ revealSpot hiddenboard [(4,4)] [] playerboard size