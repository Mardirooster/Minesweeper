-- file: Minesweeper/main.hs

module Minesweeper where

import System.Random
import Data.Char
import Data.List.Split
import Data.Function
import Data.List

type Row = [Char]
type Board = [Row]
type VisibleBoard = Board
type HiddenBoard = Board
type Game = (HiddenBoard,VisibleBoard)
type Position = (Int, Int)
type Size = (Int, Int)

mine = 'X'

width = 20
height = 20
mines = 40


-- specific versions of functions

lastof :: [a] -> [a]
lastof [] = []
lastof xs = [last xs]


initof :: [a] -> [a]
initof [] = []
initof xs = init xs


-- shuffle function, shuffles a list by creating a seed list to shuffle with

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


-- get list of all possible positions

getPositions :: Int -> Int -> [Position]
getPositions 0 _ = []
getPositions y width = (getPositions (y-1) width) ++ (getRowPositions y width)


-- basic flag function

flag :: [Position] -> VisibleBoard -> VisibleBoard
flag = setPositions 'F'


-- place a character at a position on the board

setRowPosition :: Int -> Char -> Row -> Row
setRowPosition 0 c (r:rs) = c : rs
setRowPosition y c (r:rs) = r : (setRowPosition (y-1) c rs)

setPosition :: Char -> Position -> Board -> Board
setPosition c (0,y) (b:bs) = (setRowPosition y c b) : bs
setPosition c (x,y) (b:bs) = b : (setPosition c (x-1,y) bs)


-- place a character at a list of positions on the board

setPositions :: Char -> [Position] -> Board -> Board
setPositions _ [] board = board
setPositions char (p:ps) board = setPosition char p (setPositions char ps board)


-- look at a charactera at a position on the board

peekRowPosition :: Int -> Row -> Char
peekRowPosition 0 (r:_) = r
peekRowPosition y (_:rs) = peekRowPosition (y-1) rs

peekPosition :: Position -> Board -> Char
peekPosition (0,y) (b:_) = peekRowPosition y b
peekPosition (x,y) (_:bs) = peekPosition (x-1,y) bs


-- get surrounding positions

getSurroundingPositions :: Position -> [Position]
getSurroundingPositions (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)] --  (x,y),


-- generic square element returner (including center, to be used for advanced autosolver) 

getSquarePositions :: Position -> [Position]
getSquarePositions (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)] --  (x,y),


-- filter positions based on a maximum size

filterPositions :: Size -> [Position] -> [Position]
filterPositions _ [] = []
filterPositions (sx,sy) ((x,y):ps) =
    if (x < sx) && (y < sy) && (x > -1) && (y > -1)
        then (x,y) : (filterPositions (sx,sy) ps)
        else filterPositions (sx,sy) ps


-- filter a list of positions based on the characters those positions reference on the board

characterFilterPositions :: Board -> [Char] -> [Position] -> [Position]
characterFilterPositions _ _ [] = []
characterFilterPositions board char (p:ps) = 
    if elem (peekPosition p board) char
        then p : characterFilterPositions board char ps
        else characterFilterPositions board char ps


-- get entire set of elements centered around square : to be used in more general autosolver

getSizeFilteredSquarePositions :: Position -> Size -> [Position]
getSizeFilteredSquarePositions position size = filterPositions size (getSquarePositions position)


-- surrounding elements for a position filtered for min and max values

getSizeFilteredSurroundingPositions :: Position -> Size -> [Position]
getSizeFilteredSurroundingPositions position size = filterPositions size (getSurroundingPositions position)


-- count chars in a list of positions by looking them up in a board

countChars :: [Position] -> [Char] -> Board -> Int
countChars [] _ _ = 0
countChars (p:ps) character board = 
    if elem char character
        then 1 + countChars ps character board
        else countChars ps character board
    where char = peekPosition p board



-- count the number of elements surrounding a position that contain a member of a list of characters

countCharsSurrounding :: Position -> [Char] -> Size -> Board -> Int
countCharsSurrounding position char size board = countChars charpositions char board
    where charpositions = getSizeFilteredSurroundingPositions position size


--genBoard :: StdGen -> Size -> Int -> Board -> Board



-- get a list of random positions for mines within a size

minePositions :: StdGen -> Size -> Int -> [Position]
minePositions gen (h,w) mines = let shuffledposition = shuffle gen (getPositions h w)
                                in take mines shuffledposition



-- set all the numbers in the hidden board at the start
-- for each element in each row, if it's not a mine, check all the surrounding elements for mines

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


-- display board as lines of strings

printBoard :: Board -> String
printBoard [] = "\n"
printBoard (b:bs) = b ++ "\n" ++ printBoard bs


-- if a single mine is visible, the game is lost

lost :: VisibleBoard -> Bool
lost [] = False
lost (b:bs) = elem 'X' b || (lost bs)



-- given a board, check to see if it's completed : if all elements still covered are mines, the board is won

rowWon :: Row -> Row -> Bool
rowWon [] [] = True
rowWon (v:vs) (h:hs) = 
    if (v == '#' && h /= 'X')
        then False
        else rowWon vs hs

won :: VisibleBoard -> HiddenBoard -> Bool
won [] [] = True
won (v:vs) (h:hs) = (rowWon v h) && (won vs hs)


-- reveal a spot using the hidden board
-- if that spot is a 0, reveal all surrounding spots by adding them to a queue
--      the queue is handled with unions to avoid repeats, and traversed revealed spots are added to a list
--      to stop the function running in circles

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



-- a messy function to make sure the input is in a passable format for the main game function

splitInput :: [Char] -> [[Char]]
splitInput input =
    if length splitinput > 2
        then splitinput
        else splitinput ++ ["0", "0"]
        where splitinput = splitOn " " input



-- get user input and ensure it is of a certain form

cleanUserInput :: Size -> IO [[Char]]
cleanUserInput size = do
    input <- getLine

    let splitinput = splitInput input

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



-- make a specific move : either on a specific point or perform the next automove

makeMove :: StdGen -> HiddenBoard -> VisibleBoard -> Size -> Position -> String -> VisibleBoard
makeMove gen hiddenboard visibleboard size position movetype =
    if movetype == "fleg"
        then
            flag [position] visibleboard
        else if movetype == "demine"
            then revealSpot hiddenboard [position] [] visibleboard size
            else autoMove gen hiddenboard (autoFlag hiddenboard visibleboard size) size



-- main game loop

game :: StdGen -> HiddenBoard -> VisibleBoard -> Size -> IO [Char]
game gen hiddenboard visibleboard size = do
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

    let newboard = makeMove gen hiddenboard visibleboard size (x,y) movetype

    if (lost newboard)
        then do
            putStrLn $ printBoard newboard
            return "we fukning losst"
        else if (won newboard hiddenboard)
            then do
                putStrLn $ printBoard newboard
                return "con fuckn gratys u can play minwseppep"
            else game gen hiddenboard newboard size
    --if movetype == "demine"
    --    then revealSpot hiddenboard [((input !! 1) :: Int), ((input !! 2) :: Int)] [] visibleboard size
    --    else return "fk u dis ain't done yet"


-- automated solver stuff

-- flag a list of positions on a 

flagall :: [Position] -> VisibleBoard -> VisibleBoard
flagall [] board = board
flagall (p:ps) board = flagall ps (flag [p] board)


-- find the lowhanging fruit : pick out the mines that are easily found by flagging the edges of any entirely fulfilled revealed squares
-- the word fulfilled here is used rather hyperbolically to mean squares that have the same number of surrounding unrevealed squares as their number

findBasicMines :: [Position] -> VisibleBoard -> Size -> [Position]
findBasicMines [] _ _ = []
findBasicMines (p:ps) board size =
    if (elem (peekPosition p board) ['1'..'8'])
        then if (countCharsSurrounding p ['#', 'F'] size board) == digitToInt (peekPosition p board)
            then characterFilterPositions board ['#', 'F'] (getSizeFilteredSurroundingPositions p size) `union` findBasicMines ps board size
            else findBasicMines ps board size
        else findBasicMines ps board size


autoFlag :: HiddenBoard -> VisibleBoard -> Size -> VisibleBoard
autoFlag hiddenboard visibleboard (x,y) = flag moves visibleboard --revealSpot hiddenboard (lastof moves) [] visibleboard (x,y)
    where moves = findBasicMines (getPositions x y) visibleboard (x,y)


-- find the basic moves based on the overlap between flagged elements and non-fulfilled revealed squares

findBasicMoves :: [Position] -> VisibleBoard -> Size -> [Position]
findBasicMoves [] _ _ = []
findBasicMoves (p:ps) board size =
    if (elem (peekPosition p board) ['1'..'8'])
        then if (countCharsSurrounding p ['F'] size board) == digitToInt (peekPosition p board)
            then characterFilterPositions board ['#'] (getSizeFilteredSurroundingPositions p size) `union` findBasicMoves ps board size
            else findBasicMoves ps board size
        else findBasicMoves ps board size


-- an attempt at looking for more advanced patterns

--findPatternMoves :: [Position] -> VisibleBoard -> Size -> [Position]
--findPatternMoves [] _ _ = []
--findPatternMoves (p:ps) board size =
--    if (elem (peekPosition p board) ['1'..'8'])


--pattern = [("###121---",(1,-1)), ("#1-#2-#1-",(-1,-1)), ("-1#-2#-1#", ()


-- make the moves determined by findBasicMoves

autoMove :: StdGen -> HiddenBoard -> VisibleBoard -> Size -> VisibleBoard
autoMove gen hiddenboard visibleboard (x,y) = 
    if (null moves)
        then revealSpot hiddenboard guess [] visibleboard (x,y)
        else revealSpot hiddenboard moves [] visibleboard (x,y) --revealSpot hiddenboard (lastof moves) [] visibleboard (x,y)
        where
            moves = findBasicMoves (getPositions x y) visibleboard (x,y)
            guess = randomNextMove gen visibleboard (x,y)

randomNextMove :: StdGen -> VisibleBoard -> Size-> [Position]
randomNextMove gen board (x,y) = let positions = characterFilterPositions board ['#'] (shuffle gen (getPositions x y))
                                in lastof positions


main :: IO()
main = do
    let size = (height, width)

    randgen <- newStdGen
    let emptyboard = board size '-'
    let positions = minePositions randgen size mines
    let minedboard = setPositions 'X' positions emptyboard
    let hiddenboard = setNumbers minedboard size
    let playerboard = board size '#'


    gameresult <- game randgen hiddenboard playerboard size
    putStrLn gameresult