--This is a cool minesweeper game
import Data.List
import System.IO
import System.Random        -- cabal install random
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad (when)
import System.Directory

data Grid = Grid {
    location :: (Int, Int), 
    mined :: Bool, 
    reached :: Bool,
    reachedA :: Bool,
    reachedB :: Bool,
    flagged :: Bool,
    flaggedA :: Bool,
    flaggedB :: Bool,
    num :: Int      -- Number of mines around the grid
    } deriving (Show)

data Record = Record {
    difficulty :: String,
    name :: String,
    time :: Int
} deriving (Show)

instance Eq Record where
    (Record _ _ i1) == (Record _ _ i2) = i1 == i2

instance Ord Record where
    (Record _ _ i1) `compare` (Record _ _ i2) = i1 `compare` i2

firTup (a,b) = a
secTup (a,b) = b

mynot True = False
mynot False = True

initRow :: Int -> Int -> Int -> [(Int, Int)] -> [Grid]
initRow x y length z
    | elem (x, y) z == True = Grid {location = (x, y), mined = True, reached = False, reachedA = False, reachedB = False, flagged = False, flaggedA = False, flaggedB = False, num = 0} : initRow x (y+1) length z
    | y < length = Grid {location = (x, y), mined = False, reached = False, reachedA = False, reachedB = False, flagged = False, flaggedA = False, flaggedB = False, num = 0} : initRow x (y+1) length z
    | otherwise = []

buildBoard :: Int -> Int -> Int -> Int -> [(Int, Int)] -> [[Grid]]
buildBoard x y width length z
    | x < width = initRow x y length z: (buildBoard (x+1) y width length z)
    | otherwise = []

-- mineMap = buildBoard mapLength mapWidth mapMinePos

allPairs w l = [(x,y) | x <- w, y <- l]

matchIndexPos a indexes positions
    | a /= length indexes = positions !! (indexes !! a) : matchIndexPos (a+1) indexes positions
    | otherwise = []

randomIndexes width length num =
    do
        g <- newStdGen
        return $ take num $ nub $ randomRs (0, width * length - 1 :: Int) g

-- Function to create a random mine positions list
randomPositions width length num = 
    do
        indexes <- randomIndexes width length num
        return $ matchIndexPos 0 indexes $ allPairs [0..width-1] [0..length-1]

generateRow :: [Grid] -> String
generateRow [] = []
generateRow (h : t) 
    | mined h = "[X]" ++ generateRow t
    | flagged h = "[F]" ++ generateRow t
    | reachedA h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateRow t
    | reachedA h && num h == 0 = "[A]" ++ generateRow t
    | reachedB h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateRow t
    | reachedB h && num h == 0 = "[B]" ++ generateRow t
    | otherwise = "[|]" ++ generateRow t

generateBoard :: [[Grid]] -> Int -> String
generateBoard [] _ = []
generateBoard (h : t) c
    | c < 10 = (show c) ++ " " ++  generateRow h ++ "\n" ++ generateBoard t (c+1)
    | otherwise = (show c) ++ generateRow h ++ "\n" ++ generateBoard t (c+1) 

generateColumnCoord [] _ = []
generateColumnCoord board c
    | c == 0 = "   " ++ (show c) ++ generateColumnCoord board (c+1)
    | c < length (board!!0) && c < 10 = "  " ++ (show c) ++ generateColumnCoord board (c+1)
    | c < length (board!!0) && c >= 10 = " " ++ (show c) ++ generateColumnCoord board (c+1)
    | otherwise = []

-- Hide mine version
generateRow_hide :: [Grid] -> String
generateRow_hide [] = []
generateRow_hide (h : t) 
    | flagged h = "[F]" ++ generateRow_hide t
    | reachedA h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateRow_hide t
    | reachedA h && num h == 0 = "[A]" ++ generateRow_hide t
    | reachedB h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateRow_hide t
    | reachedB h && num h == 0 = "[B]" ++ generateRow_hide t
    | otherwise = "[|]" ++ generateRow_hide t

generateBoard_hide :: [[Grid]] -> Int -> String
generateBoard_hide [] _ = []
generateBoard_hide (h : t) c
    | c < 10 = (show c) ++ " " ++  generateRow_hide h ++ "\n" ++ generateBoard_hide t (c+1)
    | otherwise = (show c) ++ generateRow_hide h ++ "\n" ++ generateBoard_hide t (c+1)

-- Hide mine version
generateSingleRow_hide :: [Grid] -> String
generateSingleRow_hide [] = []
generateSingleRow_hide (h : t) 
    | flagged h = "[F]" ++ generateSingleRow_hide t
    | reached h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateSingleRow_hide t
    | reached h && num h == 0 = "[ ]" ++ generateSingleRow_hide t
    | otherwise = "[|]" ++ generateSingleRow_hide t

generateSingleBoard_hide :: [[Grid]] -> Int -> String
generateSingleBoard_hide [] _ = []
generateSingleBoard_hide (h : t) c
    | c < 10 = (show c) ++ " " ++  generateSingleRow_hide h ++ "\n" ++ generateSingleBoard_hide t (c+1)
    | otherwise = (show c) ++ generateSingleRow_hide h ++ "\n" ++ generateSingleBoard_hide t (c+1)

generateSingleRow :: [Grid] -> String
generateSingleRow [] = []
generateSingleRow (h : t) 
    | mined h = "[X]" ++ generateSingleRow t
    | flagged h = "[F]" ++ generateSingleRow t
    | reached h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateSingleRow t
    | reached h && num h == 0 = "[ ]" ++ generateSingleRow t
    | otherwise = "[|]" ++ generateSingleRow t

generateSingleBoard :: [[Grid]] -> Int -> String
generateSingleBoard [] _ = []
generateSingleBoard (h : t) c
    | c < 10 = (show c) ++ " " ++  generateSingleRow h ++ "\n" ++ generateSingleBoard t (c+1)
    | otherwise = (show c) ++ generateSingleRow h ++ "\n" ++ generateSingleBoard t (c+1) 


    
clickRow :: Int -> [Grid] -> Int -> [Grid]
clickRow _ [] _ = []
clickRow x (h : t) c
    | c == x = Grid {location = location h, mined = mined h, reached = True, reachedA = reachedA h, reachedB = reachedB h, flagged = False, flaggedA = False, flaggedB = False, num = num h} : t
    | otherwise = h : clickRow x t (c + 1)

click :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
click _ _ [] _ = []
click x y (h : t) c
    | c == y = clickRow x h 0 : t                 
    | otherwise = h : click x y t (c + 1)

clickRowA :: Int -> [Grid] -> Int -> [Grid]
clickRowA _ [] _ = []
clickRowA x (h : t) c
    | (c == x) && (not (reachedB h)) = Grid {location = location h, mined = mined h, reached = reached h, reachedA = True, reachedB = False, flagged = False, flaggedA = False, flaggedB = False, num = num h} : t
    | otherwise = h : clickRowA x t (c + 1)

clickA :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
clickA _ _ [] _ = []
clickA x y (h : t) c
    | c == y = clickRowA x h 0 : t                 
    | otherwise = h : clickA x y t (c + 1)

clickRowB :: Int -> [Grid] -> Int -> [Grid]
clickRowB _ [] _ = []
clickRowB x (h : t) c
    | (c == x) && (not (reachedA h)) = Grid {location = location h, mined = mined h, reached = reached h, reachedA = False, reachedB = True, flagged = False, flaggedA = False, flaggedB = False, num = num h} : t
    | otherwise = h : clickRowB x t (c + 1)

clickB :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
clickB _ _ [] _ = []
clickB x y (h : t) c
    | c == y = clickRowB x h 0 : t                 
    | otherwise = h : clickB x y t (c + 1)

countReachedRowA :: [Grid] -> Int
countReachedRowA [] = 0
countReachedRowA (h : t)
    | reachedA h = 1 + countReachedRowA t
    | mined h && flaggedA h = 10 + countReachedRowA t
    | otherwise = countReachedRowA t

countReachedA :: [[Grid]] -> Int -> Int
countReachedA [] _ = 0
countReachedA (h : t) c = countReachedRowA h + countReachedA t (c + 1)

countReachedRowB :: [Grid] -> Int
countReachedRowB [] = 0
countReachedRowB (h : t)
    | reachedB h = 1 + countReachedRowB t
    | mined h && flaggedB h = 10 + countReachedRowB t
    | otherwise = countReachedRowB t

countReachedB :: [[Grid]] -> Int -> Int
countReachedB [] _ = 0
countReachedB (h : t) c = countReachedRowB h + countReachedB t (c + 1)

loseRowCheckA :: [Grid] -> Int -> Int -> Bool
loseRowCheckA [] _ _ = False
loseRowCheckA (h : t) c length
    | c < length = (mined h && reachedA h) || loseRowCheckA t (c + 1) length
    | otherwise = False

loseBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
loseBoardCheck [] _ _ _= False
loseBoardCheck (h : t) c width length
    | c < width = loseRowCheckA h 0 length || loseBoardCheck t (c + 1) width length
    | otherwise = False

loseRowCheckB :: [Grid] -> Int -> Int -> Bool
loseRowCheckB [] _ _ = False
loseRowCheckB (h : t) c length
    | c < length = (mined h && reachedB h) || loseRowCheckB t (c + 1) length
    | otherwise = False

loseBoardCheckB :: [[Grid]] -> Int -> Int -> Int -> Bool
loseBoardCheckB [] _ _ _= False
loseBoardCheckB (h : t) c width length
    | c < width = loseRowCheckB h 0 length || loseBoardCheckB t (c + 1) width length
    | otherwise = False

singleWinRowCheck :: [Grid] -> Int -> Int -> Bool
singleWinRowCheck [] _ _ = True
singleWinRowCheck (h : t) c length
    | c < length = ((mined h && not (reached h)) || (not (mined h) && reached h)) && singleWinRowCheck t (c + 1) length
    | otherwise = True

allMinesOnFlagRow [] = True
allMinesOnFlagRow (h:t)
    | mined h && mynot (flagged h) = False
    | mynot (mined h) && flagged h = False
    | otherwise = True && allMinesOnFlagRow t


allMinesOnFlag [] = True
allMinesOnFlag (h:t) = allMinesOnFlagRow h && allMinesOnFlag t

multiWinRowCheck :: [Grid] -> Int -> Int -> Bool
multiWinRowCheck [] _ _ = True
multiWinRowCheck (h : t) c length
    | c < length = ((mined h && not (reachedA h || reachedB h)) || (not (mined h) && (reachedA h || reachedB h))) && multiWinRowCheck t (c + 1) length
    | otherwise = True

multiWinBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
multiWinBoardCheck [] _ _ _= True
multiWinBoardCheck (h : t) c width length
    | c < width = multiWinRowCheck h 0 length && multiWinBoardCheck t (c + 1) width length
    | otherwise = True

singleWinBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
singleWinBoardCheck [] _ _ _= True
singleWinBoardCheck (h : t) c width length
    | c < width = singleWinRowCheck h 0 length && singleWinBoardCheck t (c + 1) width length
    | otherwise = True

singleLoseRowCheck :: [Grid] -> Int -> Int -> Bool
singleLoseRowCheck [] _ _ = False
singleLoseRowCheck (h : t) c length
    | c < length = (mined h && reached h) || singleLoseRowCheck t (c + 1) length
    | otherwise = False

singleLoseBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
singleLoseBoardCheck [] _ _ _= False
singleLoseBoardCheck (h : t) c width length
    | c < width = singleLoseRowCheck h 0 length || singleLoseBoardCheck t (c + 1) width length
    | otherwise = False

expandUpA row column board
    | row < 0 || mined (findGrid row column board) == True || reachedA (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandUpA (row - 1) (column) board

expandDownA row column board
    | row >= length board || mined (findGrid row column board) == True || reachedA (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandDownA (row + 1) (column) board

expandLeftA row column board
    | column < 0 || mined (findGrid row column board) || reachedA (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandLeftA row (column - 1) board

expandRightA row column board
    | column >= length (board!!0) || mined (findGrid row column board) == True || reachedA (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandRightA row (column + 1) board

expandUDRLA row column board = nub (expandUpA row column board ++ expandDownA row column board ++ expandLeftA row column board ++ expandRightA row column board)

expandUpB row column board
    | row < 0 || mined (findGrid row column board) == True || reachedB (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandUpB (row - 1) (column) board

expandDownB row column board
    | row >= length board || mined (findGrid row column board) == True || reachedB (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandDownB (row + 1) (column) board

expandLeftB row column board
    | column < 0 || mined (findGrid row column board) || reachedB (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandLeftB row (column - 1) board

expandRightB row column board
    | column >= length (board!!0) || mined (findGrid row column board) == True || reachedB (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandRightB row (column + 1) board

expandUDRLB row column board = nub (expandUpB row column board ++ expandDownB row column board ++ expandLeftB row column board ++ expandRightB row column board)

expandUp row column board
    | row < 0 || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandUp (row - 1) (column) board

expandDown row column board
    | row >= length board || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandDown (row + 1) (column) board

expandLeft row column board
    | column < 0 || mined (findGrid row column board) || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandLeft row (column - 1) board

expandRight row column board
    | column >= length (board!!0) || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandRight row (column + 1) board

expandUDRL row column board = nub (expandUp row column board ++ expandDown row column board ++ expandLeft row column board ++ expandRight row column board)

    -- deln n e list function removes first n occurrences of e in list
deln n e [] = []
deln n e (h:t)
    | n == 0 = h:t
    | (n > 0) && (e == h) = deln (n-1) e t
    | otherwise = h : deln n e t


findGrid row column board = board !! row !! column

expandAreaA :: [[Int]] -> [[Int]] -> [[Grid]] -> [[Int]]
expandAreaA _ [] _  = []
expandAreaA lst (h:t) board
    | mined (findGrid (h!!0) (h!!1) board) == True = (h:t)
    | (h:t) == [] = []
    | otherwise = let toExpand = nub (expandA (h:t) board)
                      expanded = nub (h:lst)
                    in h : expandAreaA expanded (toExpand \\ expanded) board

expandA [] _ = []
expandA lst board = nub (foldr (\x y -> y ++ expandUDRLA (x!!0) (x!!1) board) [] lst)

expandAreaB :: [[Int]] -> [[Int]] -> [[Grid]] -> [[Int]]
expandAreaB _ [] _  = []
expandAreaB lst (h:t) board
    | mined (findGrid (h!!0) (h!!1) board) == True = (h:t)
    | (h:t) == [] = []
    | otherwise = let toExpand = nub (expandB (h:t) board)
                      expanded = nub (h:lst)
                    in h : expandAreaB expanded (toExpand \\ expanded) board


expandB [] _ = []
expandB lst board = nub (foldr (\x y -> y ++ expandUDRLB (x!!0) (x!!1) board) [] lst)

expandArea :: [[Int]] -> [[Int]] -> [[Grid]] -> [[Int]]
expandArea _ [] _  = []
expandArea lst (h:t) board
    | mined (findGrid (h!!0) (h!!1) board) == True = (h:t)
    | (h:t) == [] = []
    | otherwise = let toExpand = nub (expand (h:t) board)
                      expanded = nub (h:lst)
                    in h : expandArea expanded (toExpand \\ expanded) board

expand [] _ = []
expand lst board = nub (foldr (\x y -> y ++ expandUDRL (x!!0) (x!!1) board) [] lst)

tupeToList t = firTup t : secTup t : []

expandAreaClickA lst board = foldr (\x y -> clickA (x!!1) (x!!0) y 0) board lst

expandAreaClickB lst board = foldr (\x y -> clickB (x!!1) (x!!0) y 0) board lst

expandAreaClick lst board = foldr (\x y -> click (x!!1) (x!!0) y 0) board lst

-- Get the number of mines around the grid
getGridNum :: Grid -> [[Grid]] -> Int
getGridNum grid board = let (x,y) = location grid
                        in mineToOne (x-1) (y-1) board + mineToOne (x-1) y board + mineToOne (x-1) (y+1) board  +
                        mineToOne x (y-1) board + mineToOne x (y+1) board +
                        mineToOne (x+1) (y-1) board + mineToOne (x+1) y board + mineToOne (x+1) (y+1) board

-- Update the column of a row's grids
updateGridNumColumn [] _ = []
updateGridNumColumn (h : t) board
    = Grid {location = location h, mined = mined h,reached = reached h, reachedA = reachedA h, reachedB = reachedB h, flagged = flagged h, flaggedA = flaggedA h, flaggedB = flaggedB h, num = getGridNum h board} : updateGridNumColumn t board

-- Get a new board with all grids' num updated
updateGridNum [] _ = []
updateGridNum (h:t) board
    = updateGridNumColumn h board : updateGridNum t board

-- Check if the grid with current coordinate is mined, if so return 1.
mineToOne x y board
    | x < 0 || y < 0 || x >= length (board!!0) || y >= length board = 0
    | mined (findGrid x y board) == True = 1
    | otherwise = 0

-- myhead returns the head of the list
myhead (h:t) = h

-- mytail returns the tail of the list
mytail (h:t) = t

-- reverseFixdel returns the string after applying '\DEL' but in reverse order
reverseFixdel [] = []
reverseFixdel lst = foldl (\x y -> if x /= [] && y == '\DEL' then mytail x else y:x) [] lst

-- fixdel get user input and reverse the order of string got from reverseFixdel
fixdel =
    do 
        lst <- getLine 
        return (foldl (\x y -> y : x) [] (reverseFixdel lst) :: String)

getValidInput bot upp =
    do
        input <- fixdel
        if isNum input == False then
            do
                putStrLn "Please enter a valid input"
                getValidInput bot upp
                else 
                    do
                        let num = read input
                        if num >= bot && num <= upp
                            then
                            return num
                            else do
                                putStrLn "Please enter a valid input"
                                getValidInput bot upp

isNum [] = True
isNum (h:t) = isDigit h && isNum t

setFlagRow :: Int -> [Grid] -> Int -> [Grid]
setFlagRow _ [] _ = []
setFlagRow x (h : t) c
    | c == x = Grid {location = location h, mined = mined h, reached = reached h,reachedA = reachedA h, reachedB = reachedB h, flagged = mynot (flagged h), flaggedA = False, flaggedB = False, num = num h} : t
    | otherwise = h : setFlagRow x t (c + 1)

setFlag :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
setFlag _ _ [] _ = []
setFlag x y (h : t) c
    | c == y = setFlagRow x h 0 : t                 
    | otherwise = h : setFlag x y t (c + 1)

setFlagRowA :: Int -> [Grid] -> Int -> [Grid]
setFlagRowA _ [] _ = []
setFlagRowA x (h : t) c
    | c == x && mynot (flaggedB h) = Grid {location = location h, mined = mined h, reached = reached h,reachedA = reachedA h, reachedB = reachedB h, flagged = mynot (flagged h), flaggedA = mynot (flaggedA h), flaggedB = False, num = num h} : t
    | otherwise = h : setFlagRowA x t (c + 1)

setFlagA :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
setFlagA _ _ [] _ = []
setFlagA x y (h : t) c
    | c == y = setFlagRowA x h 0 : t                 
    | otherwise = h : setFlagA x y t (c + 1)

setFlagRowB :: Int -> [Grid] -> Int -> [Grid]
setFlagRowB _ [] _ = []
setFlagRowB x (h : t) c
    | c == x && mynot (flaggedA h) = Grid {location = location h, mined = mined h, reached = reached h,reachedA = reachedA h, reachedB = reachedB h, flagged = mynot (flagged h), flaggedA = flaggedA h, flaggedB = mynot (flaggedB h), num = num h} : t
    | otherwise = h : setFlagRowB x t (c + 1)

setFlagB :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
setFlagB _ _ [] _ = []
setFlagB x y (h : t) c
    | c == y = setFlagRowB x h 0 : t                 
    | otherwise = h : setFlagB x y t (c + 1)

multiGameLoop :: [[Grid]] -> Int -> IO()
multiGameLoop board round
    | allMinesOnFlag board = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateBoard board 0)
        let scoreA = countReachedA board 0
        let scoreB = countReachedB board 0
        let congrats = if scoreA > scoreB
                        then "Player A win!!! \n"
                        else if scoreB > scoreA
                        then "Player B win!!! \n"
                        else "Draw!!! \n"
        putStrLn(show(scoreA))
        putStrLn(show(scoreB))
        putStrLn(congrats)
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | multiWinBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateBoard board 0)
        let scoreA = countReachedA board 0
        let scoreB = countReachedB board 0
        let congrats = if scoreA > scoreB
                        then "Player A win!!! \n"
                        else if scoreB > scoreA
                        then "Player B win!!! \n"
                        else "Draw!!! \n"
        putStrLn("Player A score: " ++ show(scoreA))
        putStrLn("Player B score: " ++ show(scoreB))
        putStrLn(congrats)
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | loseBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateBoard board 0)
        putStrLn("Player A Lose... \n")
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | loseBoardCheckB board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateBoard board 0)
        putStrLn("Player B Lose... \n")
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | otherwise = if round == 1 
                    then
                        do
                            putStrLn(generateColumnCoord board 0)
                            putStrLn(generateBoard_hide board 0)
                            putStrLn("Player A score: " ++ show(countReachedA board 0))
                            putStrLn("Player B score: " ++ show(countReachedB board 0))
                            putStrLn "Player A set/cancel flag? 1.Yes 2.No"
                            flagAnsA <- getValidInput 1 2
                            putStrLn "Player A Enter Row: "
                            row <- getValidInput 0 ((length board) -1)
                            putStrLn "Player A Enter Column : "
                            column <- getValidInput 0 (length (board!!0)-1)     
                            let lst = expandAreaA [] [[row, column]] board              
                            let finalboard = if flagAnsA == 1 
                                                then setFlagA column row board 0
                                                else expandAreaClickA lst board
                            putStrLn("Loading... \n")
                            multiGameLoop (finalboard) 0
                    else
                        do
                            putStrLn(generateColumnCoord board 0)
                            putStrLn(generateBoard_hide board 0)
                            putStrLn("Player A score: " ++ show(countReachedA board 0))
                            putStrLn("Player B score: " ++ show(countReachedB board 0))
                            putStrLn "Player B set/cancel flag? 1.Yes 2.No"
                            flagAnsB <- getValidInput 1 2
                            putStrLn "Player B Enter Row: "
                            row <- getValidInput 0 ((length board) -1)
                            putStrLn "Player B Enter Column : "
                            column <- getValidInput 0 (length (board!!0)-1)     
                            let lst = expandAreaB [] [[row, column]] board              
                            let finalboard = if flagAnsB == 1 
                                                then setFlagB column row board 0
                                                else expandAreaClickB lst board
                            putStrLn("Loading... \n")
                            multiGameLoop (finalboard) 1

singleGameLoop :: [[Grid]] -> String -> String -> UTCTime -> IO()
singleGameLoop board diffString name time 
    | allMinesOnFlag board = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        finishTime <- getCurrentTime
        let (seconds, _) = properFraction (diffUTCTime finishTime time)
        putStrLn ("Clear Time: " ++ show seconds ++ "s")
        putStrLn("You Win!!! \n")
        writeRecords diffString name seconds
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | singleWinBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        finishTime <- getCurrentTime
        let (seconds, _) = properFraction (diffUTCTime finishTime time)
        putStrLn ("Clear Time: " ++ show seconds ++ "s")
        putStrLn("You Win!!! \n")
        writeRecords diffString name seconds
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | singleLoseBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        putStrLn("You Lose... \n")
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else return ()
    | otherwise = do
                    putStrLn(generateColumnCoord board 0)
                    putStrLn(generateSingleBoard_hide board 0)
                    putStrLn "set/cancel flag? 1.Yes 2.No"
                    flagAns <- getValidInput 1 2
                    putStrLn "Enter Row: "
                    row <- getValidInput 0 ((length board) -1)
                    putStrLn "Enter Column : "
                    column <- getValidInput 0 (length (board!!0)-1)     
                    let lst = expandArea [] [[row, column]] board              
                    let finalboard = if flagAns == 1 
                                        then setFlag column row board 0
                                        else expandAreaClick lst board
                    putStrLn("Loading... \n")
                    singleGameLoop finalboard diffString name time

initGame =
    do
        createFileIfNonExist
        choose <- usrRanking
        if choose == 3 then return()
        else do
            putStrLn ("1. Single Player  2. MultiPlayer")
            choose2 <- getValidInput 1 2
            if choose2 == 1 then startSingleGame
            else startMultiGame

usrRanking = 
    do
        putStrLn ("1. Check Ranking 2. Play Game 3. Quit")
        choose <- getValidInput 1 3
        if choose == 1 then do
            generateRanking "SuperEasy"
            generateRanking "Easy"
            generateRanking "Medium"
            generateRanking "Difficult"
            usrRanking
        else if choose == 2 then return 2
        else return 3

startSingleGame = 
    do
        putStrLn "Please enter your name:"
        name <- fixdel
        putStrLn "Please choose difficulty:"
        putStrLn "0. Super Easy 1. Easy   2. Medium   3. Difficult"
        diff <- getValidInput 0 3
        let diffString = if diff == 1
            then "Easy"
            else if diff == 2
                then "Medium"
                else if diff == 3
                    then "Difficult"
                    else "SuperEasy"
        let size = if diff == 1
            then 9
            else if diff == 2
                then 18
                else if diff == 3
                    then 24
                    else 5
        let numMines = if diff == 1
            then 10
            else if diff == 2
                then 40
                else if diff == 3
                    then 99
                    else 2
        mapMinePos <- randomPositions size size numMines
        let genBoard = buildBoard 0 0 size size mapMinePos
        time <- getCurrentTime
        game <- singleGameLoop (updateGridNum genBoard genBoard) diffString name time
        return game

startMultiGame = 
    do
        putStrLn "Please choose difficulty:"
        putStrLn "0. Super Easy 1. Easy   2. Medium   3. Difficult"
        diff <- getValidInput 0 3
        let size = if diff == 1
            then 9
            else if diff == 2
                then 18
                else if diff == 3
                    then 24
                    else 5
        let numMines = if diff == 1
            then 10
            else if diff == 2
                then 40
                else if diff == 3
                    then 99
                    else 2
        mapMinePos <- randomPositions size size numMines
        let genBoard = buildBoard 0 0 size size mapMinePos
        game <- multiGameLoop (updateGridNum genBoard genBoard) 1
        return game

stringLstToRecordLst [] = []
stringLstToRecordLst (h:t) = let rs = words h
                                in (Record (rs!!0) (rs!!1) (read (rs!!2))) : stringLstToRecordLst t

cRank [] _ = []
cRank (h:t) c = (show c) ++ ". " ++ name h ++ findSpaceNum (length (name h)) ++ show (time h) ++ "s\n" ++ cRank t (c+1)

findSpaceNum n 
    | 9 - n > 0 = " " ++ findSpaceNum (n+1)
    | otherwise = []

generateRanking difficulty= 
    do
        content <- readFile (difficulty ++ ".txt")
        let allLines = lines content
        let recordLst = stringLstToRecordLst allLines
        let sortedRecords = sort recordLst
        let title = if difficulty == "SuperEasy"
                    then "Super Easy"
                    else difficulty
        putStrLn ("  " ++ title)
        putStrLn ("----------------")
        putStrLn (cRank sortedRecords 1)

writeRecords difficulty name time =
    do
        let diffString = if difficulty == "Super Easy"
                        then "SuperEasy"
                        else difficulty
        let record = Record diffString name time
        content <- readFile (diffString ++ ".txt")
        let allLines = lines content
        let recordLst = stringLstToRecordLst allLines
        let newLst = record : recordLst
        let sortedRecords = take 5 (sort newLst)
        when (length content >= 0) (writeFile (diffString ++ ".txt") $ recordsToString sortedRecords)

recordsToString [] = []
recordsToString (h:t) = (difficulty h) ++ " "  ++ (name h) ++ " " ++ show (time h) ++ "\n" ++ recordsToString t

createFileIfNonExist = 
    do
        se <- doesFileExist "SuperEasy.txt"
        e <- doesFileExist "Easy.txt"
        m <- doesFileExist "Medium.txt"
        d <- doesFileExist "Difficult.txt"
        when (se == False) (writeFile "SuperEasy.txt" "")
        when (e == False) (writeFile "Easy.txt" "")
        when (m == False) (writeFile "Medium.txt" "")
        when (d == False) (writeFile "Difficult.txt" "")
