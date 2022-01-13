-- This file presents the modelisation of the game
module Game where

import System.Random
-----------------------------------------------------------------
-- Objects definitions                                          |
-----------------------------------------------------------------
-- Real value of the tile is what the tile is when generated.
data RealValue = Mine | Empty
    deriving (Eq, Show)

-- Display value of the tile is what the player sees on the boad.
data DisplayValue = Visible | Invisible | Flag | Unsure
    deriving (Eq, Show)

-- Coordinates to locate a specific tile
type Coordinates = (Int, Int)

-- This leads us to the following type :
-- A tile is made of a location, a real value, a display value.
data Tile = Tile {
    coordinates :: Coordinates,
    realValue ::  RealValue,
    displayValue :: DisplayValue
}
    deriving (Eq, Show)

-- The game board will be composed of a 2-dimensional structure of tiles.
type GameBoard = [[Tile]]

-- Game status is either that the game is in progress, the game is lost (because we clicked on a mine), or the game is won (becasue no empty squared is invisible)
-- See below for implementation of checking.
data GameStatus = InProgress | Lost | Won
    deriving (Eq, Show)

-- The real board is the game board and the status of the game
data Board = Board {
    gameBoard :: GameBoard,
    gameStatus :: GameStatus,
    sizeI :: Int,
    sizeJ :: Int
}

-----------------------------------------------------------------
-- Functions                                                    |
-----------------------------------------------------------------
-- Function to check if a Tile is a Mine.
-- No need to perform the empty check (isTileEmpty = not isTileMine)
isTileMine :: Tile -> Bool
isTileMine Tile{realValue=Mine} = True
isTileMine _ = False

-- Function to check if a Tile is Visible.
isTileVisible :: Tile -> Bool
isTileVisible Tile{displayValue=Visible} = True
isTileVisible _ = False

-- Losing case if the tile is both a mine and visible
isTileLosing :: Tile -> Bool
isTileLosing tile = isTileMine tile && isTileVisible tile

-- This function checks if there is any tile that is both mine and visible in the board.
-- concat is used to get a list instead of a list of lists so that any could be applied.
isGameBoardLost :: GameBoard -> Bool
isGameBoardLost gameBoard = any isTileLosing (concat gameBoard)

-- Function to check if a Tile is Inivisible
isTileInvisible :: Tile -> Bool
isTileInvisible Tile{displayValue=Invisible} = True
isTileInvisible _ = False

-- Function to set the basic case of winning condition (there is no empty invisible tile left on the board)
isTileEmptyAndInvisible :: Tile -> Bool
isTileEmptyAndInvisible tile = not (isTileMine tile) && isTileInvisible tile

-- The gameboard is won if there is no empty-invisible tile in the game.
-- The original syntax was
-- isGameBoardWon gameboard = length (filter isTileEmptyAndInvisible (concat gameBoard)) == 0
-- Which was first converted to 
-- isGameBoardWon gameBoard = null (filter isTileEmptyAndInvisible (concat gameBoard))
-- Which was finally converted to the following.
isGameBoardWon :: GameBoard -> Bool
isGameBoardWon gameBoard = not (any isTileEmptyAndInvisible (concat gameBoard))

-- Checks if any coordinates are between minimal value (0) and maximal value (size) vertically and horizontally.
-- TODO: Variable size grid. 
areCoordinatesInBound :: Int -> Int -> Coordinates -> Bool
areCoordinatesInBound sizeI sizeJ (i,j) = 0 <= i && i < sizeI && 0 <= j && j < sizeJ

-- Gives us a list of potential neighbours, which can possibly be out of range (we will filter with the next function).
potentialNeighbours :: Coordinates -> [Coordinates]
potentialNeighbours (i,j) =
            [
            (i-1, j-1), (i-1, j), (i-1, j+1),
            (i  , j-1),           (i  , j+1),
            (i+1, j-1), (i+1, j), (i+1, j+1)
            ]

-- Filters the list to get only the in-bound neighbours.
actualNeighbours :: Coordinates -> Int -> Int -> [Coordinates]
actualNeighbours (i,j) a b = filter (areCoordinatesInBound a b) (potentialNeighbours (i,j))

-- Gets the tile at said coordinates, if exists, in anyother case, says that nothing exists at said coordinates.
getTileFromCoordinates :: GameBoard -> Coordinates -> Tile
getTileFromCoordinates gameBoard (i,j) = gameBoard !! i !! j

-- Function that gets a list of titles 
getTilesFromCoordinates :: GameBoard -> [Coordinates] -> [Tile]
getTilesFromCoordinates gameBoard = map (getTileFromCoordinates gameBoard)

-- Counts number of True values in a list.
-- fromEnum converts list of Bool to list of Int, and then we can sum over this list.
countTrueValues :: [Bool] -> Int
countTrueValues list = sum (map fromEnum list)

-- Function that counts mines in a list of tiles.
countMines :: [Tile] -> Int
countMines listOfTiles = countTrueValues (map isTileMine listOfTiles)

-----------------------------------------------------------------
-- Board Generation                                             |
-----------------------------------------------------------------
-- To generate the board at the beginning, we first generate an empty row of tiles.
generateRowGameBoard :: Int -> Int -> [Tile]
generateRowGameBoard _ 0 = []
generateRowGameBoard i j = generateRowGameBoard i (j-1) ++ [currentTile]
    where currentTile = Tile {
        displayValue = Invisible, -- in the beginning, the tile has not been played
        realValue = Empty, -- we will put mines afterwards
        coordinates = (i,j-1) -- goes from 0 to size-1
    }

-- To generate the board, we generate each row recursively and concatenate.
generateGameBoard :: Int -> Int -> GameBoard
generateGameBoard 0 _ = []
generateGameBoard i j = generateGameBoard (i-1) j ++ [generateRowGameBoard i j]

-- Function used to generate a list of random coordinates
generateRandomCoordinates :: Int -> Int -> Int -> IO [Coordinates]
generateRandomCoordinates _ _ 0 = return []
generateRandomCoordinates maxHeigth maxWidth coordinatesLeft = do
    randomI <- randomRIO (0, maxHeigth-1)
    randomJ <- randomRIO (0, maxWidth-1)
    recursiveGeneration <- generateRandomCoordinates maxHeigth maxWidth (coordinatesLeft-1)
    return ((randomI, randomJ):recursiveGeneration)

-- We first set a tile in the corresponding row. 
-- Recursively, if we are at the corresponding row we just insert the tile.
-- If we are not, we decrease in a way that we can reach the corresponding row.
-- In any case, we keep track of the whole object !
setTileInRow :: [Tile] -> Tile -> Int -> [Tile]
setTileInRow [] _ _ = []
setTileInRow (tile:otherTiles) newTile 0 = newTile:otherTiles
setTileInRow (tile:otherTiles) newTile j = tile:setTileInRow otherTiles newTile (j-1)

-- Similar in the idea but working on the first coordinate.
setTileInGameBoard :: GameBoard -> Tile -> Coordinates -> GameBoard
setTileInGameBoard [] _ _ = []
setTileInGameBoard gameBoard newTile coordinates =
    case coordinates of
        (0, j) -> setTileInRow (head gameBoard) newTile j:tail gameBoard
        (i, j) -> head gameBoard:setTileInGameBoard (tail gameBoard) newTile (i-1, j)

-- Syntaxic sugar for easier implementation.
replaceOldTileWithNewTile :: GameBoard -> Tile -> Tile -> GameBoard
replaceOldTileWithNewTile gameBoard oldTile newTile = setTileInGameBoard gameBoard newTile (coordinates oldTile)

-- Now we can setup the board by putting mines in it.
setMineAtCoordinates :: GameBoard -> Coordinates -> GameBoard
setMineAtCoordinates gameBoard coordinates = setTileInGameBoard gameBoard newMine coordinates
    where newMine = Tile {coordinates = coordinates, realValue = Mine, displayValue = Invisible}

-- A simple recursivity pattern does the trick here, however Haskell indicates we can use foldl to do better. Here is the initial version :
-- setMinesInGameBoard gameBoard [] = gameBoard
-- setMinesInGameBoard gameBoard (coordinate:coordinates) = setMinesInGameBoard (setMineAtCoordinates gameBoard coordinate) coordinates
-- Here is the new one :
-- setMinesInGameBoard gameBoard coordinates = foldl setMineAtCoordinates gameBoard coordinates
-- And we could also eta-reduce the function, which gives us the following function (to me, it's quite obscure)
setMinesInGameBoard :: GameBoard -> [Coordinates] -> GameBoard
setMinesInGameBoard = foldl setMineAtCoordinates