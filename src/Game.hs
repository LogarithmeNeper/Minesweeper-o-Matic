import qualified Data.Functor as Mine
-- This file presents the modelisation of the game

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
type Coordinates = (Integer, Integer)

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
    gameStatus :: GameStatus
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
