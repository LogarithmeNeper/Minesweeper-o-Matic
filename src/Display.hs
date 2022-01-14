module Display (start) where

-- Currently we import absolutely everything, which is not optimal.
-- TODO
import Game

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.IORef (newIORef, writeIORef)

-----------------------------------------------------------------
-- Global Variables                                             |
-----------------------------------------------------------------

-- Game Difficulty
-- 0 <= difficulty <= 1
difficulty :: Double
difficulty = 0.2

-- Size of GameBoard (vertically)
sizeI :: Int
sizeI = 15

-- Size of GameBoard (horizontally)
sizeJ :: Int
sizeJ = 25

-- Number of Mines in the Grid.
numberOfMines :: Int
numberOfMines = (floor . (*difficulty) . fromIntegral) sizeI * sizeJ

data State = PlayTile | FlagTile | RemoveFlagTile
data GameStatus = InProgress | Won | Lost

-----------------------------------------------------------------
-- GameBoard display                                            |
-----------------------------------------------------------------
mineString :: String
mineString = "💣"

flaggedString :: String
flaggedString = "🚩"

invisibleString :: String
invisibleString = "X"

multiplicativeFactorI :: Int
multiplicativeFactorI = 25

multiplicativeFactorJ :: Int
multiplicativeFactorJ = 25

canvasSizeI :: Int
canvasSizeI = multiplicativeFactorI * sizeI

canvasSizeJ :: Int
canvasSizeJ = multiplicativeFactorJ * sizeJ

canvasBackground :: String
canvasBackground = "#c2c2c2"

drawCells :: Coordinates -> GameBoard -> Int -> Int -> Element -> UI ()
drawCells (_, 0) _ _ _ _ = return ()
drawCells (i, j) gameBoard sizeI sizeJ canvas = do
    let drawAt = (fromIntegral (j*multiplicativeFactorJ), fromIntegral (i*multiplicativeFactorI))
        currentTile = getTileFromCoordinates gameBoard (i,j)
        {--
        cellType =
            -- If Tile invisible, then we show the empty string
            if isTileInvisible currentTile then invisibleString
            -- In any othercase, we show the display value of the object
            else
                -- If mine discovered then show mine
                if isTileMine currentTile then mineString
                else
                    -- If flagged tile then show flag
                    if isTileFlagged currentTile then flagString
                    else
                        -- last case is : not a mine, not flagged, just show how many neighours are around.
                        show (countMinesInNeighbours currentTile gameBoard sizeI sizeJ)
        --}
        cellType
          | isTileInvisible currentTile = invisibleString
          | isTileMine currentTile = mineString
          | isTileFlagged currentTile = flaggedString
          | otherwise = show (countMinesInNeighbours currentTile gameBoard sizeI sizeJ)
    canvas # UI.fillText cellType drawAt
    drawCells (i, j-1) gameBoard sizeI sizeJ canvas

drawRows :: Int -> GameBoard -> Int -> Int -> Element -> UI ()
drawRows 0 _ _ _ _ = return ()
drawRows i gameBoard sizeI sizeJ canvas = do
    drawCells (i, sizeJ) gameBoard sizeI sizeJ canvas
    drawRows (i-1) gameBoard sizeI sizeJ canvas

drawGameBoard :: GameBoard -> Int -> Int -> Element -> UI ()
drawGameBoard gameBoard sizeI sizeJ canvas = do
    canvas # UI.clearCanvas
    drawRows sizeI gameBoard sizeI sizeJ canvas


-----------------------------------------------------------------
-- UI Design                                                    |
-----------------------------------------------------------------
start :: IO ()
start = do startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    -- Page title setup
    return w # set UI.title "Minesweeper-o-Matic"

    -- Window title as a h1 element in HTML
    titleWindow <- UI.h1 # set UI.text "Minesweeper-o-Matic 💣🚩"
    getBody w #+ [return titleWindow]

    -- Simple presentation text so that the user knows what he should do
    presentationText <- UI.p # set UI.text "To play this game, just click on the grid below. Your goal is to discover all empty tiles. Avoid mines or you lose !"
    getBody w #+ [return presentationText]

    -- State helper for user
    stateDisplayString <- string "Play"
    stateDisplay <- UI.div
    element stateDisplay # set children [stateDisplayString]
    getBody w #+ [return stateDisplay]

    -- Game state helper for user
    gameStateDisplayString <- string "In Progress"
    gameStateDisplay <- UI.div
    element gameStateDisplay # set children [gameStateDisplayString]
    getBody w #+ [return gameStateDisplay]

    -- Board objects
    state <- liftIO (newIORef PlayTile)
    gameStatus <- liftIO (newIORef InProgress)

    generatedBoard <- liftIO (generateGameBoard sizeI sizeJ numberOfMines)
    displayedBoard <- liftIO (newIORef generatedBoard)

    -- Playable board
    playableBoard <- UI.canvas
        # set UI.height canvasSizeI
        # set UI.width canvasSizeJ
        # set UI.style [("background", canvasBackground)]
    getBody w #+ [return playableBoard]

    -- Buttons.
    playButton <- UI.button # set UI.text "play"
    flagButton <- UI.button # set UI.text "flag"
    removeFlagButton <- UI.button # set UI.text "remove"
    autoButton <- UI.button # set UI.text "auto"

    newGameButton <- UI.button # set UI.text "new game"
    getBody w #+ [return playButton, return flagButton, return removeFlagButton, return autoButton, return newGameButton]

    -- End of game user display
    endOfGameString <- string ""
    endOfGameDisplay <- UI.div
    element endOfGameDisplay # set children [endOfGameString]
    getBody w #+ [return endOfGameDisplay]

    -- Actions with buttons
    on UI.click playButton $ \_ -> do
        liftIO (writeIORef state PlayTile)
        stateDisplayString <- string "play"
        element stateDisplay # set children [stateDisplayString]
        return ()
    on UI.click flagButton $ \_ -> do
        liftIO (writeIORef state FlagTile)
        stateDisplayString <- string "flag"
        element stateDisplay # set children [stateDisplayString]
        return ()
    on UI.click removeFlagButton $ \_ -> do
        liftIO (writeIORef state RemoveFlagTile)
        stateDisplayString <- string "remove"
        element stateDisplay # set children [stateDisplayString]
        return ()
    on UI.click autoButton $ \_ -> do
        -- meh time
        return ()
    on UI.click newGameButton $ \_ -> do
        -- Generate new board and display it.
        liftIO (writeIORef state PlayTile)
        liftIO (writeIORef gameStatus InProgress)
        newBoard <- liftIO (generateGameBoard sizeI sizeJ numberOfMines)
        liftIO (writeIORef displayedBoard newBoard)
        stateDisplayString <- string "play"
        element stateDisplay # set children [stateDisplayString]
        endOfGameString <- string ""
        element endOfGameDisplay # set children [endOfGameString]
        drawGameBoard newBoard (sizeI-1) (sizeJ-1) playableBoard    
        return ()

    return ()

