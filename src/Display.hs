module Display (start) where

-- Currently we import absolutely everything, which is not optimal.
-- TODO
import Game

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.IORef (newIORef, writeIORef, readIORef)

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

-- Game state and game status.
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
invisibleString = "⬛"

-- Homothety for better visualisation.
multiplicativeFactorI :: Int
multiplicativeFactorI = 25

multiplicativeFactorJ :: Int
multiplicativeFactorJ = 25

-- Translation for better visualisation.
translateI :: Int
translateI = 12

translateJ :: Int
translateJ = 12

-- Real-life size 
canvasSizeI :: Int
canvasSizeI = multiplicativeFactorI * sizeI

canvasSizeJ :: Int
canvasSizeJ = multiplicativeFactorJ * sizeJ

-- Grey background.
canvasBackground :: String
canvasBackground = "#c2c2c2"

-- Drawing each cell by drawing at a certain position the current type with priority (invisible -> flagged -> mine -> visible)
drawCells :: Coordinates -> GameBoard -> Int -> Int -> Element -> UI ()
drawCells (_, -1) _ _ _ _ = return ()
drawCells (i, j) gameBoard sizeI sizeJ canvas = do
    let drawAt = (fromIntegral (j*multiplicativeFactorJ+translateJ), fromIntegral (i*multiplicativeFactorI+translateI))
        currentTile = getTileFromCoordinates gameBoard (i,j)
        cellType
          | isTileInvisible currentTile = invisibleString
          | isTileFlagged currentTile = flaggedString
          | isTileMine currentTile = mineString
          | otherwise = show (countMinesInNeighbours currentTile gameBoard sizeI sizeJ)
    canvas # UI.fillText cellType drawAt
    drawCells (i, j-1) gameBoard sizeI sizeJ canvas

-- Drawing a row by drawing each cell.
drawRows :: Int -> GameBoard -> Int -> Int -> Element -> UI ()
drawRows (-1) _ _ _ _ = return ()
drawRows i gameBoard sizeI sizeJ canvas = do
    drawCells (i, sizeJ) gameBoard sizeI sizeJ canvas
    drawRows (i-1) gameBoard sizeI sizeJ canvas

-- Drawing game board by drawing each row.
drawGameBoard :: GameBoard -> Int -> Int -> Element -> UI ()
drawGameBoard gameBoard sizeI sizeJ canvas = do
    canvas # UI.clearCanvas
    canvas # set' UI.textFont "72px"
    drawRows sizeI gameBoard sizeI sizeJ canvas

-- Function used to convert the mouse position to the actual coordinates, and then associate it with the grid.
-- TODO
convertCanvasCoordinatesToTile :: (Double, Double) -> GameBoard -> Tile
convertCanvasCoordinatesToTile (x, y) gameBoard = getTileFromCoordinates gameBoard (i, j)
    where
        i = floor $ y/fromIntegral multiplicativeFactorJ
        j = floor $ x/fromIntegral multiplicativeFactorI

-----------------------------------------------------------------
-- UI Design                                                    |
-----------------------------------------------------------------
start :: IO ()
start = startGUI defaultConfig setup

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

    -- Actions objects
    mousePosition <- liftIO (newIORef (0,0))

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
    on UI.click autoButton $ \_ -> 
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

    -- Actions with canvas and drawableBoard
    -- Idea taken from : https://stackoverflow.com/questions/59635767/threenpenny-gui-capturing-mouse-coordinates-on-click-and-using-them-to-constru
    on UI.mousemove playableBoard $ \(x, y) -> 
        liftIO (writeIORef mousePosition (x,y))

    on UI.mousedown playableBoard $ \_ -> do
        -- Get state information
        currentMousePosition <- liftIO (readIORef mousePosition)
        currentState <- liftIO (readIORef state)
        currentGameBoard <- liftIO (readIORef displayedBoard)
        if hasGameBoardEnded currentGameBoard then return ()
        else do
            case currentState of
                PlayTile -> do
                    let playedTile = convertCanvasCoordinatesToTile currentMousePosition currentGameBoard
                        updatedBoard = playTile currentGameBoard playedTile
                    liftIO (writeIORef displayedBoard updatedBoard)
                    if isGameBoardLost updatedBoard then
                        do
                            drawGameBoard updatedBoard (sizeI-1) (sizeJ-1) playableBoard
                            endOfGameString <- string "You lost."
                            element endOfGameDisplay # set children [endOfGameString]
                            return ()
                    else
                        if isGameBoardWon updatedBoard then
                            do
                                endOfGameString <- string "You won."
                                element endOfGameDisplay # set children [endOfGameString]
                                return ()
                        else
                                drawGameBoard updatedBoard (sizeI-1) (sizeJ-1) playableBoard
                FlagTile -> do
                    let playedTile = convertCanvasCoordinatesToTile currentMousePosition currentGameBoard
                        updatedBoard = flagTile currentGameBoard playedTile
                    liftIO (writeIORef displayedBoard updatedBoard)
                    drawGameBoard updatedBoard (sizeI-1) (sizeJ-1) playableBoard
                RemoveFlagTile -> do
                    let playedTile = convertCanvasCoordinatesToTile currentMousePosition currentGameBoard
                        updatedBoard = removeFlagTile currentGameBoard playedTile
                    liftIO (writeIORef displayedBoard updatedBoard)
                    drawGameBoard updatedBoard (sizeI-1) (sizeJ-1) playableBoard
        return ()

    return ()

