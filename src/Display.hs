module Display (start) where

-- Currently we import absolutely everything, which is not optimal.
-- TODO
import Game

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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

data State = Play | Flag | Remove

-----------------------------------------------------------------
-- GameBoard display                                            |
-----------------------------------------------------------------
mineString :: String
mineString = "💣"

flaggedString :: String
flaggedString = "🚩"

emptyString :: String
emptyString = "⬛"

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
    stateDisplay <- UI.p # set UI.text "Play"
    getBody w #+ [return stateDisplay]
    
    -- Buttons.
    playButton <- UI.button # set UI.text "play"
    flagButton <- UI.button # set UI.text "flag"
    removeFlagButton <- UI.button # set UI.text "remove"
    autoButton <- UI.button # set UI.text "auto"

    newGameButton <- UI.button # set UI.text "new game"
    getBody w #+ [return playButton, return flagButton, return removeFlagButton, return autoButton, return newGameButton]
    
    -- Actions with buttons
    on UI.click playButton return
    on UI.click flagButton return
    on UI.click removeFlagButton return
    on UI.click autoButton return
    on UI.click newGameButton return

    -- 
    return ()

