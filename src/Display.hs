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

data State = Play | Flag | RemoveFlag

-----------------------------------------------------------------
-- UI Design                                                    |
-----------------------------------------------------------------
start :: IO ()
start = do startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do 
    -- Page title setup
    return w # set UI.title "Minesweeper-o-Matic"

    titleWindow <- UI.h1 # set UI.text "Minesweeper-o-Matic"
    getBody w #+ [return titleWindow]

    presentationText <- UI.p # set UI.text "To play this game, just click on the grid below. Your goal is to discover all empty tiles. Avoid mines or you lose !"
    getBody w #+ [return presentationText]
    
    return ()

