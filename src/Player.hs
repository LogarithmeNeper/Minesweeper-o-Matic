module Player where

import Game

-- Function that gets all tiles around one specific tile.
getNeighboursTile :: GameBoard -> Int -> Int -> Tile -> [Tile]
getNeighboursTile gameBoard maxI maxJ tile = getTilesFromCoordinates gameBoard (actualNeighbours (coordinates tile) maxI maxJ)

-- Function to get all invisible tiles around one specific tile.
getInvisibleNeighboursTile :: GameBoard -> Int -> Int -> Tile -> [Tile]
getInvisibleNeighboursTile gameBoard maxI maxJ tile = filter isTileInvisible neighbours where neighbours = getNeighboursTile gameBoard maxI maxJ tile

-- Pattern 1 : number of invisible tiles around one tile is exactly the number of mines around the said tile.
-- The function returns a list of tiles to be ***flagged***
patternFlag :: GameBoard -> Int -> Int -> Tile -> [Tile]
patternFlag gameBoard maxI maxJ tile = let invisibleNeighbours = getInvisibleNeighboursTile gameBoard maxI maxJ tile in
    if countMinesInNeighbours tile gameBoard maxI maxJ == length invisibleNeighbours
    then invisibleNeighbours
    else []

-- Finding pattern in a row
patternFlagRow :: GameBoard -> Int -> Int -> [Tile] -> [Tile]
patternFlagRow _ _ _ [] = []
patternFlagRow gameBoard maxI maxJ (hd:tl) = patternFlag gameBoard maxI maxJ hd ++ patternFlagRow gameBoard maxI maxJ tl

-- Finding pattern in the grid.
patternFlagBoard :: GameBoard -> Int -> Int -> [Tile]
patternFlagBoard [] _ _ = []
patternFlagBoard (hd:tl) maxI maxJ = patternFlagRow (hd:tl) maxI maxJ hd ++ patternFlagBoard tl maxI maxJ

-- Gets one move to do
getMoveFlag :: GameBoard -> Int -> Int -> Maybe Tile
getMoveFlag gameBoard maxI maxJ = case patternFlagBoard gameBoard maxI maxJ of
    [] -> Nothing
    _ -> Just (head $ patternFlagBoard gameBoard maxI maxJ)

-- What is left is to link that with the actual button.