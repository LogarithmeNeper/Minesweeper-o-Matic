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

-----------------------------------------------------------------
-- Functions                                                    |
-----------------------------------------------------------------
isTileMine :: Tile -> Bool 
isTileMine Tile{realValue=Mine} = True 
isTileMine _ = False 


