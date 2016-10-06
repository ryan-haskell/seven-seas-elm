module Main exposing (..)

import Html exposing (text)

-- Directions
type Direction
  = NORTH
  | NORTHEAST
  | EAST
  | SOUTHEAST
  | SOUTH
  | SOUTHWEST
  | WEST
  | NORTHWEST

-- Location
type alias Location =
  { x: Float
  , y: Float
  }

move: Direction -> Location -> Location
move dir loc =
  case dir of
    NORTH ->
      { loc | y = loc.y - 1 }
    SOUTH ->
      { loc | y = loc.y + 1 }
    WEST ->
      { loc | x = loc.x - 1 }
    EAST ->
      { loc | x = loc.x + 1 }
    NORTHWEST ->
      { loc | x = loc.x - 1, y = loc.y - 1 }
    SOUTHEAST ->
      { loc | x = loc.x + 1, y = loc.y + 1 }
    NORTHEAST ->
      { loc | x = loc.x + 1, y = loc.y - 1 }
    SOUTHWEST ->
      { loc | x = loc.x - 1, y = loc.y + 1 }

-- Ship
type alias Ship =
  { name: String
  , location: Location
  , direction: Direction
  }

initShip: Ship
initShip =
  { name = "Player"
  , location = { x = 0, y = 0 }
  , direction = NORTH
  }

moveShip: Direction -> Ship -> Ship
moveShip dir ship =
  { ship | location = move dir ship.location, direction = dir }

newShip = (moveShip SOUTHWEST initShip)

main = text (toString newShip)
