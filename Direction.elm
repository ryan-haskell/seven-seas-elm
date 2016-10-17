module Direction exposing(Direction(..), getIndex)

import List.Extra

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


getIndex: Direction -> Maybe Int
getIndex dir =
  let
    directionList =
      [ NORTH
      , NORTHEAST
      , EAST
      , SOUTHEAST
      , SOUTH
      , SOUTHWEST
      , WEST
      , NORTHWEST
      ]
  in
    List.Extra.elemIndex dir directionList
