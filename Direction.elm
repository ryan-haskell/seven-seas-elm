module Direction exposing(Direction(..), getIndex, getTransformRotation)

import List.Extra


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


getTransformRotation: Direction -> String
getTransformRotation dir =
  let
    prefix =
      "rotate("
    suffix =
      "deg)"
    angle =
      case (getIndex dir) of
        Just index ->
          index*45
        Nothing ->
          0
  in
    prefix ++ (toString angle) ++ suffix
