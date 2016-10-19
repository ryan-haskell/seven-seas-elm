module Direction exposing
  ( Direction(..)
  , getIndex
  , getTransformRotation
  , getSideDirections
  , rotateClockwise
  )

import Debug
import Maybe

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

getIndex: Direction -> Maybe Int
getIndex dir =
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
          --Debug.log "Direction missing from list: " dir
          0
  in
    prefix ++ (toString angle) ++ suffix

getSideDirections: Direction -> (Direction, Direction)
getSideDirections dir =
  let
    index =
      case getIndex dir of
        Just num ->
          num
        Nothing ->
          0
    numDirections =
      List.length directionList
    (leftSideIndex, rightSideIndex) =
      ( (index + 2) % numDirections, (index + 6) % numDirections )
  in
    ( Maybe.withDefault NORTH (List.Extra.getAt leftSideIndex directionList)
    , Maybe.withDefault NORTH (List.Extra.getAt rightSideIndex directionList)
    )

rotateClockwise: Direction -> Direction
rotateClockwise dir =
  let
    index =
      Maybe.withDefault 0 (getIndex dir)
    newDirection =
      Maybe.withDefault NORTH (List.Extra.getAt ( (index + 1) % (List.length directionList) ) directionList)
  in
    newDirection
