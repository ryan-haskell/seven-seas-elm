module Location exposing (Location, move, isAdjacentTile, getDirection)

import Direction exposing (..)


type alias Location =
  { x: Int
  , y: Int
  }

getAbs: Int -> Int -> Int
getAbs a b =
  if a < b then
    b - a
  else
    a - b

isAdjacentTile: Location -> Location -> Bool
isAdjacentTile loc1 loc2 =
  let
    distX =
      getAbs loc1.x loc2.x
    distY =
      getAbs loc1.y loc2.y
  in
    distX <= 1 && distY <= 1 && not(distX == 0 && distY == 0)

getDirection: Location -> Location -> Direction
getDirection loc1 loc2 =
  let
    x1 = loc1.x
    x2 = loc2.x
    y1 = loc1.y
    y2 = loc2.y
  in
    if y1 > y2 then
      if x1 < x2 then
        NORTHEAST
      else if x1 > x2 then
        NORTHWEST
      else
        NORTH
    else if y1 < y2 then
      if x1 < x2 then
        SOUTHEAST
      else if x1 > x2 then
        SOUTHWEST
      else
        SOUTH
    else if x1 < x2 then
      EAST
    else
      WEST


move: Direction -> Location -> Location
move dir loc =
  case dir of
    NORTH ->
      { loc
        | y = loc.y - 1
      }
    SOUTH ->
      { loc
        | y = loc.y + 1
      }
    WEST ->
      { loc
        | x = loc.x - 1
      }
    EAST ->
      { loc
        | x = loc.x + 1
      }
    NORTHWEST ->
      { loc
        | x = loc.x - 1
        , y = loc.y - 1
      }
    SOUTHEAST ->
      { loc
        | x = loc.x + 1
        , y = loc.y + 1
      }
    NORTHEAST ->
      { loc
        | x = loc.x + 1
        , y = loc.y - 1
      }
    SOUTHWEST ->
      { loc
        | x = loc.x - 1
        , y = loc.y + 1
      }
