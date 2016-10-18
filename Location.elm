module Location exposing (Location, move)

import Direction exposing (..)


type alias Location =
  { x: Int
  , y: Int
  }


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
