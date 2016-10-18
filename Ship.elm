module Ship exposing (Ship, initShip, move)

import Direction exposing (..)
import Location exposing (Location)

-- Ship
type alias Ship =
  { name: String
  , location: Location
  , direction: Direction
  }

initShip: Ship
initShip =
  { name = "Player"
  , location = { x = 5, y = 5 }
  , direction = NORTH
  }

move: Direction -> Ship -> Ship
move dir ship =
  { ship
    | location = Location.move dir ship.location
    , direction = dir
  }
