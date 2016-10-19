module Actor exposing
  ( ActorType(..)
  , Actor
  , isSubtype
  , isPlayer
  , move
  )

import Direction exposing (..)
import Location exposing (Location)


type ActorType
  = PLAYER
  | PIRATE
  | ISLAND
  | WHIRLPOOL
  | WRECKAGE
  | CANNONBALL

type alias Actor =
  { subtype: ActorType
  , location: Location
  , direction: Direction
  }

isSubtype: ActorType -> Actor -> Bool
isSubtype subtype actor =
  actor.subtype == subtype

isPlayer: Actor -> Bool
isPlayer = isSubtype PLAYER

move: Direction -> Actor -> Actor
move dir actor =
  { actor
    | location = Location.move dir actor.location
    , direction = dir
  }
