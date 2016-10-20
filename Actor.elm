module Actor exposing
  ( ActorType(..)
  , Actor
  , isSubtype
  , isPlayer
  , isWhirlpool
  , isPirate
  , onWhirlpool
  , move
  , rotateClockwise
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

isWhirlpool: Actor -> Bool
isWhirlpool = isSubtype WHIRLPOOL

isPirate: Actor -> Bool
isPirate = isSubtype PIRATE

move: Direction -> Actor -> Actor
move dir actor =
  { actor
    | location = Location.move dir actor.location
    , direction = dir
  }

rotateClockwise: Actor -> Actor
rotateClockwise actor =
  let
    newDirection =
      Direction.rotateClockwise actor.direction
  in
    { actor | direction = newDirection }

onWhirlpool: Actor -> Int -> Bool
onWhirlpool actor mapSize =
  let
    (x, y) =
      (actor.location.x, actor.location.y)
  in
    if x == 0 || x == (mapSize - 1) then
      (y == 0 || y == (mapSize - 1))
    else
      False
