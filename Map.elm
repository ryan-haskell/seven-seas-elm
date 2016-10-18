module Map exposing (Map, initializeMap, movePlayer, getPlayer)

import Random
import Debug

import Direction exposing (Direction(..))
import Location exposing (Location)
import Actor
import Actor exposing (Actor, ActorType(..))


type alias Map =
  { size: Int
  , level: Int
  , actors: List Actor
  , tiles: List (List Location)
  }


initializeMap: Int -> Map
initializeMap level =
  let
    size =
      9
    --islands =
    --  generateIslands size
    --pirates =
    --  generatePirates size level
    player =
      initializePlayer size
    actors =
      [player]
    --actors =
    --  islands ++ pirates ++ player
    tiles =
      [[]]
  in
    Map size level actors tiles


initializePlayer: Int -> Actor
initializePlayer mapSize =
  Actor PLAYER (Location (mapSize // 2) (mapSize // 2)) SOUTH


getPlayer: Map -> Actor
getPlayer map =
  let

    actors =
      map.actors

    players =
      List.filter
        Actor.isPlayer
        actors

    player =
      case List.head players of
        Nothing ->
          Debug.crash ("No player found")
        Just actor ->
          actor

  in
    player


movePlayer: Direction -> Map -> Map
movePlayer dir map =
  let

    player =
      getPlayer map

    nonPlayerActors =
      List.filter (\actor -> not(Actor.isPlayer actor)) map.actors

    updatedPlayer =
      Actor.move dir player

    updatedActors =
      nonPlayerActors ++ [updatedPlayer]

  in
    { map | actors = updatedActors }


-- generateIslands: Int -> List Actor
-- generateIslands size =
