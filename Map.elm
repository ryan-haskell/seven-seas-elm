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
      initializeTiles size
  in
    Map size level actors tiles


initializePlayer: Int -> Actor
initializePlayer mapSize =
  Actor PLAYER (Location (mapSize // 2) (mapSize // 2)) SOUTH


initializeTiles: Int -> List (List Location)
initializeTiles mapSize =
  let
    tiles =
      List.map
        (\col -> List.map
          (\row -> Location row col)
          [0..mapSize-1])
        [0..mapSize-1]
  in
    tiles


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


movePlayer: Int -> Int -> Map -> Map
movePlayer x y map =
  let
    player =
      getPlayer map
    nonPlayerActors =
      List.filter (\actor -> not(Actor.isPlayer actor)) map.actors
    isAdjacentTile =
      Location.isAdjacentTile (player.location) (Location x y)
    dir =
      Location.getDirection (player.location) (Location x y)
    updatedPlayer =
      Actor.move dir player
    updatedActors =
      nonPlayerActors ++ [updatedPlayer]
  in
    if isAdjacentTile then
      { map | actors = updatedActors }
    else map


-- generateIslands: Int -> List Actor
-- generateIslands size =
