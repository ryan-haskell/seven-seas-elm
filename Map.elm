module Map exposing
  ( Map
  , initializeMap
  , movePlayer
  , getPlayer
  , fireCannons
  )

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


-- Map initialization

initializeMap: Int -> Int -> Int -> Map
initializeMap size level seed =
  let
    islands =
      generateIslands size seed
    pirates =
      []
      --generatePirates size level
    player =
      initializePlayer size
    actors =
      islands ++ pirates ++ [player]
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

generateIslands: Int -> Int -> List Actor
generateIslands size seedNum =
  let
    seed = Random.initialSeed seedNum

    minIslands =
      (size * size * 5 // 100)

    maxIslands =
      (size * size * 15 // 100)

    (numIslands, seed1) =
      Random.step (Random.int minIslands maxIslands) (Random.initialSeed seedNum)

    -- Generate a random seed for each island
    seeds =
      makeSeeds seed1 numIslands

    islands =
      List.map (generateIsland size) seeds

  in
    List.filter
      (\actor -> not (actor.location.x == size // 2 && actor.location.y == size // 2))
      islands

makeSeeds: Random.Seed -> Int -> List Random.Seed
makeSeeds seed numSeeds =
  makeSeedsHelper seed numSeeds 0 []

makeSeedsHelper: Random.Seed -> Int -> Int -> List Random.Seed -> List Random.Seed
makeSeedsHelper seed maxIndex index seeds =
  let
    (num, newSeed) =
      Random.step (Random.int Random.minInt Random.maxInt) seed
  in
    if index == maxIndex then
      seeds
    else
      makeSeedsHelper newSeed maxIndex (index+1) (List.append seeds [newSeed])

generateIsland: Int -> Random.Seed -> Actor
generateIsland size seed =
  let
    (x, seed1) =
      Random.step (Random.int 1 (size-2)) seed
    (y, seed2) =
      Random.step (Random.int 1 (size-2)) seed1
  in
    Actor ISLAND (Location x y) NORTH

-- Getting Data
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


-- Map updating

moveActor: Actor -> Int -> Int -> Map -> Map
moveActor movingActor x y map =
  let
    otherActors =
      List.filter (\otherActor -> movingActor /= otherActor) map.actors
    isAdjacentTile =
      Location.isAdjacentTile (movingActor.location) (Location x y)
    dir =
      Location.getDirection (movingActor.location) (Location x y)
    updatedMovingActor =
      Actor.move dir movingActor
    updatedActors =
      otherActors ++ [updatedMovingActor]
  in
    if isAdjacentTile then
      { map | actors = updatedActors }
    else map

movePlayer: Int -> Int -> Map -> Map
movePlayer x y map = moveActor (getPlayer map) x y map

fireCannons: Actor -> Map -> Map
fireCannons actor map =
  let
    cannonDirections =
      Direction.getSideDirections actor.direction
  in
    map
