module Map
    exposing
        ( Map
        , GameState(..)
        , initMap
        , movePlayer
        , getPlayer
        , fireCannons
        , rotateWhirlpools
        , movePirates
        , getGameState
        )

import Random
import Debug
import Direction exposing (Direction(..))
import Location exposing (Location)
import Actor
import Actor exposing (Actor, ActorType(..))
import RandomHelper


type GameState
    = PLAYING
    | NEXT_LEVEL
    | GAME_OVER


type alias Map =
    { size : Int
    , level : Int
    , actors : List Actor
    , tiles : List (List Location)
    }



-- Map initialization


initMap : Int -> Int -> Int -> Map
initMap size level seedNum =
    let
        seed =
            Random.initialSeed seedNum

        whirlpools =
            initWhirlpools size

        islands =
            initIslands size seed

        player =
            initPlayer size

        actors =
            whirlpools ++ islands ++ [ player ]

        pirates =
            initPirates size level seed actors

        newActors =
            whirlpools ++ islands ++ pirates ++ [ player ]

        tiles =
            initTiles size
    in
        Map size level newActors tiles


initPlayer : Int -> Actor
initPlayer mapSize =
    Actor PLAYER (Location (mapSize // 2) (mapSize // 2)) SOUTH


initTiles : Int -> List (List Location)
initTiles mapSize =
    let
        tiles =
            List.map
                (\col ->
                    List.map
                        (\row -> Location row col)
                        [0..mapSize - 1]
                )
                [0..mapSize - 1]
    in
        tiles


initWhirlpools : Int -> List Actor
initWhirlpools size =
    let
        maxIndex =
            size - 1

        listListActor =
            List.map
                (\row ->
                    List.map
                        (\col -> Actor WHIRLPOOL (Location (row * maxIndex) (col * maxIndex)) NORTH)
                        [0..1]
                )
                [0..1]
    in
        List.concat listListActor


initIslands : Int -> Random.Seed -> List Actor
initIslands size seed =
    let
        minIslands =
            (size * size * 5 // 100)

        maxIslands =
            (size * size * 15 // 100)

        ( numIslands, seed1 ) =
            Random.step (Random.int minIslands maxIslands) seed

        seeds =
            RandomHelper.makeSeeds seed1 numIslands

        islands =
            List.map (initIsland size) seeds
    in
        List.filter
            (\actor -> not (actor.location.x == size // 2 && actor.location.y == size // 2))
            islands


initIsland : Int -> Random.Seed -> Actor
initIsland size seed =
    let
        ( x, seed1 ) =
            Random.step (Random.int 1 (size - 2)) seed

        ( y, seed2 ) =
            Random.step (Random.int 1 (size - 2)) seed1
    in
        Actor ISLAND (Location x y) NORTH


initPirates : Int -> Int -> Random.Seed -> List Actor -> List Actor
initPirates size level seed actors =
    let
        numPirates =
            2

        -- TODO: Use level and size to determine
        seeds =
            RandomHelper.makeSeeds seed numPirates

        pirates =
            List.map (\seed -> initPirate size level seed actors) seeds
    in
        pirates


initPirate : Int -> Int -> Random.Seed -> List Actor -> Actor
initPirate size level seed actors =
    let
        ( x, seed1 ) =
            Random.step (Random.int 0 (size - 1)) seed

        ( y, seed2 ) =
            Random.step (Random.int 0 (size - 1)) seed1

        loc =
            Location x y
    in
        if hasActor loc actors then
            initPirate size level seed2 actors
        else
            Actor PIRATE loc SOUTH



-- Getting Data


getPlayer : Map -> Actor
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


hasActor : Location -> List Actor -> Bool
hasActor loc actors =
    let
        actorsAtLoc =
            List.filter (\actor -> actor.location == loc) actors
    in
        not (List.isEmpty actorsAtLoc)


getGameState : Map -> GameState
getGameState map =
    let
        player =
            getPlayer map

        pirates =
            List.filter Actor.isPirate map.actors

        collidingPirates =
            List.filter (\actor -> actor.subtype == PIRATE && actor.location == player.location) map.actors
    in
        if not (List.isEmpty collidingPirates) then
            GAME_OVER
        else if (List.isEmpty pirates) then
            NEXT_LEVEL
        else
            PLAYING



-- Map updating


moveActor : Actor -> Int -> Int -> Map -> Map
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
            otherActors ++ [ updatedMovingActor ]
    in
        if isAdjacentTile then
            { map | actors = updatedActors }
        else
            map


movePlayer : Int -> Int -> Map -> Map
movePlayer x y map =
    moveActor (getPlayer map) x y map


movePirates : Int -> Int -> Map -> Map
movePirates playerX playerY map =
    let
        playerLocation =
            Location playerX playerY

        ( players, nonPlayerActors ) =
            List.partition Actor.isPlayer map.actors

        ( pirates, nonShipActors ) =
            List.partition Actor.isPirate nonPlayerActors

        movedPirates =
            List.map (movePirate playerLocation) pirates

        piratesAndWreckages =
            getPiratesAndWreckages nonShipActors movedPirates
    in
        { map | actors = (nonShipActors ++ piratesAndWreckages ++ players) }


movePirate : Location -> Actor -> Actor
movePirate playerLocation pirate =
    let
        newDirection =
            -- Reverse param order if they run away
            Location.getDirection pirate.location playerLocation

        newLocation =
            Location.move newDirection pirate.location
    in
        { pirate
            | location = newLocation
            , direction = newDirection
        }


getPiratesAndWreckages : List Actor -> List Actor -> List Actor
getPiratesAndWreckages otherActors pirates =
    let
        initialActors =
            otherActors ++ pirates

        ( livingPirates, crashedPirates ) =
            List.partition
                (\pirate ->
                    List.isEmpty (List.filter (\actor -> pirate /= actor && pirate.location == actor.location) initialActors)
                )
                pirates

        wreckages =
            List.map (\actor -> Actor WRECKAGE actor.location NORTH) crashedPirates
    in
        livingPirates ++ wreckages


fireCannons : Actor -> Map -> Map
fireCannons actor map =
    let
        cannonDirections =
            Direction.getSideDirections actor.direction
    in
        map


rotateWhirlpools : Map -> Map
rotateWhirlpools map =
    let
        whirlpools =
            List.filter (Actor.isWhirlpool) map.actors

        otherActors =
            List.filter (\actor -> not (Actor.isWhirlpool actor)) map.actors

        rotatedWhirlpools =
            List.map (Actor.rotateClockwise) whirlpools

        updatedActors =
            rotatedWhirlpools ++ otherActors
    in
        { map | actors = updatedActors }
