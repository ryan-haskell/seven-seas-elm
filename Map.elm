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
        , getActorsFromRecord
        )

import Random
import Direction exposing (Direction(..))
import Location exposing (Location)
import Actor
import Actor exposing (Actor, ActorType(..))
import RandomHelper
import List.Extra


type GameState
    = Playing
    | NextLevel
    | NextLevel2
    | GameOver
    | GameOver2
    | Loading


type alias ActorRecord =
    { islands : List Actor
    , whirlpools : List Actor
    , pirates : List Actor
    , wreckages : List Actor
    , player : Actor
    }


type alias Map =
    { size : Int
    , level : Int
    , actors : ActorRecord
    , tiles : List (List Location)
    }



-- Map initialization


initMap : Int -> Int -> Int -> Map
initMap size level seedNum =
    let
        seed =
            Random.initialSeed seedNum

        islands =
            initIslands size seed

        whirlpools =
            initWhirlpools size

        player =
            initPlayer size

        actors =
            whirlpools ++ islands ++ [ player ]

        pirates =
            initPirates size level seed actors

        wreckages =
            []

        actorRecord =
            ActorRecord islands whirlpools pirates wreckages player

        tiles =
            initTiles size
    in
        Map size level actorRecord tiles


getActorsFromRecord : ActorRecord -> List Actor
getActorsFromRecord record =
    let
        actorTuple =
            ( record.islands
            , record.whirlpools
            , record.pirates
            , record.wreckages
            , record.player
            )
    in
        getActors actorTuple


getActors : ( List Actor, List Actor, List Actor, List Actor, Actor ) -> List Actor
getActors ( islands, whirlpools, pirates, wreckages, player ) =
    let
        piratesAndWreckages =
            pirates ++ wreckages

        sortedPiratesAndWreckages =
            (List.sortBy .id piratesAndWreckages)
    in
        islands ++ whirlpools ++ sortedPiratesAndWreckages ++ [ player ]


initPlayer : Int -> Actor
initPlayer mapSize =
    Actor PLAYER (Location (mapSize // 2) (mapSize // 2)) SOUTH 0


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
                        (\col -> Actor WHIRLPOOL (Location (row * maxIndex) (col * maxIndex)) NORTH 0)
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
        Actor ISLAND (Location x y) NORTH 0


initPirates : Int -> Int -> Random.Seed -> List Actor -> List Actor
initPirates size level seed actors =
    let
        numPirates =
            level + 1

        -- TODO: Use level and size to determine
        seedTuples =
            RandomHelper.makeSeedTuples seed numPirates

        pirates =
            List.map (\( id, seed ) -> initPirate size level id seed actors) seedTuples

        uniquePirates =
            List.Extra.uniqueBy (\pirate -> ( pirate.location.x, pirate.location.y )) pirates
    in
        uniquePirates


initPirate : Int -> Int -> Int -> Random.Seed -> List Actor -> Actor
initPirate size level id seed actors =
    let
        ( x, seed1 ) =
            Random.step (Random.int 0 (size - 1)) seed

        ( y, seed2 ) =
            Random.step (Random.int 0 (size - 1)) seed1

        loc =
            Location x y
    in
        if hasActor loc actors then
            initPirate size level id seed2 actors
        else
            Actor PIRATE loc SOUTH id



-- Getting Data


getPlayer : Map -> Actor
getPlayer map =
    let
        player =
            map.actors.player
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
            map.actors.pirates

        wreckages =
            map.actors.wreckages

        collidingPirates =
            List.filter (\actor -> actor.location == player.location) wreckages
    in
        if not (List.isEmpty collidingPirates) then
            GameOver
        else if (List.isEmpty pirates) then
            NextLevel
        else
            Playing



-- Map updating


moveActor : Actor -> Int -> Int -> Map -> Map
moveActor movingActor x y map =
    let
        isAdjacentTile =
            Location.isAdjacentTile (movingActor.location) (Location x y)

        dir =
            Location.getDirection (movingActor.location) (Location x y)

        updatedMovingActor =
            Actor.move dir movingActor

        actors =
            map.actors

        updatedActors =
            case movingActor.subtype of
                PLAYER ->
                    { actors | player = updatedMovingActor }

                PIRATE ->
                    let
                        otherPirates =
                            List.filter (\pirate -> pirate.id /= movingActor.id) actors.pirates
                    in
                        { actors | pirates = otherPirates ++ [ updatedMovingActor ] }

                _ ->
                    actors
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

        actors =
            map.actors

        pirates =
            map.actors.pirates

        movedPirates =
            List.map (movePirate playerLocation) pirates

        actorsAfterMovedPirates =
            { actors | pirates = movedPirates }

        ( livingPirates, newWreckages ) =
            getPiratesAndWreckages actorsAfterMovedPirates

        wreckages =
            actors.wreckages ++ newWreckages
    in
        { map
            | actors =
                { actors
                    | pirates = livingPirates
                    , wreckages = wreckages
                }
        }


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


getPiratesAndWreckages : ActorRecord -> ( List Actor, List Actor )
getPiratesAndWreckages actorRecord =
    let
        pirates =
            actorRecord.pirates

        actorList =
            getActorsFromRecord actorRecord

        ( livingPirates, crashedPirates ) =
            List.partition
                (\pirate ->
                    List.isEmpty (List.filter (\actor -> pirate /= actor && pirate.location == actor.location) actorList)
                )
                pirates

        wreckages =
            List.map (\actor -> Actor WRECKAGE actor.location NORTH actor.id) crashedPirates
    in
        ( livingPirates, wreckages )


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
            map.actors.whirlpools

        rotatedWhirlpools =
            List.map (Actor.rotateClockwise) whirlpools

        actors =
            map.actors
    in
        { map | actors = { actors | whirlpools = rotatedWhirlpools } }
