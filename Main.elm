module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Random
import Time exposing (Time)
import AnimationFrame
import Direction
import Direction exposing (Direction(..))
import Location exposing (Location)
import Map
import Map exposing (Map, GameState(..))
import Actor exposing (Actor, ActorType(..))


-- CONSTANTS


animationTime : number
animationTime =
    500



-- MODEL


type alias Model =
    { map : Map
    , randomSeed : Int
    , timeSinceMove : Time
    , enableInput : Bool
    , gameState : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Map.initMap 7 1 0) 0 0 True PLAYING
    , Random.generate SetRandomSeed (Random.int Random.minInt Random.maxInt)
    )



-- UPDATE


type Msg
    = TileClicked Int Int
    | ActorClicked Actor
    | SetRandomSeed Int
    | HandleTimeDiff Time
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClicked x y ->
            let
                playerMovedMap =
                    Map.movePlayer x y model.map

                playerUpdated =
                    (Map.getPlayer model.map /= Map.getPlayer playerMovedMap)

                ( updatedMap, enableInput ) =
                    if playerUpdated then
                        ( Map.movePirates x y playerMovedMap, False )
                    else
                        ( model.map, True )

                updatedMap2 =
                    if model.enableInput then
                        updatedMap
                    else
                        model.map

                gameState =
                    Map.getGameState updatedMap2
            in
                ( { model
                    | map = updatedMap2
                    , gameState = gameState
                    , enableInput = enableInput
                  }
                , Cmd.none
                )

        ActorClicked actor ->
            let
                ( updatedMap, enableInput ) =
                    case actor.subtype of
                        PLAYER ->
                            let
                                ( x, y ) =
                                    ( actor.location.x, actor.location.y )
                            in
                                ( Map.movePirates x y model.map, False )

                        --( Map.fireCannons actor model.map, False )
                        WHIRLPOOL ->
                            ( Map.movePlayer actor.location.x actor.location.y model.map, False )

                        _ ->
                            ( model.map, True )

                updatedMap2 =
                    if model.enableInput then
                        updatedMap
                    else
                        model.map

                gameState =
                    Map.getGameState updatedMap2
            in
                ( { model
                    | map = updatedMap2
                    , gameState = gameState
                    , enableInput = enableInput
                  }
                , Cmd.none
                )

        SetRandomSeed seed ->
            ( { model
                | map = Map.initMap 7 1 seed
                , randomSeed = seed
              }
            , Cmd.none
            )

        HandleTimeDiff time ->
            let
                updatedTime =
                    if model.timeSinceMove + time > animationTime then
                        0
                    else
                        model.timeSinceMove + time

                enableInput =
                    if updatedTime == 0 && model.gameState == PLAYING then
                        True
                    else
                        model.enableInput
            in
                ( { model | timeSinceMove = updatedTime, enableInput = enableInput }, Cmd.none )

        None ->
            ( model, Cmd.none )



-- VIEW


viewMapTile : Int -> Int -> Html Msg
viewMapTile x y =
    div
        [ class "map-tile"
        , attribute "x" (toString x)
        , attribute "y" (toString y)
        , onClick (TileClicked x y)
        , style
            [ ( "flex", "1" )
            , ( "background-color", "#069" )
            ]
        ]
        []


viewMapTileRow : List Location -> Html Msg
viewMapTileRow row =
    div
        [ class "map-row"
        , style
            [ ( "display", "flex" )
            , ( "flex", "1" )
            ]
        ]
        (List.map
            (\loc -> viewMapTile loc.x loc.y)
            row
        )


viewMapTiles : List (List Location) -> Html Msg
viewMapTiles listOfRows =
    div
        [ class "map-tiles"
        , style
            [ ( "width", "100vmin" )
            , ( "height", "100vmin" )
            , ( "marginLeft", "calc((100vw - 100vmin)/2)" )
            , ( "marginTop", "calc((100vh - 100vmin)/2)" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "background-color", "#069" )
            ]
        ]
        (List.map
            (\row -> viewMapTileRow row)
            listOfRows
        )


getOffset : Int -> Int -> String
getOffset size pos =
    "calc( 100vmin * " ++ toString pos ++ " / " ++ toString size ++ " )"


getTileSize : Int -> String
getTileSize size =
    "calc( 100vmin / " ++ toString size ++ " )"


viewActor : Actor -> Int -> Html Msg
viewActor actor mapSize =
    let
        onWhirlpool =
            Actor.onWhirlpool actor mapSize

        timeLabel =
            (toString (animationTime / 1000)) ++ "s"

        transition =
            "top "
                ++ timeLabel
                ++ ", left "
                ++ timeLabel

        actorAngle =
            (Direction.getAngle actor.direction)

        transform =
            "rotate(" ++ (toString actorAngle) ++ "deg)"
    in
        (img
            [ src ("img/" ++ (String.toLower (toString actor.subtype)) ++ ".svg")
            , style
                [ ( "width", getTileSize mapSize )
                , ( "height", getTileSize mapSize )
                , ( "position", "absolute" )
                , ( "top", getOffset mapSize actor.location.y )
                , ( "left", getOffset mapSize actor.location.x )
                , ( "transition", transition )
                , ( "transform", transform )
                ]
            , onClick (ActorClicked actor)
            ]
            []
        )


viewActors : List Actor -> Int -> Int -> List (Html Msg)
viewActors actors mapSize randomInt =
    if randomInt /= 0 then
        List.map
            (\actor -> (viewActor actor mapSize))
            actors
    else
        []


view : Model -> Html Msg
view model =
    let
        player =
            Map.getPlayer model.map

        mapSize =
            model.map.size

        actors =
            Map.getActorsFromRecord model.map.actors
    in
        div
            [ class "game"
            , attribute "seed" (toString model.randomSeed)
            , style
                [ ( "height", "100%" )
                , ( "overflow", "hidden" )
                , ( "cursor", "pointer" )
                ]
            ]
            [ viewMapTiles model.map.tiles
            , div
                [ class "actors"
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    , ( "marginLeft", "calc((100vw - 100vmin)/2)" )
                    , ( "marginTop", "calc((100vh - 100vmin)/2)" )
                    , ( "width", "100vmin" )
                    , ( "height", "100vmin" )
                    , ( "visibility", "hidden" )
                    ]
                ]
                [ div
                    [ style [ ( "visibility", "visible" ) ] ]
                    (viewActors actors mapSize model.randomSeed)
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs HandleTimeDiff



-- MAIN


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
