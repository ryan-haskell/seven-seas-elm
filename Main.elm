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


mapSize : Int
mapSize =
    9



-- MODEL


type alias Model =
    { map : Map
    , randomSeed : Int
    , timeSinceMove : Time
    , enableInput : Bool
    , gameState : GameState
    , message : Maybe String
    , whirlpoolAngle : Int
    }


newLevelCmd : Cmd Msg
newLevelCmd =
    Random.generate NewLevel (Random.int Random.minInt Random.maxInt)


newSeedCmd : Cmd Msg
newSeedCmd =
    Random.generate NewSeed (Random.int Random.minInt Random.maxInt)


init : ( Model, Cmd Msg )
init =
    ( Model (Map.initMap mapSize 1 0) 0 0 True Playing Nothing 0
      --(Just "Level 1!")
    , newLevelCmd
    )



-- UPDATE


type Msg
    = TileClicked Int Int
    | ActorClicked Actor
    | NewLevel Int
    | NewSeed Int
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

        NewLevel seedNum ->
            ( { model
                | map = Map.initMap mapSize model.map.level seedNum
                , randomSeed = seedNum
              }
            , Cmd.none
            )

        NewSeed seedNum ->
            ( { model | randomSeed = seedNum }
            , Cmd.none
            )

        HandleTimeDiff time ->
            let
                updatedTime =
                    if model.timeSinceMove + time > animationTime then
                        0
                    else
                        model.timeSinceMove + time

                map =
                    model.map

                level =
                    map.level

                ( updatedModel, updatedCommand ) =
                    if updatedTime == 0 then
                        case model.gameState of
                            Playing ->
                                ( { model | enableInput = True, message = Nothing }, Cmd.none )

                            GameOver ->
                                ( { model | gameState = GameOver2 }, Cmd.none )

                            GameOver2 ->
                                ( { model
                                    | message = (Just "Game Over!")
                                    , map = { map | level = 1 }
                                    , gameState = Loading
                                  }
                                , newLevelCmd
                                )

                            NextLevel ->
                                ( { model | gameState = NextLevel2 }, Cmd.none )

                            NextLevel2 ->
                                ( { model
                                    | message = (Just ("Level " ++ (toString (level + 1)) ++ "!"))
                                    , map = { map | level = level + 1 }
                                    , gameState = Loading
                                  }
                                , newLevelCmd
                                )

                            WhirlpoolSpin ->
                                ( { model | gameState = WhirlpoolLand }
                                , newSeedCmd
                                )

                            WhirlpoolLand ->
                                ( { model
                                    | map = (Map.whirlpoolPlayer map model.randomSeed)
                                    , gameState = Loading
                                  }
                                , Cmd.none
                                )

                            Loading ->
                                ( { model | gameState = Playing }, Cmd.none )
                    else
                        ( model, Cmd.none )
            in
                ( { updatedModel
                    | timeSinceMove = updatedTime
                    , whirlpoolAngle = model.whirlpoolAngle + 5
                  }
                , updatedCommand
                )

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


viewActor : Actor -> Int -> Int -> Html Msg
viewActor actor mapSize whirlpoolAngle =
    let
        onWhirlpool =
            (Actor.onWhirlpool actor mapSize) && (actor.subtype == PLAYER)

        timeLabel =
            (toString (animationTime / 1000)) ++ "s"

        defaultTransition =
            "top "
                ++ timeLabel
                ++ ", left "
                ++ timeLabel

        whirlpoolTransition =
            defaultTransition
                ++ ", transform "
                ++ timeLabel

        actorAngle =
            (Direction.getAngle actor.direction)

        defaultTransform =
            "rotate(" ++ (toString actorAngle) ++ "deg)"

        whirlpoolTransform =
            "rotate(" ++ (toString whirlpoolAngle) ++ "deg)"

        spinningTransform =
            "rotate(" ++ (toString (actorAngle + 360)) ++ "deg)"

        transition =
            if onWhirlpool then
                whirlpoolTransition
            else
                defaultTransition

        transform =
            if onWhirlpool then
                spinningTransform
            else if actor.subtype == WHIRLPOOL then
                whirlpoolTransform
            else
                defaultTransform
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


viewActors : List Actor -> Int -> Int -> Int -> List (Html Msg)
viewActors actors mapSize whirlpoolAngle randomInt =
    if randomInt /= 0 then
        List.map
            (\actor -> (viewActor actor mapSize whirlpoolAngle))
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
                    (viewActors actors mapSize model.whirlpoolAngle model.randomSeed)
                ]
            , h3
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    , ( "margin", "0" )
                    , ( "cursor", "default" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "justify-content", "center" )
                    , ( "align-items", "center" )
                    , ( "background-color", "#069" )
                    , ( "color", "white" )
                    , ( "font-family", "Arial" )
                    , ( "display"
                      , (case model.message of
                            Just str ->
                                "flex"

                            Nothing ->
                                "none"
                        )
                      )
                    ]
                ]
                [ text (Maybe.withDefault "" model.message) ]
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
