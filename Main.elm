module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Random

import Direction
import Direction exposing (Direction(..))
import Location exposing (Location)
import Map
import Map exposing (Map)
import Actor exposing (Actor, ActorType(..))



-- MODEL
type alias Model =
  { map: Map
  , randomSeed: Int
  }

init : (Model, Cmd Msg)
init =
  (  Model (Map.initializeMap 7 1 0) 0
  , (Random.generate SetRandomSeed (Random.int Random.minInt Random.maxInt))
  )


-- UPDATE
type Msg
  = TileClicked Int Int
  | ActorClicked Actor
  | SetRandomSeed Int
  | None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TileClicked x y ->
      ({ model | map = Map.movePlayer x y model.map }, Cmd.none)
    ActorClicked actor ->
      case actor.subtype of
        PLAYER ->
          ({ model | map = (Map.fireCannons actor model.map) }, Cmd.none)
        _ ->
          (model, Cmd.none)
    SetRandomSeed seed ->
      ({ model
        | map = Map.initializeMap 7 1 seed
        , randomSeed = seed }, Cmd.none)
    None ->
      (model, Cmd.none)


-- VIEW
viewMapTile: Int -> Int -> Html Msg
viewMapTile x y =
  div [ class "map-tile"
      , attribute "x" (toString x)
      , attribute "y" (toString y)
      , onClick (TileClicked x y)
      , style
        [ ("flex", "1")
        , ("background-color", "#069")
        ]
      ] []

viewMapTileRow: List Location -> Html Msg
viewMapTileRow row =
  div [ class "map-row"
      , style
        [ ("display", "flex")
        , ("flex", "1")
        ]
      ]
  (
    List.map
      (\loc -> viewMapTile loc.x loc.y)
    row
  )

viewMapTiles: List (List Location) -> Html Msg
viewMapTiles listOfRows =
  div [ class "map-tiles"
      , style
        [ ("width", "100vmin")
        , ("height", "100vmin")
        , ("marginLeft", "calc((100vw - 100vmin)/2)")
        , ("marginTop", "calc((100vh - 100vmin)/2)")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("background-color", "#069")
        ]
      ]
  (
    List.map
      (\row -> viewMapTileRow row)
      listOfRows
  )

getOffset: Int -> Int -> String
getOffset size pos =
  "calc( 100vmin * " ++ toString pos ++ " / " ++ toString size ++ " )"

getTileSize: Int -> String
getTileSize size =
  "calc( 100vmin / " ++ toString size ++ " )"


viewActor: Actor -> Int -> (Html Msg)
viewActor actor mapSize =
  (img
    [ src ("img/" ++ (String.toLower (toString actor.subtype)) ++ ".svg")
    , style
      [ ("width", getTileSize mapSize)
      , ("height", getTileSize mapSize)
      , ("position", "absolute")
      , ("top", getOffset mapSize actor.location.y)
      , ("left", getOffset mapSize actor.location.x)
      , ("transition", "top .5s, left .5s")
      , ("transform", (Direction.getTransformRotation actor.direction))
      ]
    , onClick (ActorClicked actor)
    ]
    []
  )

viewActors: List Actor -> Int -> Int -> List (Html Msg)
viewActors actors mapSize randomInt =
  if randomInt /= 0 then
    List.map
      (\actor -> (viewActor actor mapSize))
    actors
  else []


view: Model -> Html Msg
view model =
  let
    player =
      Map.getPlayer model.map
    mapSize =
      model.map.size
  in
    div [ class "game"
        , attribute "seed" (toString model.randomSeed)
        , style
          [ ("height", "100%")
          , ("overflow", "hidden")
          ]
        ]
    [ viewMapTiles model.map.tiles
    , text (toString (Direction.getSideDirections (player.direction)))
    , div
      [ class "actors"
      , style
        [ ("position", "absolute")
        , ("top", "0")
        , ("left", "0")
        , ("marginLeft", "calc((100vw - 100vmin)/2)")
        , ("marginTop", "calc((100vh - 100vmin)/2)")
        , ("width", "100vmin")
        , ("height", "100vmin")
        , ("visibility", "hidden")
        ]
      ]
      [ div
        [ style [("visibility", "visible")] ]
        (viewActors model.map.actors mapSize model.randomSeed)
      ]
   ]

-- MAIN
main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
