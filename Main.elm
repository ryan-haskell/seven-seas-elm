module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Direction
import Direction exposing (Direction(..))
import Location exposing (Location)
import Map
import Map exposing (Map)


-- MODEL
type alias Model =
  { map: Map }

init : (Model, Cmd Msg)
init = (Model (Map.initializeMap 1), Cmd.none)


-- UPDATE
type Msg
  = Move Direction
  | None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move dir ->
      ({ model | map = Map.movePlayer dir model.map }, Cmd.none)
    None ->
      (model, Cmd.none)


-- VIEW
makeMapTileId: Int -> Int -> String
makeMapTileId x y =
  "tile-"
  ++ (toString x)
  ++ "-"
  ++ (toString y)

viewMapTile: Int -> Int -> Html Msg
viewMapTile x y =
  div [ attribute "x" (toString x)
      , attribute "y" (toString y)
      ] []

viewMapTileRow: List Location -> Html Msg
viewMapTileRow row =
  div [class "map-row"]
  (
    List.map
      (\loc -> viewMapTile loc.x loc.y)
    row
  )

viewMapTiles: List (List Location) -> Html Msg
viewMapTiles listOfRows =
  div [ class "map-tiles" ]
  (
    List.map
      (\row -> viewMapTileRow row)
      listOfRows
  )


view: Model -> Html Msg
view model =
  let
    player =
      Map.getPlayer model.map
  in
    div []
    [ div [] [ text (toString player) ]
    , viewMapTiles model.map.tiles
    , img
      [ src "img/ship.svg"
      , style
        [ ("width", "50px")
        , ("position", "absolute")
        , ("top", (toString (player.location.y * 50)) ++ "px")
        , ("left", (toString (player.location.x * 50)) ++ "px")
        , ("transition", "top .5s, left .5s")
        , ("transform", (Direction.getTransformRotation player.direction))
        ]
      ]
      []
   ]

-- MAIN
main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
