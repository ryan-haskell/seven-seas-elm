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
init = (Model (Map.initializeMap 7 1), Cmd.none)


-- UPDATE
type Msg
  = TileClicked Int Int
  | None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TileClicked x y ->
      ({ model | map = Map.movePlayer x y model.map }, Cmd.none)
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

view: Model -> Html Msg
view model =
  let
    player =
      Map.getPlayer model.map
    mapSize =
      model.map.size
  in
    div [ class "game"
        , style
          [ ("height", "100%")
          ]
        ]
    --[ --div [] []-- text (toString player) ]
    [ viewMapTiles model.map.tiles
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
      [ div [ style [("visibility", "visible")] ]
        [ img
          [ src "img/ship.svg"
          , style
            [ ("width", getTileSize mapSize)
            , ("height", getTileSize mapSize)
            , ("position", "absolute")
            , ("top", getOffset mapSize player.location.y)
            , ("left", getOffset mapSize player.location.x)
            , ("transition", "top .5s, left .5s")
            , ("transform", (Direction.getTransformRotation player.direction))
            ]
          ]
          []
        ]
      ]
   ]

-- MAIN
main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
