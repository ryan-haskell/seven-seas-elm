module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Direction
import Direction exposing (Direction(..))
import Ship exposing (..)

-- MAIN APP
main = App.beginnerProgram
  { update = update
  , view = view
  , model = model
  }


-- MODEL
type alias Model = { ship: Ship }

model: Model
model = Model initShip


-- UPDATE
type Msg = Move Direction | None

update: Msg -> Model -> Model
update msg model =
  case msg of
    Move dir ->
      { model | ship = Ship.move dir model.ship }
    None ->
      model

getRotation: Direction -> String
getRotation dir =
  let
    prefix =
      "rotate("
    suffix =
      "deg)"
    angle =
      case (Direction.getIndex dir) of
        Just index ->
          index*45
        Nothing ->
          0
  in
    prefix ++ (toString angle) ++ suffix


-- VIEW
view: Model -> Html Msg
view model =
  div []
  [ div []
    [ button [onClick (Move NORTH)] [text "NORTH"]
    , button [onClick (Move NORTHEAST)] [text "NORTHEAST"]
    , button [onClick (Move EAST)] [text "EAST"]
    , button [onClick (Move SOUTHEAST)] [text "SOUTHEAST"]
    , button [onClick (Move SOUTH)] [text "SOUTH"]
    , button [onClick (Move SOUTHWEST)] [text "SOUTHWEST"]
    , button [onClick (Move WEST)] [text "WEST"]
    , button [onClick (Move NORTHWEST)] [text "NORTHWEST"]
    ]
  , div [] [text (toString model.ship)]
  , div [] [text (getRotation model.ship.direction)]
  , img
    [ src "img/ship.svg"
    , style [("width", "50px")]
    ] []
  ]
