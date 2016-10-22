module Direction
    exposing
        ( Direction(..)
        , getAngle
        , getSideDirections
        , rotateClockwise
        )

import Maybe
import List.Extra


type Direction
    = NORTH
    | NORTHEAST
    | EAST
    | SOUTHEAST
    | SOUTH
    | SOUTHWEST
    | WEST
    | NORTHWEST


directionList : List Direction
directionList =
    [ NORTH
    , NORTHEAST
    , EAST
    , SOUTHEAST
    , SOUTH
    , SOUTHWEST
    , WEST
    , NORTHWEST
    ]


getIndex : Direction -> Maybe Int
getIndex dir =
    List.Extra.elemIndex dir directionList


getAngle : Direction -> Int
getAngle dir =
    case (getIndex dir) of
        Just index ->
            index * 45

        Nothing ->
            0


getSideDirections : Direction -> ( Direction, Direction )
getSideDirections dir =
    let
        index =
            Maybe.withDefault 0 (getIndex dir)

        numDirections =
            List.length directionList

        ( leftSideIndex, rightSideIndex ) =
            ( (index + 2) % numDirections
            , (index + 6) % numDirections
            )
    in
        ( Maybe.withDefault NORTH (List.Extra.getAt leftSideIndex directionList)
        , Maybe.withDefault NORTH (List.Extra.getAt rightSideIndex directionList)
        )


rotateClockwise : Direction -> Direction
rotateClockwise dir =
    let
        index =
            Maybe.withDefault 0 (getIndex dir)

        newDirection =
            Maybe.withDefault NORTH (List.Extra.getAt ((index + 1) % (List.length directionList)) directionList)
    in
        newDirection
