module Main exposing (..)

import Html exposing (text)

import Direction exposing (..)
import Ship exposing (..)

newShip = (Ship.move SOUTHWEST initShip)

main = text (toString newShip)
