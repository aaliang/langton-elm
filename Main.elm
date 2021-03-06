--import Text exposing (..)
--import Color exposing (..)
--import Graphics.Element exposing (..)
--import Graphics.Collage exposing (..)

--diamond color size =
--    square size |> filled color |> rotate (degrees 45)

--main = 
--    collage 200 200 [ diamond blue 100, diamond red 75]

import Graphics.Collage exposing (square, collage, filled)
import Color exposing (blue, red)
main =
  collage 200 200 [
    square 100 |> filled blue,
    square 100 |> filled red]
