module Board exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes as Attr
import Random
import Time
import Debug
import Collage exposing (..)
import Color exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Collage.Text as Text exposing (Text, fromString, size, color, shape)

----------------------------------------------------------------------

-- MAIN

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { randomNumbers : List Int }

type alias Flags =
  ()

initModel =
  { randomNumbers = [] }

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)


-- UPDATE

type Msg
  = MouseDown
  | EscapeKeyDown
  | OtherKeyDown
  | RandomNumber Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EscapeKeyDown ->
      (initModel, Cmd.none)
    OtherKeyDown ->
      (model, Cmd.none)
    MouseDown ->
      (model, Random.generate RandomNumber (Random.int 1 10))
    RandomNumber i ->
      ({ randomNumbers = i :: model.randomNumbers}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    -- https://css-tricks.com/quick-css-trick-how-to-center-an-object-exactly-in-the-center/
    let
      styles =
        [ ("position", "fixed")
        , ("top", "50%")
        , ("left", "50%")
        , ("transform", "translate(-50%, -50%)")
        ]
      canvas =
        [(Collage.square 600 |> outlined (solid thick (uniform red)))]

      display =
        -- Html.text (Debug.toString (model.hitCount, model.missCount))
        svg (stack canvas)

    in
      Html.div (List.map (\(k, v) -> Attr.style k v) styles) [display]


-- SUBSCRIPTIONS
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onMouseDown
        (Decode.succeed MouseDown)
    , Browser.Events.onKeyDown
        (Decode.map
          (\key -> if key == "Escape" then EscapeKeyDown else OtherKeyDown)
          keyDecoder)
    ]
