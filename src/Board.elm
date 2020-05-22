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
import Dice exposing (..)

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

type alias Spot =
  {
  num_pieces : Int,
  vulnerable : Bool,
  player : Int {-0 ,1, 2, 0 means no one occupies -}
  }

type alias Board =
  {
  spots : List Spot
  }

type alias Player =
  {
  player_num : Int,
  beared : Bool,
  barred : Bool
  }

type alias Bar =
  {
  whites : Int,
  blacks : Int
  }

type alias Die =
  {
  roll1 : Int,
  roll2 : Int,
  sel_d1 : Bool,
  double : Bool
  }

set_double : Int -> Int -> Bool
set_double r1 r2 =
  if (r1 == r2) then True
  else False

type alias Move =
  {
  src : Int,
  dst : Int
  }

type alias Score =
  {
  p1 : Int,
  p2 : Int,
  doubled_val : Int,
  dbl_p1_ctrl : Bool
  }

type alias Model =
  { randomNumbers : List Int,
    board : Board,
    dice : Die,
    bar : Bar,
    p1 : Player,
    p2 : Player,
    score : Score
    }

type alias Flags =
  ()

initModel =
  { randomNumbers = [],
   board = {
   spots = [{num_pieces = 2, vulnerable = False, player = 1},
   {num_pieces = 0, vulnerable = False, player = 0}, 
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 5, vulnerable = False, player = 2},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 3, vulnerable = False, player = 2},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 5, vulnerable = False, player = 1},
   {num_pieces = 5, vulnerable = False, player = 2},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 3, vulnerable = False, player = 1},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 5, vulnerable = False, player = 1},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 0, vulnerable = False, player = 0},
   {num_pieces = 1, vulnerable = False, player = 2}]},
   dice = { roll1 = (Dice.roll 1 D6), roll2 = (Dice.roll 1 D6), sel_d1 = True, double = False},
   bar = {whites = 0, blacks = 0},
   p1 = {player_num = 1, beared = False, barred = False},
   p2 = {player_num = 2, beared = False, barred = False},
   score = {p1 = 0, p2 = 0, doubled_val = 1, dbl_p1_ctrl = True}
  }
  

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
        --q1
          [Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (640, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (535, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (430, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (325, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (220, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (115, 200)]
        --q2
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-640, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-535, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-430, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-325, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-220, 200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-115, 200)]
        --q3
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-640, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-535, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-430, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-325, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (-220, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (-115, -200)]
        --q4
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (640, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (535, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (430, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (325, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform white, solid thick (uniform black))|> shift (220, -200)]
        ++[Collage.ellipse 40 150|> styled (uniform red, solid thick (uniform black))|> shift (115, -200)]
        --background and bar
        ++[Collage.rectangle 100 750|> styled (uniform brown, solid thick (uniform black))]
        ++[Collage.rectangle 1400 750|> styled (uniform brown, solid thick (uniform black))]

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
