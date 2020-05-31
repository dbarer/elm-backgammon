module Board exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes as Attr
import Random
import Array
import Time
import Debug
import Collage exposing (..)
import Color exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Collage.Events exposing (onClick)
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
  player : Int {- 0 ,1, 2: 0 means no one occupies -}
  }

type alias Board =
  {
  spots : Array.Array Spot
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

type alias Dice =
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

type alias Turn =
  {
    player : Int
  }

type alias Model =
  { board : Board,
    dice : Dice,
    bar : Bar,
    p1 : Player,
    p2 : Player,
    score : Score,
    turn : Turn
    }


type alias Flags =
  ()

initModel ={
   board = {
     spots = Array.fromList ([
       {num_pieces = 2, vulnerable = False, player = 1},
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
       {num_pieces = 2, vulnerable = False, player = 2},
       {num_pieces = 0, vulnerable = False, player = 1},
       {num_pieces = 0, vulnerable = False, player = 2}
       ])
   },
   dice = { roll1 = 3, roll2 = 1, sel_d1 = True, double = False},
   bar = {whites = 0, blacks = 0},
   p1 = {player_num = 1, beared = False, barred = False},
   p2 = {player_num = 2, beared = False, barred = False},
   score = {p1 = 0, p2 = 0, doubled_val = 1, dbl_p1_ctrl = True},
   turn = {player = 1}
  }


init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

-- UPDATE

type Msg
  = Double
  | ClickedOn Int
  | Tick
  | Upd_dice Dice

legal_move : Model -> Int -> Int -> Bool
legal_move m src dst =
  let
    dst_check = 
      case (Array.get dst m.board.spots) of
        Nothing -> False
        Just spot ->
          case compare (spot.num_pieces) 1 of
            GT -> False
            EQ -> True
            LT -> True
    src_check  =
      case (Array.get src m.board.spots) of
        Nothing -> False
        Just spot ->
          case compare (spot.player) m.turn.player of
            EQ -> True
            _ -> False
  in
    dst_check && src_check


update_vul : Int -> Bool
update_vul n =
  case n of
    1 -> True
    _ -> False

update_src : Board -> Int -> Board
update_src b src =
  case (Array.get src b.spots) of
    Nothing -> b
    Just spot ->
      let
        spot1 = {spot | num_pieces = spot.num_pieces - 1}
        spot2 = {spot1 | vulnerable = (update_vul spot1.num_pieces) }
      in
        Board (Array.set src spot2 b.spots)

update_dst : Board -> Bar -> Int -> Int -> (Board , Bar)
update_dst b bar dst pl =
  case (Array.get dst b.spots) of
    Nothing -> (b, bar) 
    Just spot ->
      if(spot.vulnerable == False) then 
        let
          spot1 = {spot | num_pieces = spot.num_pieces + 1}
          spot2 = {spot1 | vulnerable = (update_vul spot1.num_pieces), player = pl}
        in
          (Board (Array.set dst spot2 b.spots) , bar)
      else
        let
          spot1 = {spot | player = pl}
          w = bar.whites
          bl = bar.blacks
          bars =
            case pl of
              1 -> {bar | blacks = bl + 1}
              2 -> {bar | whites = w + 1}
              _ -> bar
          bar_index  =
            case pl of
              1 -> 25
              2 -> 24
              _ -> 0
          bar_spot = case (Array.get bar_index b.spots) of
            Nothing -> spot1
            Just spot2 -> {spot2 | num_pieces = spot2.num_pieces + 1}
        in
          (Board (Array.set bar_index bar_spot (Array.set dst spot1 b.spots)), bars)

select_dice : Dice -> Int
select_dice d =
  case d.sel_d1 of
    True -> d.roll1
    _ -> d.roll2

dice_roll: Random.Generator Dice
dice_roll =
  Random.map2 (\x y -> Dice x y True False)
    (Random.int 1 6)
    (Random.int 1 6)

direction : Int -> Int
direction n =
  case n of
    1 -> 1
    2 -> -1
    _ -> 1

update_board : Model -> Int -> (Board , Bar)
update_board mod n =
  if(legal_move mod n (n + (select_dice mod.dice)) == False) then (mod.board, mod.bar)
  else if (mod.dice.double == True) then Debug.todo "Double"
  else
   update_dst (update_src mod.board n) mod.bar (n + (direction mod.turn.player) * select_dice mod.dice) mod.turn.player

switch_turn : Int -> Int
switch_turn n =
  case n of
    1 -> 2
    2 -> 1
    _ -> 2

dice_val : Dice -> Int
dice_val d =
 case d.sel_d1 of
        True -> d.roll1
        _ -> d.roll2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> 
      (model, Cmd.none)
    Double ->
      (model, Cmd.none)
    Upd_dice d -> ({model | dice = d}, Cmd.none)
    ClickedOn n ->      
      let
        legal = legal_move model n (n + (direction model.turn.player) * (dice_val model.dice))
        tup = update_board model n
      in
        case legal of 
          False -> (model, Cmd.none)
          _ -> ({model | board = Tuple.first tup, bar = Tuple.second tup, turn = Turn (switch_turn model.turn.player)}, Random.generate Upd_dice dice_roll)

-- VIEW
--puts N pieces on the given spot
makeNPieces : Color -> Int -> Int -> List (Collage Msg)
makeNPieces clr spot cnt =
  case cnt of
    0 -> []
    _ -> case compare 6 spot of
      -- Q1
      GT -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (640-((spot-1)*105)), toFloat (380-((cnt-1)*80))) |> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
      EQ -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (640-((spot-1)*105)), toFloat (380-((cnt-1)*80))) |> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
      -- Else
      LT -> case compare 12 spot of
        -- Q2
        GT -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (-115-((spot-7)*105)), toFloat (380-((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
        EQ -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (-115-((spot-7)*105)), toFloat (380-((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
        -- Else
        LT -> case compare 18 spot of
          -- Q3
          GT -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (-640+((spot-13)*105)), toFloat (-380+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
          EQ -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (-640+((spot-13)*105)), toFloat (-380+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
          -- Q4
          LT -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (115+((spot-19)*105)), toFloat (-380+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))

-- iterates over all spots and calls makeNPieces with the proper number and color
spotsToPieces : Int -> List Spot -> List (Collage Msg)
spotsToPieces num spots =
  case spots of
    p::ps ->
      if p.player == 1 then
        case compare 24 num of
          GT -> makeNPieces Color.black num p.num_pieces ++ spotsToPieces (num+1) ps
          EQ -> makeNPieces Color.black num p.num_pieces ++ spotsToPieces (num+1) ps
          LT -> []
      else if p.player == 2 then
        case compare 24 num of
          GT -> makeNPieces Color.blue num p.num_pieces ++ spotsToPieces (num+1) ps
          EQ -> makeNPieces Color.blue num p.num_pieces ++ spotsToPieces (num+1) ps
          LT -> []
      else
        spotsToPieces (num+1) ps
    [] -> []

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
        spotsToPieces 1 (Array.toList model.board.spots)
        --q1
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (640, 220)|> onClick (ClickedOn 0)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (535, 220)|> onClick (ClickedOn 1)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (430, 220)|> onClick (ClickedOn 2)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (325, 220)|> onClick (ClickedOn 3)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (220, 220)|> onClick (ClickedOn 4)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (115, 220)|> onClick (ClickedOn 5)]
        --q2
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-640, 220)|> onClick (ClickedOn 11)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-535, 220)|> onClick (ClickedOn 10)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-430, 220)|> onClick (ClickedOn 9)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-325, 220)|> onClick (ClickedOn 8)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-220, 220)|> onClick (ClickedOn 7)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-115, 220)|> onClick (ClickedOn 6)]
        --q3
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-640, -220)|> onClick (ClickedOn 12)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-535, -220)|> onClick (ClickedOn 13)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-430, -220)|> onClick (ClickedOn 14)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-325, -220)|> onClick (ClickedOn 15)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-220, -220)|> onClick (ClickedOn 16)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-115, -220)|> onClick (ClickedOn 19)]
        --q4
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (640, -220)|> onClick (ClickedOn 23)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (535, -220)|> onClick (ClickedOn 22)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (430, -220)|> onClick (ClickedOn 21)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (325, -220)|> onClick (ClickedOn 20)]
        ++[Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (220, -220)|> onClick (ClickedOn 19)]
        ++[Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (115, -220)|> onClick (ClickedOn 18)]
        --background and bar
        ++[Collage.rectangle 100 880|> styled (uniform brown, solid thick (uniform black))|> onClick (ClickedOn 24)]
        ++[Collage.rectangle 1400 880|> styled (uniform brown, solid thick (uniform black))]

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
subscriptions model = Time.every 50 (\t -> Tick)

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--   Sub.batch
--     [ Browser.Events.onMouseDown
--         (Decode.succeed MouseDown)
--     , Browser.Events.onKeyDown
--         (Decode.map
--           (\key -> if key == "Escape" then EscapeKeyDown else OtherKeyDown)
--           keyDecoder)
--     ]
