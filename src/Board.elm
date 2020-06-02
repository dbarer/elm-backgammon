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
  dbl_p1_ctrl : Int
  }

type alias Turn =
  {
    player : Int
  }

type alias Model =
  { board : Board,
    dice : Dice,
    bar : Bar,
    players : List Player,
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
       {num_pieces = 0, vulnerable = False, player = 1}, --[24], 25
       {num_pieces = 0, vulnerable = False, player = 2},  --[25], 26
       {num_pieces = 0, vulnerable = False, player = 1}, -- [26], 27  Score p1 
       {num_pieces = 0, vulnerable = False, player = 2} --[27], 28 Score p2
       ])
   },
   dice = { roll1 = 3, roll2 = 1, sel_d1 = True, double = False},
   bar = {whites = 0, blacks = 0},
   players = [{player_num = 1, beared = False, barred = False}, {player_num = 2, beared = False, barred = False}],
   score = {p1 = 0, p2 = 0, doubled_val = 1, dbl_p1_ctrl = 0},
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
            GT ->
              let
                p = m.turn.player
              in      
                case compare p spot.player of
                  EQ -> True
                  _ -> False
            _ -> True
    
    src_check  =
      case (Array.get src m.board.spots) of
        Nothing -> False
        Just spot ->
          case compare (spot.player) m.turn.player of
            EQ -> True
            _ ->  False
  in
    dst_check && src_check




beared_get : Int ->  Array.Array Spot -> Int  -> Int
beared_get n xs pl =
  case Array.get n xs of
    Nothing -> 0
    Just spot ->
      case compare pl spot.player of
        EQ -> spot.num_pieces
        _ -> 0


{-
barred_chk : Model -> Int -> Bool
barred_chk  m pl =
  case pl of
    1 ->
      case (Array.get 24 m.board.spots).num_pieces of
        0 -> False
        _ -> True
    2 ->
      case (Array.get 25 m.board.spots).num_pieces of
        0 -> False
        _ -> True


-}
beared : Model -> Int -> Bool
beared m pl =
  case pl of
    1 ->
      let
        s1 = (beared_get 18 m.board.spots 1)
        s2 = (beared_get 19 m.board.spots 1)
        s3 = (beared_get 20 m.board.spots 1)
        s4 = (beared_get 21 m.board.spots 1)
        s5 = (beared_get 22 m.board.spots 1)
        s6 = (beared_get 23 m.board.spots 1)
        s7 = m.score.p1
        tot = s1 + s2 + s3 + s4 + s5 + s6 + s7
      in
        case tot of
          18 -> True
          _ -> False
    2 ->
      let
        s1 = (beared_get 1 m.board.spots 2)
        s2 = (beared_get 2 m.board.spots 2)
        s3 = (beared_get 3 m.board.spots 2)
        s4 = (beared_get 4 m.board.spots 2)
        s5 = (beared_get 5 m.board.spots 2)
        s6 = (beared_get 6 m.board.spots 2)
        s7 = m.score.p2
        tot = s1 + s2 + s3 + s4 + s5 + s6 + s7
      in
        case tot of
          18 -> True
          _ -> False
    _ -> False

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

update_dst : Board ->  Int -> Int -> Board
update_dst b dst pl =
  case (Array.get dst b.spots) of
    Nothing -> b
    Just spot ->
      if(spot.vulnerable == True && spot.player /= pl ) then
        let
          spot1 = {spot | player = pl}
          bar_index  =
            case pl of
              1 -> 25
              2 -> 24
              _ -> 0
          bar_spot = case (Array.get bar_index b.spots) of
            Nothing -> spot1
            Just spot2 -> {spot2 | num_pieces = spot2.num_pieces + 1}
        in
          (Board (Array.set bar_index bar_spot (Array.set dst spot1 b.spots)))
      else
        let
          spot1 = {spot | num_pieces = spot.num_pieces + 1}
          spot2 = {spot1 | vulnerable = (update_vul spot1.num_pieces), player = pl}
        in
          (Board (Array.set dst spot2 b.spots))
{-
barred_move : Board -> Int -> Int -> Board
barred_move b n pl
  case pl of
    1 ->
      let
        upd_src = Array.get 24 b.spots
        new_src = {upd_src | num_pieces = upd_src.num_pieces - 1}
        upd_dst = Array.get n b.spots
        new_dst = {upd_dst | num_pieces = upd_dst.num_pieces + 1, player = p}l
    2 -> 
    _ -> b
-}
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

update_board : Model -> Int -> Board
update_board mod n =
  if (n + (direction mod.turn.player * (select_dice mod.dice)) < 0 ) then
    update_dst (update_src mod.board n) 27 mod.turn.player
  else if (n + (direction mod.turn.player * (select_dice mod.dice)) > 23) then
    update_dst (update_src mod.board n) 26 mod.turn.player
  else
    let
      legal = 
        case n of
          24 -> legal_move mod n ((select_dice mod.dice) - 1)
          25 -> legal_move mod n (n-1 + (direction mod.turn.player) * select_dice mod.dice)
          _ -> legal_move mod n (n + ((direction mod.turn.player)*(select_dice mod.dice))) 
    in
      if (legal == False) then mod.board
      else if (mod.dice.double == True) then Debug.todo "Double"
      else
        let
          src = (update_src mod.board n)
        in
          case n of
            24 -> update_dst src ((select_dice mod.dice) - 1) mod.turn.player
            25 -> update_dst src (n - 1 + (direction mod.turn.player) * select_dice mod.dice) mod.turn.player
            _ -> update_dst src (n + (direction mod.turn.player) * select_dice mod.dice) mod.turn.player

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

p_access : Model -> Player
p_access m  =
  let
    generic = Player 1 False False 
  in
    case m.turn.player of
      1 -> case (List.head m.players) of
        Nothing -> generic
        Just p -> p
      2 -> case (List.tail m.players) of
        Nothing -> generic
        Just xs -> case List.head xs of
          Nothing -> generic
          Just p -> p
      _ -> generic

ispbar : Model -> Bool
ispbar m =
  case m.turn.player of
    1 -> case (Array.get 24 m.board.spots) of
      Nothing -> False
      Just s -> case s.num_pieces of
        0 -> False
        _ -> True
    2 -> case (Array.get 25 m.board.spots) of
      Nothing -> False
      Just s -> case s.num_pieces of
        0 -> False
        _ -> True
    _ -> False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (model, Cmd.none)
    Double ->
      (model, Cmd.none)
    Upd_dice d -> ({model | dice = d}, Cmd.none)
    ClickedOn n ->
      if (n==(-10)) then ({model | turn = Turn (switch_turn model.turn.player)}, Random.generate Upd_dice dice_roll)
      else if (n==(-11)) then ({model | dice = { roll1 = model.dice.roll1, roll2 = model.dice.roll2, sel_d1 = True, double = model.dice.double}}, Cmd.none)
      else if (n==(-12)) then ({model | dice = { roll1 = model.dice.roll1, roll2 = model.dice.roll2, sel_d1 = False, double = model.dice.double}}, Cmd.none)
      else
        let
          barred = ispbar model -- (p_access model).barred
          --beared = p_access model.beared
          legal = True -- legal_move model n (n + ((direction model.turn.player) * (dice_val model.dice)))
          barred_brd = model.board
          brd = update_board model n
        in
          case legal of
            False -> (model, Cmd.none)
            _ -> ({model | board = brd}, Cmd.none)

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
          --Else
          LT -> case compare 24 spot of
            --Q4
            GT -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (115+((spot-19)*105)), toFloat (-380+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
            EQ -> (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (115+((spot-19)*105)), toFloat (-380+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
            --Bar
            LT -> if spot == 25 then
                    (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (0), toFloat (100+((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
                  else if spot == 26 then
                    (Collage.circle 40 |> filled (uniform clr) |> shift (toFloat (0), toFloat (-100-((cnt-1)*80)))|> onClick (ClickedOn (spot-1))) :: (makeNPieces clr spot (cnt-1))
                  else
                    []
-- iterates over all spots and calls makeNPieces with the proper number and color
spotsToPieces : Int -> List Spot -> List (Collage Msg)
spotsToPieces num spots =
  case spots of
    p::ps ->
      if p.player == 1 then
        case compare 26 num of
          GT -> makeNPieces Color.black num p.num_pieces ++ spotsToPieces (num+1) ps
          EQ -> makeNPieces Color.black num p.num_pieces ++ spotsToPieces (num+1) ps
          LT -> []
      else if p.player == 2 then
        case compare 26 num of
          GT -> makeNPieces Color.blue num p.num_pieces ++ spotsToPieces (num+1) ps
          EQ -> makeNPieces Color.blue num p.num_pieces ++ spotsToPieces (num+1) ps
          LT -> []
      else
        spotsToPieces (num+1) ps
    [] -> []

selectedCursor : Bool -> Int -> Collage Msg
selectedCursor d1Sel playerDir =
  let
    d1Pos = 272
    d2Pos = 482
  in
    if d1Sel then
      (Collage.roundedRectangle 62 62 5|> styled (transparent, solid thick (uniform green))|> shift ((toFloat (playerDir)*d1Pos), toFloat 0))
    else
      (Collage.roundedRectangle 62 62 5|> styled (transparent, solid thick (uniform green))|> shift ((toFloat (playerDir)*d2Pos), toFloat 0))
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
        --pieces on board and on bar
        spotsToPieces 1 (Array.toList model.board.spots)
        --pieces cleared
        --dice
        ++[-- selected window
           selectedCursor (model.dice.sel_d1) (direction (model.turn.player)),
           -- Dice
           ((Text.fromString (String.fromInt model.dice.roll1))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift ((toFloat (direction (model.turn.player))*272), toFloat 0) |> onClick (ClickedOn -11)),
           (Collage.roundedRectangle 60 60 5|> styled (uniform white, solid thick (uniform black))|> shift (toFloat ((direction (model.turn.player))*272), toFloat 0)|> onClick (ClickedOn -11)),
           ((Text.fromString (String.fromInt model.dice.roll2))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift ((toFloat (direction (model.turn.player))*482), toFloat 0)|> onClick (ClickedOn -12)),
           (Collage.roundedRectangle 60 60 5|> styled (uniform white, solid thick (uniform black))|> shift (toFloat ((direction (model.turn.player))*482), toFloat 0) |> onClick (ClickedOn -12)),
           -- roll button
           ((Text.fromString ("Roll"))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift (toFloat -775, toFloat 0) |> onClick (ClickedOn (-10))),
           (Collage.roundedRectangle 115 75 3|> styled (uniform white, solid thick (uniform black))|> shift (toFloat -775, toFloat 0) |> onClick (ClickedOn -10))]
        --doubling cube
        ++[((Text.fromString ("64"))|> Text.size Text.large |> Text.color Color.green |> Text.shape Text.SmallCaps |> Text.size 38 |> rendered |> shift (0, 0)),
            (Collage.square 85|> styled (uniform white, solid thick (uniform black))|> onClick (ClickedOn (-20)))]
        --q1
        ++[(Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (640, 220)|> onClick (ClickedOn 0)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (535, 220)|> onClick (ClickedOn 1)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (430, 220)|> onClick (ClickedOn 2)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (325, 220)|> onClick (ClickedOn 3)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (220, 220)|> onClick (ClickedOn 4)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (115, 220)|> onClick (ClickedOn 5)),
        --q2
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-640, 220)|> onClick (ClickedOn 11)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-535, 220)|> onClick (ClickedOn 10)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-430, 220)|> onClick (ClickedOn 9)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-325, 220)|> onClick (ClickedOn 8)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-220, 220)|> onClick (ClickedOn 7)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-115, 220)|> onClick (ClickedOn 6)),
        --q3
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-640, -220)|> onClick (ClickedOn 12)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-535, -220)|> onClick (ClickedOn 13)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-430, -220)|> onClick (ClickedOn 14)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-325, -220)|> onClick (ClickedOn 15)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (-220, -220)|> onClick (ClickedOn 16)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (-115, -220)|> onClick (ClickedOn 19)),
        --q4
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (640, -220)|> onClick (ClickedOn 23)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (535, -220)|> onClick (ClickedOn 22)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (430, -220)|> onClick (ClickedOn 21)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (325, -220)|> onClick (ClickedOn 20)),
        (Collage.ellipse 45 200|> styled (uniform white, solid thick (uniform black))|> shift (220, -220)|> onClick (ClickedOn 19)),
        (Collage.ellipse 45 200|> styled (uniform red, solid thick (uniform black))|> shift (115, -220)|> onClick (ClickedOn 18))]
        --background and bar
        ++[(Collage.rectangle 100 440|> styled (uniform brown, solid thick (uniform black))|> shift (0, 220)|> onClick (ClickedOn 24)),
           (Collage.rectangle 100 440|> styled (uniform brown, solid thick (uniform black))|> shift (0, -220)|> onClick (ClickedOn 25))]
        ++[Collage.rectangle 1400 880|> styled (uniform brown, solid thick (uniform black))]

      display =
        -- Html.text (Debug.toString (model.hitCount, model.missCount))
        svg ((stack canvas) |> scale 0.85)

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
