module Board exposing (..)

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


type alias Dice =
  {
  roll1 : Int,
  roll2 : Int,
  sel_d1 : Bool,
  double : Bool
  }


type alias Move =
  {
  src : Int,
  dst : Int
  }

type alias Score =
  {
  p1 : Int,
  p2 : Int,
  new_double : Bool,
  doubled_val : Int,
  dbl_p1_ctrl : Int
  }

type alias Turn =
  {
    moves_left : Int,
    d1_used : Bool,
    player : Int
  }

type alias Model =
  {
    board : Board,
    dice : Dice,
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
       {num_pieces = 0, vulnerable = False, player = 1}, --[24], ClickedOn 25, Bar p1
       {num_pieces = 0, vulnerable = False, player = 2}, --[25], ClickedOn 26, Bar p2
       {num_pieces = 0, vulnerable = False, player = 1}, --[26], ClickedOn 27, Score p1
       {num_pieces = 0, vulnerable = False, player = 2}  --[27], ClickedOn 28, Score p2
       ])
   },
   dice = { roll1 = 3, roll2 = 1, sel_d1 = True, double = False},
   players = [{player_num = 1, beared = False, barred = False}, {player_num = 2, beared = False, barred = False}],
   score = {p1 = 0, p2 = 0, doubled_val = 1, dbl_p1_ctrl = 0, new_double = False},
   turn = {moves_left = 2, d1_used = False, player = 1}
  }


init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Random.generate Upd_dice dice_roll)

-- UPDATE

type Msg
  = 
  ClickedOn Int
  | Tick
  | Upd_score
  | Upd_dice Dice

bar_loc : Int -> Int
bar_loc pl =
  case pl of
    1 -> 24
    _ -> 25

out_loc : Int -> Int
out_loc pl =
  case pl of
    1 -> 26
    _ -> 27

legal_move : Model -> Int -> Int -> Bool
legal_move m src dst =
  if (m.turn.moves_left <= 0) then False
  else if (m.turn.moves_left == 1 && (m.turn.d1_used == m.dice.sel_d1)) then False
  else if (m.turn.player == 1 && (dst == 25 || dst == 24)) then False
  else if ((Maybe.withDefault (Spot 0 False 0) (Array.get (bar_loc m.turn.player)  m.board.spots)).num_pieces) > 0 && (src /= (bar_loc m.turn.player)) then False
  else if ((not (beared m m.turn.player)) && (dst == out_loc m.turn.player)) then False
  else
    let
      spts = m.board.spots
      dst_check =
        case (Array.get dst spts) of
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
        case (Array.get src spts) of
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
          15 -> True
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
          15 -> True
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
  if (n + (direction mod.turn.player * (select_dice mod.dice)) < 0 && n /= 25 && (beared mod mod.turn.player)) then
    update_dst (update_src mod.board n) 27 mod.turn.player
  else if (n + (direction mod.turn.player * (select_dice mod.dice)) > 23 && n /= 24 && (beared mod mod.turn.player)) then
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



noMove : Model -> Bool
noMove model =
  case model.turn.player of
    1 ->
      let
        barred = case (Maybe.withDefault (Spot 0 False 0) (Array.get (bar_loc 1) model.board.spots)).num_pieces of
          0 -> False
          _ -> True
        r1_p1 = case (Maybe.withDefault (Spot 0 False 0) (Array.get model.dice.roll1 model.board.spots)) of
          spot1 -> ((spot1.num_pieces > 1) && (spot1.player /= model.turn.player))
        r2_p1 = case (Maybe.withDefault (Spot 0 False 0) (Array.get model.dice.roll2 model.board.spots)) of
          spot1 -> ((spot1.num_pieces > 1) && (spot1.player /= model.turn.player))
      in
        case model.turn.moves_left of
          1 -> barred && (r1_p1 || r2_p1)
          _ -> barred && r1_p1 && r2_p1
    2 ->
      let
        barred = case (Maybe.withDefault (Spot 0 False 0) (Array.get (bar_loc 2) model.board.spots)).num_pieces of
          0 -> False
          _ -> True
        r1_p2 = case (Maybe.withDefault (Spot 0 False 0) (Array.get (24 - model.dice.roll1) model.board.spots)) of
          spot1 -> ((spot1.num_pieces > 0) && (spot1.player /= model.turn.player))
        r2_p2 = case (Maybe.withDefault (Spot 0 False 0) (Array.get (24 - model.dice.roll2) model.board.spots)) of
          spot1 -> ((spot1.num_pieces > 0) && (spot1.player /= model.turn.player))
      in
        case model.turn.moves_left of
          1 -> barred && (r1_p2 || r2_p2)
          _ -> barred && r1_p2 && r2_p2
    _ -> False



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (model, Cmd.none)
    Upd_dice d ->
      let
        tn = model.turn
      in
        if (d.roll1 == d.roll2) then ({model | dice = {d | double = True}, turn = {tn | moves_left = 4}}, Cmd.none)
        else
          ({model | dice = {d | double = False}}, Cmd.none)
    Upd_score ->
      let
        doubled = model.score.doubled_val
        p1won = case (Array.get 26 model.board.spots) of
          Nothing -> 0
          Just spot ->
            case spot.num_pieces of
              15 -> case (Array.get 27 model.board.spots) of
                Nothing -> doubled
                Just spot2 -> case spot2.num_pieces of
                  0 -> 2 * doubled
                  _ -> doubled
              _ -> 0
        p2won = case (Array.get 27 model.board.spots) of
            Nothing -> 0
            Just spot ->
              case spot.num_pieces of
                15 -> case (Array.get 26 model.board.spots) of
                  Nothing -> doubled
                  Just spot2 -> case spot2.num_pieces of
                    0 -> 2 * doubled
                    _ -> doubled
                _ -> 0
        tmp_sc = model.score
        sco = {tmp_sc | p1 = tmp_sc.p1 + p1won, p2 = tmp_sc.p2 + p2won}
      in
        if(p1won == 0 && p2won ==0) then (model, Cmd.none)
        else
          ({initModel | score = sco}, Cmd.none)
    ClickedOn n ->
      if (n==(-10)) then
        if (model.turn.moves_left == 0 ) then
          let
            sc = model.score
          in  
            ({model | turn =  Turn 2 False (switch_turn model.turn.player), score = {sc | new_double = False  }}, Random.generate Upd_dice dice_roll)
        else (model, Cmd.none)
      else if (n==(-11)) then ({model | dice = { roll1 = model.dice.roll1, roll2 = model.dice.roll2, sel_d1 = True, double = model.dice.double}}, Cmd.none)
      else if (n==(-12)) then ({model | dice = { roll1 = model.dice.roll1, roll2 = model.dice.roll2, sel_d1 = False, double = model.dice.double}}, Cmd.none)
      else if (n==(-20)) then
        if (model.score.dbl_p1_ctrl == 0) then
          --if nobody has the cube
          if (model.turn.player == 1) then
            -- p1 doubles
            ({model | score = {p1 = model.score.p1, p2 = model.score.p2, doubled_val = (2), dbl_p1_ctrl = 1, new_double = True}}, Cmd.none)
          else
            -- p2 doubles
            ({model | score = {p1 = model.score.p1, p2 = model.score.p2, doubled_val = (2), dbl_p1_ctrl = -1, new_double = True}}, Cmd.none)
        else if (model.score.dbl_p1_ctrl == 1) then
          --if p2 has the cube
          if (model.turn.player == 2) then
            ({model | score = {p1 = model.score.p1, p2 = model.score.p2, new_double = True,  doubled_val = (model.score.doubled_val * 2), dbl_p1_ctrl = -1}}, Cmd.none)
          else
            (model, Cmd.none)
        else if (model.score.dbl_p1_ctrl == -1) then
          --if p1 has the cube
          if (model.turn.player == 1) then
            ({model | score = {p1 = model.score.p1, p2 = model.score.p2, doubled_val = (model.score.doubled_val * 2), dbl_p1_ctrl = 1, new_double = True}}, Cmd.none)
          else
            (model, Cmd.none)
        else
          (model, Cmd.none)
      else if (n==(-31)) then
        --p1Forfeit
        if (model.turn.player == 1 || (model.turn.player == 2 && model.score.new_double == True)) then
          if (model.score.new_double == True) then
            ({initModel | score = {p1 = model.score.p1, p2 = (model.score.p2 + (model.score.doubled_val // 2)), doubled_val = 1, dbl_p1_ctrl = 0, new_double = False}}, Cmd.none)
          else
            ({initModel | score = {p1 = model.score.p1, p2 = (model.score.p2 + model.score.doubled_val), doubled_val = 1, dbl_p1_ctrl = 0, new_double = False}}, Cmd.none)
        else
          (model, Cmd.none)
      else if (n==(-32)) then
        --p2Forfeit
        if (model.turn.player == 2 || (model.turn.player == 1 && model.score.new_double == True)) then
          if (model.score.new_double == True) then
            ({initModel | score = {p1 = (model.score.p1 + (model.score.doubled_val // 2)), p2 = model.score.p2, doubled_val = 1, dbl_p1_ctrl = 0, new_double = False}}, Cmd.none)    
          else
            ({initModel | score = {p1 = (model.score.p1 + model.score.doubled_val), p2 = model.score.p2, doubled_val = 1, dbl_p1_ctrl = 0, new_double = False}}, Cmd.none)
        else
          (model, Cmd.none)
      else if (noMove model) then
       -- Debug.log "no" ( toString (noMove model))
         let
           trn = model.turn
         in
           -- Debug.log "Hello"
           ({model | turn = {trn | moves_left = 0}}, Cmd.none) 
      else
        let
          brd = update_board model n
          legal = not(brd == model.board)
          trn = model.turn
          m_dice = model.dice
          d1_play = model.dice.sel_d1
          moves = trn.moves_left - 1
          selection = case moves of
            1 -> not (d1_play)
            _ -> m_dice.sel_d1

        in
          case legal of
            False -> (model, Cmd.none)
            _ -> update Upd_score {model | board = brd, turn = {trn | moves_left = trn.moves_left - 1, d1_used = d1_play}, dice = {m_dice | sel_d1 = selection}}

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

turnCursor : Int -> Collage Msg
turnCursor player =
  if (player == 1) then
    (Collage.roundedRectangle 200 100 5|> styled (transparent, solid thin (uniform black))|> shift (toFloat -820, toFloat -240))
  else
    (Collage.roundedRectangle 200 100 5|> styled (transparent, solid thin (uniform blue))|> shift (toFloat -820, toFloat 240))

selectedCursor : Bool -> Int -> Int -> Collage Msg
selectedCursor d1Sel playerDir moves=
  if (moves==0) then
    (Collage.roundedRectangle 117 97 3|> styled (transparent, solid thick (uniform green))|> shift (toFloat -820, toFloat 0))
  else
    let
      d1Pos = 272
      d2Pos = 482
    in
      if d1Sel then
        (Collage.roundedRectangle 62 62 5|> styled (transparent, solid thick (uniform green))|> shift ((toFloat (playerDir)*d1Pos), toFloat 0))
      else
        (Collage.roundedRectangle 62 62 5|> styled (transparent, solid thick (uniform green))|> shift ((toFloat (playerDir)*d2Pos), toFloat 0))

renderOffBlack : Int -> List (Collage Msg)
renderOffBlack cnt =
  if cnt > 0 then
    (Collage.circle 40 |> styled (uniform black, solid thick (uniform blue)) |> shift (toFloat (750), toFloat (-420+((cnt-1)*27)))) :: renderOffBlack (cnt-1)
  else
    []

renderOffBlue : Int -> List (Collage Msg)
renderOffBlue cnt =
  if cnt > 0 then
    (Collage.circle 40 |> styled (uniform blue, solid thick (uniform black)) |> shift (toFloat (750), toFloat (420-((cnt-1)*27)))) :: renderOffBlue (cnt-1)
  else
    []


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
        --pieces cleared and score
        ++ renderOffBlack ((Maybe.withDefault ({num_pieces = 0, vulnerable = False, player = 0}) (Array.get 26 model.board.spots)).num_pieces)
        ++ renderOffBlue ((Maybe.withDefault ({num_pieces = 0, vulnerable = False, player = 0}) (Array.get 27 model.board.spots)).num_pieces)
        ++[((Text.fromString (String.fromInt ((Maybe.withDefault ({num_pieces = 0, vulnerable = False, player = 0}) (Array.get 26 model.board.spots)).num_pieces)))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat 820, toFloat -220)),
           ((Text.fromString (String.fromInt ((Maybe.withDefault ({num_pieces = 0, vulnerable = False, player = 0}) (Array.get 27 model.board.spots)).num_pieces)))|> Text.size Text.large |> Text.color Color.blue |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat 820, toFloat 220)),
           ((Text.fromString ("Games Won:"))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat -820, toFloat -220)),
           ((Text.fromString ("Games Won:"))|> Text.size Text.large |> Text.color Color.blue |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat -820, toFloat 260)),
           ((Text.fromString (String.fromInt (model.score.p1)))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat -820, toFloat -260)),
           ((Text.fromString (String.fromInt (model.score.p2)))|> Text.size Text.large |> Text.color Color.blue |> Text.shape Text.SmallCaps |> Text.size 32 |> rendered |> shift (toFloat -820, toFloat 220))]
        --dice
        ++[-- selected window
           selectedCursor (model.dice.sel_d1) (direction (model.turn.player)) (model.turn.moves_left),
           turnCursor (model.turn.player),
           -- Dice
           ((Text.fromString (String.fromInt model.dice.roll1))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift ((toFloat (direction (model.turn.player))*272), toFloat 0) |> onClick (ClickedOn -11)),
           (Collage.roundedRectangle 60 60 5|> styled (uniform white, solid thick (uniform black))|> shift (toFloat ((direction (model.turn.player))*272), toFloat 0)|> onClick (ClickedOn -11)),
           ((Text.fromString (String.fromInt model.dice.roll2))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift ((toFloat (direction (model.turn.player))*482), toFloat 0)|> onClick (ClickedOn -12)),
           (Collage.roundedRectangle 60 60 5|> styled (uniform white, solid thick (uniform black))|> shift (toFloat ((direction (model.turn.player))*482), toFloat 0) |> onClick (ClickedOn -12)),
           -- roll button
           ((Text.fromString ("Roll"))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift (toFloat -820, toFloat 0) |> onClick (ClickedOn (-10))),
           (Collage.roundedRectangle 115 95 3|> styled (uniform white, solid thin (uniform white))|> shift (toFloat -820, toFloat 0) |> onClick (ClickedOn -10))]
        --doubling cube
        ++[((Text.fromString (String.fromInt model.score.doubled_val))|> Text.size Text.large |> Text.color Color.green |> Text.shape Text.SmallCaps |> Text.size 38 |> rendered |> shift (toFloat 0, toFloat (360*model.score.dbl_p1_ctrl)) |> onClick (ClickedOn (-20))),
            (Collage.square 85|> styled (uniform white, solid thick (uniform black))|> shift (toFloat 0, toFloat (360*model.score.dbl_p1_ctrl)) |> onClick (ClickedOn (-20)))]
        --forfeit buttons
        ++[((Text.fromString ("Forfeit"))|> Text.size Text.large |> Text.color Color.black |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift (toFloat -820, toFloat -120) |> onClick (ClickedOn (-31))),
           (Collage.roundedRectangle 145 55 3|> styled (uniform white, solid thin (uniform red))|> shift (toFloat -820, toFloat -120) |> onClick (ClickedOn -31)),
           ((Text.fromString ("Forfeit"))|> Text.size Text.large |> Text.color Color.blue |> Text.shape Text.SmallCaps |> Text.size 30 |> rendered |> shift (toFloat -820, toFloat 120) |> onClick (ClickedOn (-32))),
           (Collage.roundedRectangle 145 55 3|> styled (uniform white, solid thin (uniform red))|> shift (toFloat -820, toFloat 120) |> onClick (ClickedOn -32))]
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
           (Collage.rectangle 100 440|> styled (uniform brown, solid thick (uniform black))|> shift (0, -220)|> onClick (ClickedOn 25)),
           (Collage.rectangle 1400 880|> styled (uniform brown, solid thick (uniform black)))]

      display =
        -- Html.text (Debug.toString (model.hitCount, model.missCount))
        svg ((stack canvas) |> scale 0.8)

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
