module Snl exposing (..)

import Task
import Http

import Json.Decode
import GameIO exposing (makeDecode)
import SnlMisc exposing (GameState,DiceState,BoardPosition,Player,PlayerState,Model,PlayerName)
import GameMessages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Dict exposing (Dict)
import String
import Debug
import Random
import Char

               
diceStates : List DiceState
diceStates = [1..6]


                   



snakesLadders : Dict BoardPosition BoardPosition
snakesLadders = Dict.fromList [
                 (6, 17),
                 (14, 3),
                 (20, 15),
                 (24, 26),
                 (30, 44),
                 (39, 33),
                 (49, 62),
                 (66, 53),
                 (69, 58),
                 (79, 67),
                 (82, 86),
                 (84, 71),
                 (88, 36)
             ]


                      




initialModel : Model
initialModel =
  {
    id = "",
    players = List.map (\i -> Player (String.fromChar (Char.fromCode ((Char.toCode 'a') + i))) SnlMisc.Playing 0 0) [0..10]
    ,seed = Random.initialSeed 42
    ,lastRoll = Nothing
    ,whoseTurn = Just "a"
    ,status = SnlMisc.Pending
    ,turnCount = 0
  }

obtainPlayer : PlayerName -> List Player -> Maybe Player
obtainPlayer pn players =
  case players of
    [] ->
      Nothing
    _ ->
      List.head (List.filter (\p -> p.name == pn) players)

-- return next player, indication whether game turn count has advanced
obtainNext : PlayerName -> List Player -> (Maybe Player,Int)
obtainNext pn ingame =
  let
    cur = List.filter (\p -> (p.name > pn)) ingame
  in
    case cur of
      [] ->
        case ingame of
          [] ->
            (Nothing,0)
          _ ->
            (List.head ingame,1)
      _ ->
        (List.head cur,0)

makeMove : DiceState -> Player -> Player
makeMove ds p =
  let
    pos = p.position+ds
    delt = Debug.log "pos?" (Dict.get pos snakesLadders)
    pos' = if delt == Nothing then pos else (Maybe.withDefault -1 delt)
    state'  = if pos'>=99 then SnlMisc.Finished else SnlMisc.Playing
  in
    { p | position = pos', state = state',turnsTaken=p.turnsTaken+1}

nextClean nt =
  case nt of
    Nothing ->
      "--"
    Just n ->
      n.name

replcond : Maybe Player -> Player -> Player
replcond playing p =
  case playing of
    Just playing ->
      if p.name==playing.name then playing else p
    Nothing ->
      p


update : SnlMisc.Msg -> Model -> (Model,Cmd Msg)
update action model =
  case action of
    SnlMisc.NoOp ->
      (model,Cmd.none)
    SnlMisc.PlayThrough ->
      (model,(requestMakeTurn model.id True))
    SnlMisc.NewGame ->
      let
        seed' = model.seed
      in
        ({ initialModel | seed = seed'},Cmd.none)
    SnlMisc.Turn ->
        Debug.log "asking for turn" (model,(requestMakeTurn model.id False)) -- TODO
    SnlMisc.TurnFailed e ->
        let
            e' = Debug.log "turn returned http err" e
        in
            (model,Cmd.none)
    SnlMisc.TurnReturned m ->
        Debug.log "turn returned a wonderuful new reality" (m,Cmd.none)
        


renderCell : Int -> Int -> List Player -> String
renderCell rid cellid players =
  let
    pos = (rid*10)+cellid
    cellPlayers = List.filter (\p -> p.position==pos) players                    
    cellval = Dict.get pos snakesLadders
    disp = if cellval == Nothing then " " --("cell "++(toString pos))
           else
             if Maybe.withDefault -1 cellval >= pos
             then "H"
             else "~"
    dispP = disp ++ (String.join ","  (List.map (\p -> p.name)  cellPlayers))
  in
    dispP

drawBoard model =
  let
    header = [td [] [ text "\\" ]] ++ List.map (\hid -> td [] [ text (toString hid)]) [0..9]
    rows = List.map (\rid -> tr []
                             (
                             [ td [] [text ((toString rid))] ] ++
                              List.map (\cellid -> td [] [text (renderCell rid cellid model.players)]) [0..9]
                              )
                    ) [0..9]
    cells = List.map (\cellid -> [0..10]) rows
  in
    table [ (attribute "border" "1") ]
            ([tr [] header] ++ rows)

gameStateToString : SnlMisc.GameState -> String
gameStateToString s =
    case s of
        SnlMisc.Pending ->
            "pending"
        SnlMisc.Ended ->
            "ended"
        SnlMisc.Ongoing ->
            "ongoing"
                
drawControls model =
  let
    finished = List.map (\p -> p.name) (List.filter (\p -> p.state==SnlMisc.Finished) model.players)
    attrdis = if model.status==SnlMisc.Ended || List.length model.players==0 then [ attribute "disabled" "1" ] else []
    attren = if model.status==SnlMisc.Ended then [] else [ attribute "disabled" "1" ]
    dis2 = [ onClick SnlMisc.PlayThrough ] ++ attrdis
    dis3 = [ onClick SnlMisc.Turn ] ++ attrdis
    leastTurnsTaken = (List.foldl (\p l -> if p.turnsTaken < l then p.turnsTaken else l) 100000 model.players)
    leaders = List.filter (\p -> p.turnsTaken<=leastTurnsTaken) model.players
    leaderNames = String.join "," (List.map (\l -> l.name) leaders)
  in
    div [] [ button dis3 [ text "Roll dice" ],
             button dis2 [ text "Play through" ],
                    span [] [ text ("Turn "++(toString model.turnCount))],
                    div [] [ ],
                    span [] [ text ("Last roll is " ++ (toString (Maybe.withDefault 0 model.lastRoll)))],
                    div [] [ ],
                    span [] [ text ("Next to play is " ++ (Maybe.withDefault "--" model.whoseTurn))],
                    div [] [ ],
                    span [] [ text ("leader names: " ++ leaderNames)],
                    div [] [ ],
                    span [] [ text ("Finished: " ++ String.join "," finished)],
                    div [] [ ],
                    span [] [ text ("Game State: " ++ (gameStateToString model.status))],
                    div [] [ ],
                    span [] [ text ("Game ID: " ++ (model.id))]                        
           ]

           
requestMakeTurn : String -> Bool -> Cmd GameMessages.Msg
requestMakeTurn gameid playthrough =
    let
        c = if playthrough then NewReturnedPlaythrough else NewReturned
    in
        Task.perform NewFailed c (Http.get makeDecode ("/game/"++gameid++"/make_turn"))
               
view : Model -> Html SnlMisc.Msg
view model =
  let
    board = drawBoard model
    ctrl = drawControls model
  in
    div [] [board , ctrl]

subscriptions : Model -> Sub SnlMisc.Msg
subscriptions model =
  Sub.none
        

