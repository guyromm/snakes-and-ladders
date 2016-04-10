import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Signal exposing (Address)
import Dict exposing (Dict)
import String
import Debug
import Random
import List.Extra

type alias DiceState = Int
               
diceStates : List DiceState
diceStates = [1..6]

                                                                               
            
type Action =
            NoOp
            | RollDice

type PlayerState =
                 Playing
                 | Finished
                   
type alias BoardPosition = Int


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
type alias PlayerName = String
                      
type alias Player = { name : PlayerName,
                      state : PlayerState,
                      position : BoardPosition,
                      turnsTaken : Int
                                 }

type alias Model = {
    players : List Player
   ,seed : Random.Seed
   ,lastRoll : Maybe DiceState
   ,whoseTurn : Maybe PlayerName
   ,turnCount : Int
   ,gameOver : Bool
    }


initialModel : Model
-- initialModel =
--   {
--     players = [Player "a" Playing 0 0,Player "b" Playing 0 0]
--     ,seed = Random.initialSeed 42
--     ,lastRoll = Nothing
--     ,whoseTurn = Just "a"
--     ,turnCount = 0
--     ,gameOver = False
--   }

initialModel =
  {
    players = [Player "a" Finished 99 666,Player "b" Finished 99 666]
    ,seed = Random.initialSeed 42
    ,lastRoll = Nothing
    ,whoseTurn = Just "b"
    ,turnCount = 0
    ,gameOver = False
  }

unsafeNth : Int -> List a -> a
unsafeNth n xs = case List.drop n xs of
                   [] -> Debug.crash "attempted to take nonexistant!"
                   (x::_) -> x
                                                                                          
obtainPlayer : PlayerName -> List Player -> Player
obtainPlayer pn players =
  case players of
    [] ->
      Debug.crash "players is empty!"
    _ ->
      unsafeNth 0 (List.filter (\p -> p.name == pn) players)

obtainNext : PlayerName -> List Player -> (Maybe Player,Int)
obtainNext pn ingame =
  let
    next = Debug.log "next are " List.filter (\p -> (p.name > pn)) ingame
  in
    case next of
      [] ->
        case ingame of
          [] ->
            (Nothing,0)
          _ ->
            ((Just (unsafeNth 0 ingame)),1)
      _ ->
        (Just (unsafeNth 0 next),0)

                
makeMove : Player -> DiceState -> Player
makeMove p ds =
  let
    pos = p.position+ds
    delt = Debug.log "pos?" (Dict.get pos snakesLadders)
    pos' = if delt == Nothing then pos else (Maybe.withDefault -1 delt)
    state'  = if pos'>=99 then Finished else Playing
  in
    { p | position = pos', state = state',turnsTaken=p.turnsTaken+1}

nextClean nt =
  case nt of
    Nothing ->
      "--"
    Just n ->
      n.name
              
update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    RollDice ->
      let
        -- who's in game?
        ingame = List.filter (\p -> p.state==Playing) model.players
        -- is game over?
        gameOver = List.length ingame  == 0
        -- obtain current player
        playing = obtainPlayer (Maybe.withDefault "--" model.whoseTurn) ingame
        -- roll dice 
        (dr,seed') = Random.generate (Random.int 1 6 ) model.seed
        -- make the move 
        playing' = makeMove playing dr
        newpos = Debug.log "player made move. now at  " playing'.position
        -- evaluate whether a victory has occured
        -- update model.players
        players' = List.Extra.replaceIf (\p -> p.name==playing.name) playing' model.players
        -- update whoseTurn
        ingame' = List.filter (\p -> p.state==Playing) players'

        (newTurn,incTurnCount) = Debug.log "it's now the turn of " (obtainNext (Maybe.withDefault "--" model.whoseTurn) ingame')
        newTurnName = nextClean newTurn
      in
        Debug.log "updated:" { model |
                  seed = seed',
                  lastRoll = Just dr ,
                  players = players',
                  whoseTurn = Just newTurnName,
                  turnCount=model.turnCount+incTurnCount,
                  gameOver=gameOver
        }

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
--  toString cellid


--view : address -> model -> Html
drawBoard address model =
  let
    header = [td [] [ text "\\" ]] ++ List.map (\hid -> td [] [ text (toString hid)]) [0..9]
    rows = List.map (\rid -> tr []
                             (
                             [
                              td [] [text ((toString rid))]
                             ] ++
                              List.map (\cellid -> td [] [text (renderCell rid cellid model.players)]) [0..9]

                              )
                    ) [0..9]
    cells = List.map (\cellid -> [0..10]) rows
  in
    table [ (attribute "border" "1") ]
            ([tr [] header] ++ rows)

drawControls address model =
  let
    finished = List.map (\p -> p.name) (List.filter (\p -> p.state==Finished) model.players)
    dis = if model.gameOver then [ onClick address RollDice , attribute "disabled" "1" ] else [onClick address RollDice]
  in
    div [] [ button dis [ text "Roll dice" ],
                    span [] [ text ("Turn "++(toString model.turnCount))],
                    div [] [ ],
                    span [] [ text ("Last roll is " ++ (toString (Maybe.withDefault 0 model.lastRoll)))],
                    div [] [ ],
                    span [] [ text ("Next to play is " ++ (Maybe.withDefault "--" model.whoseTurn))],
                    div [] [ ],
                    span [] [ text ("Finished: " ++ String.join "," finished)],
                    div [] [ ],
                    span [] [ text ("Is game over?: " ++ (if model.gameOver then "YES" else "NO"))]
           ]
         
view address model =
  let
    board = drawBoard address model
    ctrl = drawControls address model
  in
    div [] [board , ctrl]

main =
  StartApp.start { model = initialModel,
                           view = view,
                           update = update }
