import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Dict exposing (Dict)
import String
import Debug
import Random
import Char

type alias DiceState = Int
               
diceStates : List DiceState
diceStates = [1..6]

type Action =
            NoOp
            | RollDice
            | PlayThrough
            | NewGame

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
   ,gameCount : Int
    }


initialModel : Model
initialModel =
  {
    players = List.map (\i -> Player (String.fromChar (Char.fromCode ((Char.toCode 'a') + i))) Playing 0 0) [0..10]
    ,seed = Random.initialSeed 42
    ,lastRoll = Nothing
    ,whoseTurn = Just "a"
    ,turnCount = 0
    ,gameOver = False
    ,gameCount = 0
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
    state'  = if pos'>=99 then Finished else Playing
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

advanceState : Model -> Bool -> Model
advanceState model recurse =
    let
        -- who's in game?
        ingame = List.filter (\p -> p.state==Playing) model.players
        -- obtain current player
        playing = Debug.log "obtainPlayer returned " (obtainPlayer (Maybe.withDefault "--" model.whoseTurn) ingame)
        -- roll dice 
        (dr,seed') = Random.generate (Random.int 1 6 ) model.seed
        -- make the move
        playing' = Maybe.map (makeMove dr) playing
        -- update model.players
        replcond' = replcond playing'
        players' = List.map replcond' model.players
        -- update whoseTurn
        ingame' = List.filter (\p -> p.state==Playing) players'
        (newTurn,incTurnCount) = Debug.log "it's now the turn of " (obtainNext (Maybe.withDefault "--" model.whoseTurn) ingame')
        newTurnName = nextClean newTurn
        -- is game over?
        gameOver = List.length ingame'  == 0
        gameCount' = if gameOver then model.gameCount+1 else model.gameCount
        model' = { model |
                  seed = seed',
                  lastRoll = Just dr ,
                  players = players',
                  whoseTurn = Just newTurnName,
                  turnCount=model.turnCount+incTurnCount,
                  gameOver=gameOver,
                  gameCount=gameCount'
        }
      in
        if (recurse && not model'.gameOver) then (advanceState model' recurse) else model'

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    PlayThrough ->
      let
        model' = advanceState model True
      in
        model'
    NewGame ->
      let
        seed' = model.seed
        gameCount' = model.gameCount
      in
        { initialModel | seed = seed' , gameCount=gameCount'}
    RollDice ->
      advanceState model False

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

drawBoard address model =
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

drawControls address model =
  let
    finished = List.map (\p -> p.name) (List.filter (\p -> p.state==Finished) model.players)
    attrdis = if model.gameOver then [ attribute "disabled" "1" ] else []
    attren = if model.gameOver then [] else [ attribute "disabled" "1" ]
    dis = [ onClick address RollDice ] ++ attrdis
    dis2 = [ onClick address PlayThrough ] ++ attrdis
    dis3 = [ onClick address NewGame ] ++ attren
  in
    div [] [ button dis [ text "Roll dice" ],
             button dis2 [ text "Play through" ],
             button dis3 [ text "New game" ],
                    span [] [ text ("Turn "++(toString model.turnCount))],
                    div [] [ ],
                    span [] [ text ("Last roll is " ++ (toString (Maybe.withDefault 0 model.lastRoll)))],
                    div [] [ ],
                    span [] [ text ("Next to play is " ++ (Maybe.withDefault "--" model.whoseTurn))],
                    div [] [ ],
                    span [] [ text ("Finished: " ++ String.join "," finished)],
                    div [] [ ],
                    span [] [ text ("Is game over?: " ++ (if model.gameOver then "YES" else "NO"))],
                    div [] [ ],
                    span [] [ text ("game count: " ++ (toString model.gameCount))]                         
           ]

view address model =
  let
    board = drawBoard address model
    ctrl = drawControls address model
  in
    div [] [board , ctrl]

main = StartApp.start { model = initialModel,view = view,update = update }
