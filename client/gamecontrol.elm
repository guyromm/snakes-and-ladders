module GameControl exposing (..)

import Random exposing (Seed)
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Snl
import Html.App
import Task exposing (Task, andThen)

import Debug


import Http


type GameState =
               None
               | Pending
               | Playing
               | Finished

type Msg =
            NoOp
            | New
            | NewReturned Snl.Model
            | NewFailed Http.Error
            | Game Snl.Msg


type alias Model = {
    game: Maybe Snl.Model,
    id: Maybe String ,
    state: GameState
  }

apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
  f `Json.Decode.andThen` (\f' -> f' `map` aDecoder)
              
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) = apply
                               
initialState : Model
initialState = {
  game= Nothing,
  id=Nothing,
  state=None
  }

view : Model -> Html Msg
view model =
  case model.game of
    Nothing ->
      div []
          [
           button [ onClick New ] [ text "New Game" ]
          ]
    Just g ->
      let
        _ = Debug.log "hi there bastard" 1
      in
        div []
              [
               Html.App.map Game (Snl.view g),
               button [ onClick New ] [ text "Restart Game" ]
              ]
          


update : Msg -> Model -> (Model,Cmd Msg)
update action model =
  case action of
    -- New ->
    --     { model | game = Just (Snl.initialModel) }
    New ->
      (model, requestNewGame)
    NewReturned g ->
      ({model | game = Just g}, Cmd.none)
    NewFailed e ->
      let
        e' = Debug.log "http err" e
      in
        Debug.log "NewFailed" (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)
    Game act ->
      case model.game of
        Nothing ->
          (model, Cmd.none)
        Just mg ->
          ({ model | game = Just (Snl.update act mg) }, Cmd.none)

type alias NewGameResponse = {
    gid : String,
    state : String
    }
                           
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

gameStateNameDecoder : String -> Decoder Snl.GameState
gameStateNameDecoder state = succeed (gameState state)

gameState : String -> Snl.GameState
gameState state =
  case state of
    "pending" -> Snl.Pending
    "ongoing" -> Snl.Ongoing
    "ended" -> Snl.Ended
    _ -> Snl.Pending -- FIXME!
playerStateDecoder : String -> Decoder Snl.PlayerState
playerStateDecoder status = succeed (playerState status)

playerState : String -> Snl.PlayerState
playerState status =
  case status of
    "playing" -> Snl.Playing
    "finished" -> Snl.Finished
    _ -> Snl.Playing -- FIXME!

playerDecoder : Decoder Snl.Player
playerDecoder = succeed Snl.Player
                |: ("name" := string)
                |: (("state" := string) `Json.Decode.andThen` playerStateDecoder)
                |: ("position" := int)
                |: ("turnsTaken" := int)


gameStateDecoder : Decoder Snl.Model
gameStateDecoder = succeed Snl.Model
                  |: ("players" := Json.Decode.list playerDecoder)
                  |: ("seed" := (succeed (Random.initialSeed 42)))
                  |: (maybe ("lastRoll" := int))
                  |: (maybe ("whoseTurn" := string))
                  |: (("status" := string) `Json.Decode.andThen` gameStateNameDecoder)
                  |: ("turnCount" := int)
                  |: ("gameOver" := bool)
                     
makeDecode : Decoder Snl.Model
makeDecode =
  Json.Decode.at ["state"] gameStateDecoder
      
requestNewGame : Cmd Msg
requestNewGame =
  Task.perform NewFailed NewReturned (Http.get makeDecode "/game/new")
     
main =
    Html.App.program {
            init = (initialState, Cmd.none),
            subscriptions = subscriptions,           
            update = update,
            view = view }
          

