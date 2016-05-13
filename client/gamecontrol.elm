module GameControl where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Snl
import StartApp.Simple exposing (start)
import Signal
import Task exposing (Task, andThen)

import Debug


import Http
import Json.Decode exposing ((:=))
import Effects

type GameState =
               None
               | Pending
               | Playing
               | Finished

type Action =
            NoOp
            | RequestNew
            | New
            | Game Snl.Action
                 
type alias Model = {
    game: Maybe Snl.Model,
    id: Maybe String ,
    state: GameState
  }

initialState : Model
initialState = {
  game= Nothing, --Just Snl.initialModel,
  id=Nothing,
  state=None
  }

view : Signal.Address Action -> Model -> Html
view address model =
  case model.game of
    Nothing ->
      div []
          [
           button [ onClick address RequestNew ] [ text "New Game" ]
          ]
    Just g ->
      div []
          [
           Snl.view (Signal.forwardTo address Game) g,
           button [ onClick address RequestNew ] [ text "Restart Game" ]
          ]
          


update : Action -> Model -> ( Model, Effects.Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    Game act ->
      case model.game of
        Nothing ->
          (model, Effects.none)
        Just mg ->
          ({ model | game = Just (Snl.update act mg) } , Effects.none)
    RequestNew ->
      (model, fetchTest)
    New ->
        ({ model | game = Nothing }, Effects.none)

type alias NewGameResponse = {
    gid : String,
    state : String -- Snl.Model
    }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

-- report : String -> Task x ()
report markdown =
  Signal.send actions.address (Debug.log "test result" markdown)
                   

port fetchTest : Effects.Effects Action
port fetchTest =
  Http.getString "/test" |> Effects.task -- `andThen` Task ()
                           
                      
main =
    StartApp.Simple.start { model = initialState,
            update = update,
            view = view }
          
