module GameControl exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Snl
import Html.App
import Task exposing (Task, andThen)

import Debug


import Http
import Json.Decode exposing ((:=))

type GameState =
               None
               | Pending
               | Playing
               | Finished

type Msg =
            NoOp
            | New
            | Game Snl.Msg
                 
type alias Model = {
    game: Maybe Snl.Model,
    id: Maybe String ,
    state: GameState
  }

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
          


update : Msg -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Game act ->
      case model.game of
        Nothing ->
          model
        Just mg ->
          { model | game = Just (Snl.update act mg) }
    New ->
        { model | game = Just (Snl.initialModel) }

type alias NewGameResponse = {
    gid : String,
    state : String
    }
                           
                      
main =
    Html.App.beginnerProgram { model = initialState,
            update = update,
            view = view }
          

