module GameControl where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Snl
import StartApp.Simple exposing (start)
import Signal



import Http
type GameState =
               None
               | Pending
               | Playing
               | Finished

type Action =
            NoOp
            | New
            | Game Snl.Action
                 
type alias Model = {
    game: Maybe Snl.Model,
    id: Maybe String ,
    state: GameState
  }

initialState : Model
initialState = {
  game= Just Snl.initialModel, -- Nothing,
--  game = Nothing,
  id=Nothing,
  state=None
  }

view : Signal.Address Action -> Model -> Html
view address model =
  case model.game of
    Nothing ->
      div []
          [
           button [ onClick address New ] [ text "New Game" ]
          ]
    Just g ->
      div []
          [
           Snl.view (Signal.forwardTo address Game) g,
           button [ onClick address New ] [ text "Restart Game" ]
          ]
          


update : Action -> Model -> Model
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
      { model | game = Nothing }

main =
    StartApp.Simple.start { model = initialState,
            update = update,
            view = view }
          
