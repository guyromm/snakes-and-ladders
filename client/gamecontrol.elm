module GameControl exposing (..)

-- our modules
import SnlMisc
import GameMessages exposing (..)
import GameIO exposing (..)
import Snl

-- external
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App
import Debug
import Http


type GameState =
               None
               | Pending
               | Playing
               | Finished



type alias Model = {
    game: Maybe SnlMisc.Model,
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
               button [ onClick AddPlayer ] [ text "Add Player" ],
               button [ onClick New ] [ text "Restart Game" ]
              ]
          


update : Msg -> Model -> (Model,Cmd Msg)
update action model =
  case action of
    New ->
      (model, requestNewGame)
    NewReturned g ->
      ({model | game = Just g}, Cmd.none)
    NewFailed e ->
      let
        e' = Debug.log "http err" e
      in
        Debug.log "NewFailed" (model, Cmd.none)
    AddPlayer ->
        case model.game of
            Nothing ->
                (model,Cmd.none)
            Just mg ->
                (model,GameIO.requestAddPlayer mg.id)
    NoOp ->
      (model, Cmd.none)
    Game act ->
      case model.game of
        Nothing ->
          (model, Cmd.none)
        Just mg ->
          let
              (upd,cmd) = (Snl.update act mg)
          in
              ({ model | game = Just upd }, Cmd.none)
    GameAction act ->
        (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

                     
        
main =
    Html.App.program {
            init = (initialState, Cmd.none),
            subscriptions = subscriptions,           
            update = update,
            view = view }
          

