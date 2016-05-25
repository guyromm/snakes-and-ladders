module GameIO exposing (..)

import Random exposing (Seed)
import Task exposing (Task, andThen)
import GameMessages exposing (..)
import Http
import Json.Decode exposing (..)
import SnlMisc exposing (..)

-- helper funcs for decoders
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
  f `Json.Decode.andThen` (\f' -> f' `map` aDecoder)
              
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) = apply
                               

-- decoders

gameStateNameDecoder : String -> Decoder SnlMisc.GameState
gameStateNameDecoder state = succeed (gameState state)

gameState : String -> SnlMisc.GameState
gameState state =
  case state of
    "pending" -> SnlMisc.Pending
    "ongoing" -> SnlMisc.Ongoing
    "ended" -> SnlMisc.Ended
    _ -> SnlMisc.Pending -- FIXME!
playerStateDecoder : String -> Decoder SnlMisc.PlayerState
playerStateDecoder status = succeed (playerState status)

playerState : String -> SnlMisc.PlayerState
playerState status =
  case status of
    "playing" -> SnlMisc.Playing
    "finished" -> SnlMisc.Finished
    _ -> SnlMisc.Playing -- FIXME!

playerDecoder : Decoder SnlMisc.Player
playerDecoder = succeed SnlMisc.Player
                |: ("name" := string)
                |: (("state" := string) `Json.Decode.andThen` playerStateDecoder)
                |: ("position" := int)
                |: ("turnsTaken" := int)


gameStateDecoder : Decoder SnlMisc.Model
gameStateDecoder = succeed SnlMisc.Model
                  |: ("id" := string)
                  |: ("players" := Json.Decode.list playerDecoder)
                  |: ("seed" := (succeed (Random.initialSeed 42)))
                  |: (maybe ("lastRoll" := int))
                  |: (maybe ("whoseTurn" := string))
                  |: (("status" := string) `Json.Decode.andThen` gameStateNameDecoder)
                  |: ("turnCount" := int)
                  |: ("gameOver" := bool)

                     

makeDecode : Decoder SnlMisc.Model
makeDecode =
  Json.Decode.at ["state"] gameStateDecoder
      
requestNewGame : Cmd GameMessages.Msg
requestNewGame =
  Task.perform NewFailed NewReturned (Http.get makeDecode "/game/new")

requestAddPlayer : String -> Cmd GameMessages.Msg
requestAddPlayer gameid =
    Task.perform NewFailed NewReturned (Http.get makeDecode ("/game/"++gameid++"/player/new"))


