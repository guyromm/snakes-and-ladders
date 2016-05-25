module SnlMisc exposing (..)

import Random
import Http

type alias DiceState = Int
type GameState =
               Pending
               | Ongoing
               | Ended
                 

type alias BoardPosition = Int
type PlayerState =
                 Playing
                 | Finished

type alias PlayerName = String
type alias Player = { name : PlayerName,
                      state : PlayerState,
                      position : BoardPosition,
                      turnsTaken : Int
                                 }

type Msg =
            NoOp
            | PlayThrough
            | NewGame
            | Turn
            | TurnFailed Http.Error
            | TurnReturned Model
              
            
              

type alias Model = {
  id : String
  ,players : List Player
  ,seed : Random.Seed
  ,lastRoll : Maybe DiceState
  ,whoseTurn : Maybe PlayerName
  ,status : GameState
  ,turnCount : Int
  }
