module GameMessages exposing (..)

import SnlMisc exposing (..)
import Http
type Msg =
            NoOp
            | New
            | NewReturned SnlMisc.Model
            | NewFailed Http.Error
            | Game SnlMisc.Msg
            | AddPlayer
            | GameAction SnlMisc.Msg


