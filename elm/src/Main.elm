-- Generate the output by running
-- > elm-make src/Main.elm --output=../html/debug.js


module Main exposing (..)

import Array exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Navigation exposing (..)
import Paginate exposing (..)
import Trace exposing (..)
import Types exposing (..)
import Update exposing(..)
import View exposing (..)

main : Program (DebugTrace Value) Model Msg
main =
    Navigation.programWithFlags ChangeLocation
        { init =
            \tr loc ->
                let
                    unpackedTrace =
                        traverseDebugTrace (decodeValue (callDataDecoder tr) >> Result.map (processCall tr)) tr

                    callsPerPage0 =
                        20
                in
                ( { trace = unpackedTrace
                  , location = loc
                  , selectedFunction = Maybe.Nothing
                  , callFilter = Maybe.Nothing
                  , filteredCalls = unpackedTrace |> Result.map (\x -> Array.toIndexedList x.calls) |> Result.withDefault [] |> Paginate.fromList callsPerPage0
                  }
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }

