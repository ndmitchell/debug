module Trace exposing (CallData, DebugFunction, DebugTrace, callDataDecoder, traverseDebugTrace)

import Array exposing (Array)
import Dict exposing (Dict, get, toList)
import Json.Decode as Decode exposing (Decoder, decodeValue, dict, int, list, string, value)
import Json.Encode exposing (Value)
import Maybe
import Result.Extra as Result


type alias DebugTrace a =
    { functions : Array DebugFunction -- ^ Flat list of all the functions traced
    , variables : Array String -- ^ Flat list of all the variable values observed
    , calls : Array a -- ^ Flat list of all the function calls traced
    }


traverseDebugTrace : (a -> Result String b) -> DebugTrace a -> Result String (DebugTrace b)
traverseDebugTrace f trace =
    Array.map f trace.calls |> Array.toList |> Result.combine |> Result.map (\callsList -> { trace | calls = Array.fromList callsList })


type alias DebugFunction =
    { name : String -- ^ Function name
    , source : String -- ^ Function source, using @\n@ to break lines
    , arguments : List String -- ^ Variables for the arguments to the function
    , result : String -- ^ Variable for the result of the function
    }


type alias CallData =
    { callDepends : List Int
    , callParents : List Int
    , callFunction : DebugFunction
    , callLocals : Dict String Int
    }


callDataDecoder : DebugTrace a -> Decoder CallData
callDataDecoder trace =
    dict value |> Decode.andThen (decodeCallDataDict trace >> Result.unpack Decode.fail Decode.succeed)


decodeCallDataDict : DebugTrace a -> Dict String Value -> Result String CallData
decodeCallDataDict trace d =
    Result.map4 CallData
        (get "$depends" d |> Maybe.map (Decode.decodeValue (list int)) |> Maybe.withDefault (Result.Ok []))
        (get "$parents" d |> Maybe.map (Decode.decodeValue (list int)) |> Maybe.withDefault (Result.Ok []))
        (get "" d
         |> Result.fromMaybe "Corrupt trace: call without function id"
         |> Result.andThen (Decode.decodeValue int)
         |> Result.andThen (flip Array.get trace.functions >> Result.fromMaybe "Corrupt trace: missing function id"))
        (decodeCallDataLocals d)


decodeCallDataLocals : Dict String Value -> Result String (Dict String Int)
decodeCallDataLocals =
    toList
        >> List.filterMap
            (\( k, v ) ->
                if k /= "" && k /= "$parents" && k /= "$depends" then
                    Just (Decode.decodeValue int v |> Result.map (\v -> ( k, v )))
                else
                    Nothing
            )
        >> Result.combine
        >> Result.map Dict.fromList
