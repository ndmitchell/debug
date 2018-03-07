-- Generate the output by running
-- > elm-make Main.elm --output=../html/debug.js

module Main exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import List
import Maybe.Extra as Maybe
import Regex exposing (..)
import Set
import String
import Trace exposing (..)
import Tuple exposing (second)


type Msg
    = SelectCall Int
    | SelectFunction String
    | CallFilter String


type alias ProcessedCall =
    { rendered : String, call : CallData }


type alias Model =
    { selectedCall : Int
    , selectedFunction : Maybe String
    , callFilter : Maybe Regex
    , trace : Result String (DebugTrace ProcessedCall)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCall x ->
            ( { model | selectedCall = x }, Cmd.none )

        SelectFunction x ->
            ( { model
                | selectedFunction =
                    if x == "(All)" then
                        Nothing
                    else
                        Just x
              }
            , Cmd.none
            )

        CallFilter x ->
            ( { model
                | callFilter =
                    if x == "" then
                        Nothing
                    else
                        Just (regex x)
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.trace of
        Result.Err e ->
            text e

        Result.Ok trace ->
            let
                processedCall =
                    Array.get model.selectedCall trace.calls

                fun =
                    processedCall |> Maybe.map (\processedCall -> processedCall.call.callFunction)
            in
            table [ style [ ( "height", "100%" ), ( "width", "100%" ) ] ]
                [ tr [] [ td [ colspan 3 ] [ h1 [] [ text "Haskell Debugger" ] ] ]
                , tr []
                    [ td [ rowspan 3, style [ ( "width", "25%" ), ( "padding-right", "40px" ) ] ]
                        [ h2 [] [ text "Functions" ]
                        , viewCallList model trace
                        ]
                    , td []
                        [ h2 [] [ text "Source" ]
                        , ul [ id "function-source" ]
                            [ viewSource trace (Maybe.map (\x -> x.call) processedCall) ]
                        ]
                    ]
                , tr []
                    [ td []
                        [ h2 [] [ text "Variables" ]
                        , viewVariables trace (Maybe.map (\x -> x.call) processedCall)
                        ]
                    ]
                , tr [ id "function-depends-section" ]
                    [ td []
                        [ h2 [] [ text "Calls" ]
                        , viewCallStack trace (Maybe.map (\x -> x.call) processedCall)
                        ]
                    ]
                ]


viewCallList : Model -> DebugTrace ProcessedCall -> Html Msg
viewCallList model trace =
    div []
        [ select [ id "function-drop", style [ ( "width", "100%" ) ], onInput SelectFunction ]
            (option [] [ text "(All)" ]
                :: (trace.functions |> Array.map (\x -> x.name) |> Array.toList |> Set.fromList |> Set.toList |> List.map (\x -> option [] [ text x ]))
            )
        , input
            [ id "function-text"
            , style [ ( "margin-top", "5px" ), ( "width", "100%" ) ]
            , type_ "text"
            , placeholder "Filter (using regex)"
            , onInput CallFilter
            ]
            []
        , ul [ id "call-list" ]
            (matchCalls trace model.selectedFunction model.callFilter
                |> List.map
                    (\( i, c ) ->
                        li [] [ viewCallLink i c.rendered ]
                    )
            )
        ]


viewCallLink : Int -> String -> Html Msg
viewCallLink index rendered =
    a [ href "javascript:()", onClick (SelectCall index) ] [ text rendered ]


viewVariables : DebugTrace a -> Maybe CallData -> Html Msg
viewVariables trace call =
    case call of
        Nothing ->
            text "Corrupt trace: no call"

        Just call ->
            ul [ id "function-variables" ]
                (case findCallDetails trace call of
                    Just ( res, vals ) ->
                        li [] [ pre [] [ text <| "$result = " ++ res ] ]
                            :: List.map
                                (\( n, v ) ->
                                    li [] [ pre [] [ text (n ++ " = " ++ v) ] ]
                                )
                                vals

                    _ ->
                        []
                )


viewCallStack : DebugTrace ProcessedCall -> Maybe CallData -> Html Msg
viewCallStack trace call =
    case call of
        Nothing ->
            text "Corrupt trace: no call"

        Just call ->
            case getCallStackUpwards trace call of
                Result.Err e ->
                    text e

                Result.Ok callstack ->
                    let
                        upwards =
                            List.foldl buildCallStackList (\k -> ul [ id "function-depends" ] [ k ]) (List.reverse callstack)

                        descendants =
                            call.callDepends |> List.map (\i -> ( i, Array.get i trace.calls |> Maybe.map (\x -> x.rendered) |> Maybe.withDefault "Corrupt trace" ))

                        downwards =
                            li []
                                [ text (renderCall trace call)
                                , ul [] (descendants |> List.map (\x -> li [] [ uncurry viewCallLink x ]))
                                ]
                    in
                    upwards downwards


viewSource : DebugTrace a -> Maybe CallData -> Html msg
viewSource trace call =
    case call of
        Nothing ->
            text "Corrupt trace: no call"

        Just call ->
            let
                loop src =
                    case find (AtMost 1) hsLexer src of
                        [] ->
                            []

                        { match } :: _ ->
                            case match of
                                "" ->
                                    []

                                "where" ->
                                    span [ class "hs-keyword" ] [ text "where" ]
                                        :: loop (String.dropLeft (String.length match) src)

                                _ ->
                                    (case String.indexes match symbols of
                                        _ :: _ ->
                                            span [ class "hs-keyglyph" ] [ text match ]

                                        [] ->
                                            case getArg trace call match of
                                                Just arg ->
                                                    abbr [ title arg ] [ text match ]

                                                Nothing ->
                                                    text match
                                    )
                                        :: loop (String.dropLeft (String.length match) src)
            in
            pre [] (loop call.callFunction.source)


getArg :
    DebugTrace a
    -> CallData
    -> String
    -> Maybe String
getArg trace call arg =
    Dict.get arg call.callLocals
        |> Maybe.andThen
            (\ix ->
                Array.get ix trace.variables
            )


hsLexer : Regex
hsLexer =
    regex """[a-zA-Z][a-zA-Z0-9]+|[\x0D
]|."""


symbols : String
symbols =
    """->:=()[]"""


buildCallStackList : ( Int, String ) -> (Html Msg -> b) -> Html Msg -> b
buildCallStackList call parent k =
    parent <| li [] [ uncurry viewCallLink call, ul [] [ k ] ]


getCallStackUpwards : DebugTrace ProcessedCall -> CallData -> Result String (List ( Int, String ))
getCallStackUpwards trace call =
    case call.callParents of
        [ i ] ->
            Array.get i trace.calls |> Result.fromMaybe "Corrupt trace" |> Result.andThen (\p -> getCallStackUpwards trace p.call |> Result.map (\x -> ( i, p.rendered ) :: x))

        [] ->
            Result.Ok []

        pp ->
            Result.Err <| "Unexpected: more than one parents for a call: " ++ toString pp


matchCalls : DebugTrace ProcessedCall -> Maybe String -> Maybe Regex -> List ( Int, ProcessedCall )
matchCalls trace selectedFunction callFilter =
    trace.calls
        |> Array.toIndexedList
        |> List.filter
            (\( i, c ) ->
                let
                    filterByName =
                        case selectedFunction of
                            Nothing ->
                                True

                            Just n ->
                                c.call.callFunction.name == n

                    filterByCall =
                        case callFilter of
                            Nothing ->
                                True

                            Just r ->
                                contains r c.rendered
                in
                filterByName && filterByCall
            )


findCallDetails : DebugTrace a -> CallData -> Maybe ( String, List ( String, String ) )
findCallDetails trace call =
    (call.callFunction.result :: call.callFunction.arguments)
        |> List.map
            (\arg ->
                case Dict.get arg call.callLocals of
                    Nothing ->
                        Just ( arg, "_" )

                    Just valueIx ->
                        Array.get valueIx trace.variables |> Maybe.map (\x -> ( arg, x ))
            )
        |> Maybe.combine
        |> Maybe.map
            (\vv ->
                case vv of
                    ( _, res ) :: args ->
                        ( res, args )

                    _ ->
                        ( "_", [] )
            )


renderCall : DebugTrace a -> CallData -> String
renderCall trace call =
    case findCallDetails trace call of
        Nothing ->
            "Invalid trace: value null ref"

        Just ( res, args ) ->
            String.join " " <| call.callFunction.name :: List.map second args ++ [ " = ", res ]


processCall : DebugTrace a -> CallData -> ProcessedCall
processCall tr call =
    { call = call, rendered = renderCall tr call }


main : Program (DebugTrace Value) Model Msg
main =
    programWithFlags
        { init =
            \tr ->
                ( { trace = traverseDebugTrace (decodeValue (callDataDecoder tr) >> Result.map (processCall tr)) tr
                  , selectedCall = 0
                  , selectedFunction = Maybe.Nothing
                  , callFilter = Nothing
                  }
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
