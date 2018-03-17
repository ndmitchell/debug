module Update exposing (update)

import Array exposing (..)
import Paginate exposing (..)
import Regex exposing (..)
import Trace exposing (..)
import Types exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation x ->
            ( { model | location = x }, Cmd.none )

        SelectFunction x ->
            let
                selectedFunction =
                    if x == "(All)" then
                        Nothing
                    else
                        Just x
            in
            ( { model
                | selectedFunction = selectedFunction
                , filteredCalls =
                    model.trace
                        |> Result.toMaybe
                        |> Maybe.map (matchCalls selectedFunction model.callFilter)
                        |> Maybe.withDefault []
                        |> Paginate.fromList (Paginate.itemsPerPage model.filteredCalls)
              }
            , Cmd.none
            )

        CallFilter x ->
            let
                callFilter =
                    if x == "" then
                        Nothing
                    else
                        Just (regex x)
            in
            ( { model
                | callFilter = callFilter
                , filteredCalls =
                    model.trace
                        |> Result.toMaybe
                        |> Maybe.map (matchCalls model.selectedFunction callFilter)
                        |> Maybe.withDefault []
                        |> Paginate.fromList (Paginate.itemsPerPage model.filteredCalls)
              }
            , Cmd.none
            )

        CallPage First ->
            ( { model | filteredCalls = Paginate.first model.filteredCalls }, Cmd.none )

        CallPage Last ->
            ( { model | filteredCalls = Paginate.last model.filteredCalls }, Cmd.none )

        CallPage Next ->
            ( { model | filteredCalls = Paginate.next model.filteredCalls }, Cmd.none )

        CallPage Prev ->
            ( { model | filteredCalls = Paginate.prev model.filteredCalls }, Cmd.none )

        CallPage (GoTo n) ->
            ( { model | filteredCalls = Paginate.goTo n model.filteredCalls }, Cmd.none )

        ChangeCallPageSize size ->
            let
                sizeAsInt =
                    Result.withDefault 10 <| String.toInt size
            in
            ( { model
                | filteredCalls =
                    let
                        newPage =
                            1 + (Paginate.currentPage model.filteredCalls - 1) * Paginate.itemsPerPage model.filteredCalls // sizeAsInt
                    in
                    Paginate.changeItemsPerPage sizeAsInt model.filteredCalls
                        |> Paginate.goTo newPage
              }
            , Cmd.none
            )

matchCalls : Maybe String -> Maybe Regex -> DebugTrace ProcessedCall -> List ( Int, ProcessedCall )
matchCalls selectedFunction callFilter trace =
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
                                contains r c.searchString
                in
                filterByName && filterByCall
            )

