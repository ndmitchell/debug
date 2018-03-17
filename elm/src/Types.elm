module Types exposing (..)
import Html exposing (Html)
import Navigation exposing (..)
import Paginate exposing (..)
import Regex exposing (..)
import Trace exposing (..)
import UrlParser exposing (..)

type Page
    = First
    | Last
    | Next
    | Prev
    | GoTo Int


type Msg
    = ChangeLocation Location
    | SelectFunction String
    | CallFilter String
    | CallPage Page
    | ChangeCallPageSize String


type alias ProcessedCall =
    { searchString : String, rendered : Html Msg, call : CallData }


type alias Model =
    { location : Location
    , selectedFunction : Maybe String
    , callFilter : Maybe Regex
    , filteredCalls : PaginatedList ( Int, ProcessedCall )
    , trace : Result String (DebugTrace ProcessedCall)
    }


type ParsedLocation
    = SelectCall Int

router : Location -> ParsedLocation
router =
    Maybe.withDefault (SelectCall 0) << parseHash (UrlParser.map SelectCall (UrlParser.s "call" </> int))
