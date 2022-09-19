module Main exposing (main)

import Browser
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Location Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Location =
    { addressSearch : String }


init : Location
init =
    { addressSearch = "" }



-- UPDATE


type Msg
    = ChangeAddressSearch String


update : Msg -> Location -> Location
update msg location =
    case msg of
        ChangeAddressSearch newSearch ->
            { location | addressSearch = newSearch }



-- VIEW


view : Location -> Html Msg
view location =
    div []
        [ input [ type_ "text", value location.addressSearch, onInput ChangeAddressSearch ] []
        , span [] [ text location.addressSearch ]
        ]
