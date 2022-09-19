module Main exposing (main)

import Browser
import Constants
import Html exposing (Html, div, form, input, text)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Random
import Uuid



-- MAIN


type alias Flags =
    { seed : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Search
    = Initial
    | Failure Http.Error
    | Loading
    | Success String


type alias Model =
    { addressSearch : String
    , search : Search
    , mapboxSessionToken : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( uuid, _ ) =
            Random.step Uuid.uuidGenerator (Random.initialSeed flags.seed)
    in
    ( { addressSearch = ""
      , search = Initial
      , mapboxSessionToken = Uuid.toString uuid
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeAddressSearch String
    | DoSearch
    | GotSearchSuggestions (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAddressSearch newSearch ->
            ( { model | addressSearch = newSearch }
            , Cmd.none
            )

        DoSearch ->
            ( { model | search = Loading }
            , getSearchSuggestions model
            )

        GotSearchSuggestions result ->
            case result of
                Ok suggestions ->
                    ( { model | search = Success suggestions }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | search = Failure error }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ form
            [ onSubmit DoSearch
            ]
            [ input
                [ type_ "text"
                , value model.addressSearch
                , onInput ChangeAddressSearch
                ]
                []
            , input
                [ type_ "submit"
                , value "Search"
                , disabled (String.length (String.trim model.addressSearch) == 0)
                ]
                []
            ]
        , div []
            [ case model.search of
                Initial ->
                    text ""

                Failure error ->
                    case error of
                        Http.BadUrl _ ->
                            text "Something went wrong. Please let the maintainer know."

                        Http.Timeout ->
                            text "The request timed out. Please try again."

                        Http.NetworkError ->
                            text "Check your internet connection."

                        Http.BadStatus status ->
                            text ("Error: " ++ String.fromInt status)

                        Http.BadBody body ->
                            text ("Error: " ++ body)

                Loading ->
                    text "Searching..."

                Success result ->
                    text result
            ]
        ]



--- HTTP


getSearchSuggestions : Model -> Cmd Msg
getSearchSuggestions model =
    Http.get
        { url =
            "https://api.mapbox.com/search/v1/suggest/"
                ++ model.addressSearch
                ++ "?access_token="
                ++ Constants.mapboxAccessToken
                ++ "&session_token="
                ++ model.mapboxSessionToken
                ++ "&language=en"
                ++ "&country=GB"
        , expect = Http.expectString GotSearchSuggestions
        }
