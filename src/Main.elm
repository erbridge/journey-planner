module Main exposing (main)

import Browser
import Constants
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Json.Decode
import Random
import Url.Builder
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
    | Success SearchSuggestions


type alias SearchSuggestions =
    List SearchSuggestion


type alias SearchSuggestion =
    { featureName : String
    , description : String
    , id : String
    }


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
    | DoSearchSuggest
    | GotSearchSuggestions (Result Http.Error SearchSuggestions)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAddressSearch newSearch ->
            ( { model | addressSearch = newSearch }
            , Cmd.none
            )

        DoSearchSuggest ->
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
        [ lazy viewSearch model
        , lazy viewSearchResult model
        ]


viewSearch : Model -> Html Msg
viewSearch model =
    Html.form
        [ onSubmit DoSearchSuggest
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


viewSearchResult : Model -> Html Msg
viewSearchResult model =
    div []
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

            Success suggestions ->
                case List.head suggestions of
                    Just suggestion ->
                        div []
                            [ text
                                (suggestion.featureName
                                    ++ ", "
                                    ++ suggestion.description
                                )
                            , hr [] []
                            , text suggestion.id
                            ]

                    Nothing ->
                        text ""
        ]



--- HTTP


getSearchSuggestions : Model -> Cmd Msg
getSearchSuggestions model =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.mapbox.com"
                [ "search"
                , "v1"
                , "suggest"
                , model.addressSearch
                ]
                [ Url.Builder.string "access_token" Constants.mapboxAccessToken
                , Url.Builder.string "session_token" model.mapboxSessionToken
                , Url.Builder.string "language" "en"
                , Url.Builder.string "country" "GB"
                ]
        , expect = Http.expectJson GotSearchSuggestions searchSuggestionsDecoder
        }


searchSuggestionsDecoder : Json.Decode.Decoder SearchSuggestions
searchSuggestionsDecoder =
    Json.Decode.field "suggestions"
        (Json.Decode.list searchSuggestionDecoder)


searchSuggestionDecoder : Json.Decode.Decoder SearchSuggestion
searchSuggestionDecoder =
    Json.Decode.map3 SearchSuggestion
        (Json.Decode.field "feature_name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.at [ "action", "body", "id" ] Json.Decode.string)
