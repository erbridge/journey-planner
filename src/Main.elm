module Main exposing (main)

import Browser
import Constants
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Json.Decode
import Json.Encode
import Random
import Task
import Time
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


type SearchState
    = Initial
    | Failure Http.Error
    | Loading
    | Suggested (List SearchSuggestion)
    | Success


type alias SearchSuggestion =
    { featureName : String
    , description : String
    , id : String
    }


type alias Coordinates =
    ( Float, Float )


type alias SearchResult =
    { coordinates : Coordinates
    }


type Location
    = VagueLocation
        { address : String
        }
    | ExactLocation
        { coordinates : Coordinates
        , address : String
        , arrivalTime : TimeConstraint
        , stayDuration : Int
        }


type TimeConstraint
    = Anytime
    | VagueTime
        { start : Time.Posix
        , end : Time.Posix
        }
    | ExactTime Time.Posix


toVagueLocation : SearchSuggestion -> Location
toVagueLocation suggestion =
    VagueLocation { address = toAddress suggestion }


toAddress : SearchSuggestion -> String
toAddress suggestion =
    suggestion.featureName
        ++ ", "
        ++ suggestion.description


toExactLocation : Location -> Coordinates -> Location
toExactLocation location coordinates =
    case location of
        ExactLocation loc ->
            ExactLocation
                { loc | coordinates = coordinates }

        VagueLocation loc ->
            ExactLocation
                { address = loc.address
                , coordinates = coordinates
                , arrivalTime = Anytime
                , stayDuration = 30
                }


type alias Model =
    { mapboxSessionToken : String
    , timezone : Time.Zone
    , searchState : SearchState
    , search : String
    , locations : List Location
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( uuid, _ ) =
            Random.step Uuid.uuidGenerator (Random.initialSeed flags.seed)
    in
    ( { mapboxSessionToken = Uuid.toString uuid
      , timezone = Time.utc
      , searchState = Initial
      , search = ""
      , locations = []
      }
    , Task.perform AdjustTimezone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimezone Time.Zone
    | ChangeAddressSearch String
    | DoSearchSuggest
    | GotSearchSuggestions (Result Http.Error (List SearchSuggestion))
    | DoSearchRetrieve SearchSuggestion
    | GotSearchResults (Result Http.Error (List SearchResult))
    | AdjustLocationStayDuration Coordinates String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimezone timezone ->
            ( { model | timezone = timezone }
            , Cmd.none
            )

        ChangeAddressSearch newSearch ->
            ( { model | search = newSearch }
            , Cmd.none
            )

        DoSearchSuggest ->
            ( { model | searchState = Loading }
            , getSearchSuggestions model
            )

        GotSearchSuggestions result ->
            case result of
                Ok suggestions ->
                    ( { model | searchState = Suggested suggestions }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | searchState = Failure error }
                    , Cmd.none
                    )

        DoSearchRetrieve suggestion ->
            ( { model
                | searchState = Loading
                , locations = toVagueLocation suggestion :: model.locations
              }
            , getSearchResults model suggestion
            )

        GotSearchResults result ->
            case result of
                Ok searchResults ->
                    case List.head searchResults of
                        Just searchResult ->
                            ( { model
                                | searchState = Success
                                , search = ""
                                , locations =
                                    case List.head model.locations of
                                        Just location ->
                                            toExactLocation location searchResult.coordinates :: Maybe.withDefault [] (List.tail model.locations)

                                        Nothing ->
                                            model.locations
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | searchState = Success }
                            , Cmd.none
                            )

                Err error ->
                    ( { model | searchState = Failure error }
                    , Cmd.none
                    )

        AdjustLocationStayDuration coordinates stayDuration ->
            let
                updateLocation : Location -> Location
                updateLocation location =
                    case location of
                        VagueLocation loc ->
                            location

                        ExactLocation loc ->
                            if loc.coordinates == coordinates then
                                case toInt stayDuration of
                                    Just duration ->
                                        ExactLocation { loc | stayDuration = duration }

                                    Nothing ->
                                        location

                            else
                                location

                newLocations =
                    List.map updateLocation model.locations
            in
            ( { model | locations = newLocations }
            , Cmd.none
            )


toInt : String -> Maybe Int
toInt str =
    if String.isEmpty str then
        Just 0

    else
        String.toInt
            (case String.uncons str of
                Just ( '0', strTail ) ->
                    strTail

                Just ( _, _ ) ->
                    str

                Nothing ->
                    str
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
        , lazy viewSearchOutcome model
        ]


viewSearch : Model -> Html Msg
viewSearch model =
    Html.form
        [ onSubmit DoSearchSuggest
        ]
        [ input
            [ type_ "text"
            , value model.search
            , onInput ChangeAddressSearch
            ]
            []
        , input
            [ type_ "submit"
            , value "Search"
            , disabled (String.length (String.trim model.search) == 0)
            ]
            []
        ]


viewSearchOutcome : Model -> Html Msg
viewSearchOutcome model =
    div []
        [ case model.searchState of
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

            Suggested suggestions ->
                ul []
                    (List.map viewSearchSuggestion suggestions)

            Success ->
                ol []
                    (List.map2 viewLocation
                        (List.repeat (List.length model.locations) model.timezone)
                        (List.reverse model.locations)
                    )
        ]


viewSearchSuggestion : SearchSuggestion -> Html Msg
viewSearchSuggestion suggestion =
    li []
        [ text (toAddress suggestion)
        , button
            [ onClick (DoSearchRetrieve suggestion)
            ]
            [ text "<-"
            ]
        ]


viewLocation : Time.Zone -> Location -> Html Msg
viewLocation timezone location =
    li []
        (case location of
            VagueLocation loc ->
                [ text loc.address
                ]

            ExactLocation loc ->
                [ text
                    (loc.address
                        ++ " [ "
                        ++ String.fromFloat (Tuple.first loc.coordinates)
                        ++ " , "
                        ++ String.fromFloat (Tuple.second loc.coordinates)
                        ++ " ] (arriving by "
                        ++ toString timezone loc.arrivalTime
                        ++ " and staying for "
                    )
                , input
                    [ type_ "number"
                    , Html.Attributes.min "0"
                    , value (String.fromInt loc.stayDuration)
                    , onInput (AdjustLocationStayDuration loc.coordinates)
                    ]
                    []
                , text " minutes) "
                ]
        )


toString : Time.Zone -> TimeConstraint -> String
toString zone timeConstraint =
    case timeConstraint of
        Anytime ->
            "whenever"

        VagueTime range ->
            "from "
                ++ String.fromInt (Time.toHour zone range.start)
                ++ ":"
                ++ String.fromInt (Time.toMinute zone range.start)
                ++ " to "
                ++ String.fromInt (Time.toHour zone range.end)
                ++ ":"
                ++ String.fromInt (Time.toMinute zone range.end)

        ExactTime time ->
            String.fromInt (Time.toHour zone time)
                ++ ":"
                ++ String.fromInt (Time.toMinute zone time)



--- HTTP


getSearchSuggestions : Model -> Cmd Msg
getSearchSuggestions model =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.mapbox.com"
                [ "search"
                , "v1"
                , "suggest"
                , model.search
                ]
                [ Url.Builder.string "access_token" Constants.mapboxAccessToken
                , Url.Builder.string "session_token" model.mapboxSessionToken
                , Url.Builder.string "language" "en"
                , Url.Builder.string "country" "GB"
                ]
        , expect = Http.expectJson GotSearchSuggestions searchSuggestionsDecoder
        }


getSearchResults : Model -> SearchSuggestion -> Cmd Msg
getSearchResults model suggestion =
    Http.post
        { url =
            Url.Builder.crossOrigin "https://api.mapbox.com"
                [ "search"
                , "v1"
                , "retrieve"
                ]
                [ Url.Builder.string "access_token" Constants.mapboxAccessToken
                , Url.Builder.string "session_token" model.mapboxSessionToken
                ]
        , body = Http.jsonBody (Json.Encode.object [ ( "id", Json.Encode.string suggestion.id ) ])
        , expect = Http.expectJson GotSearchResults searchResultsDecoder
        }


searchSuggestionsDecoder : Json.Decode.Decoder (List SearchSuggestion)
searchSuggestionsDecoder =
    Json.Decode.field "suggestions"
        (Json.Decode.list searchSuggestionDecoder)


searchSuggestionDecoder : Json.Decode.Decoder SearchSuggestion
searchSuggestionDecoder =
    Json.Decode.map3 SearchSuggestion
        (Json.Decode.field "feature_name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.at [ "action", "body", "id" ] Json.Decode.string)


searchResultsDecoder : Json.Decode.Decoder (List SearchResult)
searchResultsDecoder =
    Json.Decode.field "features"
        (Json.Decode.list searchResultDecoder)


searchResultDecoder : Json.Decode.Decoder SearchResult
searchResultDecoder =
    Json.Decode.map SearchResult
        (Json.Decode.at [ "geometry", "coordinates" ]
            (Json.Decode.map2 Tuple.pair
                (Json.Decode.index 0 Json.Decode.float)
                (Json.Decode.index 1 Json.Decode.float)
            )
        )
