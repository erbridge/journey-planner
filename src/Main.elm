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


type alias VagueLocation =
    { address : String
    }


type alias ExactLocation =
    { coordinates : Coordinates
    , address : String
    , arrivalTime : TimeConstraint
    , minStayDuration : Int
    , maxAwayDuration : Int
    }


type TimeConstraint
    = Anytime
    | VagueTime
        { start : Time.Posix
        , end : Time.Posix
        }
    | ExactTime Time.Posix


toVagueLocation : SearchSuggestion -> VagueLocation
toVagueLocation suggestion =
    { address = toAddress suggestion }


toAddress : SearchSuggestion -> String
toAddress suggestion =
    suggestion.featureName
        ++ ", "
        ++ suggestion.description


toExactLocation : VagueLocation -> Coordinates -> ExactLocation
toExactLocation location coordinates =
    { address = location.address
    , coordinates = coordinates
    , arrivalTime = Anytime
    , minStayDuration = 30
    , maxAwayDuration = 0
    }


type alias JourneyStep =
    { start : ExactLocation
    , end : ExactLocation
    , crowDistance : Float
    }


toCrowDistance : Coordinates -> Coordinates -> Float
toCrowDistance ( lon1, lat1 ) ( lon2, lat2 ) =
    (((1 - cos (degrees (lat2 - lat1)))
        + (cos (degrees lat1)
            * cos (degrees lat2)
            * (1 - cos (degrees (lon2 - lon1)))
          )
     )
        / 2
        |> sqrt
        |> asin
    )
        * 7918


type alias Model =
    { mapboxSessionToken : String
    , timezone : Time.Zone
    , searchState : SearchState
    , search : String
    , searchLocation : Maybe VagueLocation
    , locations : List ExactLocation
    , startCoordinates : Maybe Coordinates
    , endCoordinates : Maybe Coordinates
    , journeySteps : List JourneyStep
    , route : List ExactLocation
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
      , searchLocation = Nothing
      , locations = []
      , startCoordinates = Nothing
      , endCoordinates = Nothing
      , journeySteps = []
      , route = []
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
    | AdjustLocationAwayDuration Coordinates String
    | SetStartLocation Coordinates
    | UnsetStartLocation
    | SetEndLocation Coordinates
    | UnsetEndLocation
    | CalculateShortestRoute


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
                , searchLocation = Just (toVagueLocation suggestion)
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
                                , searchLocation = Nothing
                                , locations =
                                    case model.searchLocation of
                                        Just location ->
                                            let
                                                exactLocation =
                                                    toExactLocation location searchResult.coordinates
                                            in
                                            if List.member exactLocation model.locations then
                                                model.locations

                                            else
                                                exactLocation :: model.locations

                                        Nothing ->
                                            model.locations
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | searchState = Failure (Http.BadStatus 404) }
                            , Cmd.none
                            )

                Err error ->
                    ( { model | searchState = Failure error }
                    , Cmd.none
                    )

        AdjustLocationStayDuration coordinates minStayDuration ->
            let
                updateLocation : ExactLocation -> ExactLocation
                updateLocation location =
                    if location.coordinates == coordinates then
                        case toInt minStayDuration of
                            Just duration ->
                                { location | minStayDuration = duration }

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

        AdjustLocationAwayDuration coordinates maxAwayDuration ->
            let
                updateLocation : ExactLocation -> ExactLocation
                updateLocation location =
                    if location.coordinates == coordinates then
                        case toInt maxAwayDuration of
                            Just duration ->
                                { location | maxAwayDuration = duration }

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

        SetStartLocation coordinates ->
            ( { model | startCoordinates = Just coordinates }
            , Cmd.none
            )

        UnsetStartLocation ->
            ( { model | startCoordinates = Nothing }
            , Cmd.none
            )

        SetEndLocation coordinates ->
            ( { model | endCoordinates = Just coordinates }
            , Cmd.none
            )

        UnsetEndLocation ->
            ( { model | endCoordinates = Nothing }
            , Cmd.none
            )

        CalculateShortestRoute ->
            let
                journeySteps =
                    makeJourneySteps (List.reverse model.locations)

                route =
                    calculateShortestRoute model
            in
            ( { model
                | journeySteps = journeySteps
                , route = route
              }
            , Cmd.none
            )


makeJourneySteps : List ExactLocation -> List JourneyStep
makeJourneySteps locations =
    List.concatMap (makeJourneyStepsFrom locations) locations


makeJourneyStepsFrom : List ExactLocation -> ExactLocation -> List JourneyStep
makeJourneyStepsFrom locations start =
    let
        isNotStart : ExactLocation -> Bool
        isNotStart location =
            location /= start

        makeJourneyStep : ExactLocation -> JourneyStep
        makeJourneyStep end =
            { start = start
            , end = end
            , crowDistance = toCrowDistance end.coordinates start.coordinates
            }
    in
    List.filter isNotStart locations
        |> List.map makeJourneyStep


calculateShortestRoute : Model -> List ExactLocation
calculateShortestRoute model =
    let
        locations =
            case model.startCoordinates of
                Just startCoordinates ->
                    if Just startCoordinates == model.endCoordinates then
                        let
                            startLocation : Maybe ExactLocation
                            startLocation =
                                List.foldl
                                    (\loc found ->
                                        if found == Nothing && loc.coordinates == startCoordinates then
                                            Just loc

                                        else
                                            found
                                    )
                                    Nothing
                                    model.locations
                        in
                        case startLocation of
                            Just loc ->
                                loc :: model.locations

                            Nothing ->
                                model.locations

                    else
                        model.locations

                Nothing ->
                    model.locations

        permutations =
            makeAllPermutations [] [] locations

        validPermutations =
            permutations
                |> List.filter
                    (\permutation ->
                        case model.startCoordinates of
                            Just requiredStartCoordinates ->
                                case List.head permutation of
                                    Just start ->
                                        start.coordinates == requiredStartCoordinates

                                    Nothing ->
                                        True

                            Nothing ->
                                True
                    )
                |> List.filter
                    (\permutation ->
                        case model.endCoordinates of
                            Just requiredEndCoordinates ->
                                case List.head (List.reverse permutation) of
                                    Just end ->
                                        end.coordinates == requiredEndCoordinates

                                    Nothing ->
                                        True

                            Nothing ->
                                True
                    )
    in
    calculateShortestRoute_ ( [], 1 / 0 ) validPermutations


calculateShortestRoute_ : ( List ExactLocation, Float ) -> List (List ExactLocation) -> List ExactLocation
calculateShortestRoute_ ( bestRoute, bestRouteLength ) possibleRoutes =
    case List.head possibleRoutes of
        Just route ->
            let
                distanceToNext : Int -> ExactLocation -> Float
                distanceToNext index location =
                    case List.head (List.drop (index + 1) route) of
                        Just nextLocation ->
                            toCrowDistance location.coordinates nextLocation.coordinates

                        Nothing ->
                            0

                routeLength =
                    List.foldl (+) 0 (List.indexedMap distanceToNext route)

                ( newBestRoute, newBestRouteLength ) =
                    if routeLength < bestRouteLength then
                        ( route, routeLength )

                    else
                        ( bestRoute, bestRouteLength )
            in
            case List.tail possibleRoutes of
                Just routes ->
                    calculateShortestRoute_ ( newBestRoute, newBestRouteLength ) routes

                Nothing ->
                    newBestRoute

        Nothing ->
            bestRoute


makeAllPermutations : List (List a) -> List a -> List a -> List (List a)
makeAllPermutations permutations acc list =
    let
        without : a -> List a
        without element =
            let
                index =
                    list
                        |> List.indexedMap (\i e -> ( i, e ))
                        |> List.foldl
                            (\( i, e ) found ->
                                if found == Nothing && e == element then
                                    Just i

                                else
                                    found
                            )
                            Nothing
            in
            case index of
                Just idx ->
                    list
                        |> List.indexedMap (\i e -> ( i, e ))
                        |> List.filter (\( i, _ ) -> i /= idx)
                        |> List.map Tuple.second

                Nothing ->
                    list

        permute element =
            makeAllPermutations permutations (element :: acc) (without element)
    in
    if List.isEmpty list then
        acc :: permutations

    else
        permutations ++ List.concatMap permute list


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
                div []
                    [ ol []
                        (List.map
                            (viewLocation model.timezone ( model.startCoordinates, model.endCoordinates ))
                            (List.reverse model.locations)
                        )
                    , button
                        [ onClick CalculateShortestRoute
                        ]
                        [ text "Find journey"
                        ]
                    , div []
                        [ text
                            (String.join
                                " ➡️ "
                                (List.map .address model.route)
                            )
                        ]
                    ]
        ]


viewSearchSuggestion : SearchSuggestion -> Html Msg
viewSearchSuggestion suggestion =
    li []
        [ text (toAddress suggestion ++ " ")
        , button
            [ onClick (DoSearchRetrieve suggestion)
            ]
            [ text "<-"
            ]
        ]


viewLocation : Time.Zone -> ( Maybe Coordinates, Maybe Coordinates ) -> ExactLocation -> Html Msg
viewLocation timezone ( startCoordinates, endCoordinates ) location =
    li []
        [ text location.address

        -- , text
        --     (" - arriving by "
        --         ++ toString timezone location.arrivalTime
        --         ++ " and staying for at least "
        --     )
        -- , input
        --     [ type_ "number"
        --     , Html.Attributes.min "0"
        --     , style "maxWidth" "4em"
        --     , value (String.fromInt location.minStayDuration)
        --     , onInput (AdjustLocationStayDuration location.coordinates)
        --     ]
        --     []
        -- , text " minutes while not being away for more than "
        -- , input
        --     [ type_ "number"
        --     , Html.Attributes.min "0"
        --     , style "maxWidth" "4em"
        --     , value (String.fromInt location.maxAwayDuration)
        --     , onInput (AdjustLocationAwayDuration location.coordinates)
        --     ]
        --     []
        -- , text " minutes "
        , text " ( "
        , input
            [ type_ "checkbox"
            , checked
                (case startCoordinates of
                    Just coordinates ->
                        coordinates == location.coordinates

                    Nothing ->
                        False
                )
            , onCheck
                (let
                    setIfChecked checked =
                        if checked then
                            SetStartLocation location.coordinates

                        else
                            UnsetStartLocation
                 in
                 setIfChecked
                )
            ]
            []
        , text " start | "
        , input
            [ type_ "checkbox"
            , checked
                (case endCoordinates of
                    Just coordinates ->
                        coordinates == location.coordinates

                    Nothing ->
                        False
                )
            , onCheck
                (let
                    setIfChecked checked =
                        if checked then
                            SetEndLocation location.coordinates

                        else
                            UnsetEndLocation
                 in
                 setIfChecked
                )
            ]
            []
        , text " end ) "
        ]


viewJourneyStep : JourneyStep -> Html Msg
viewJourneyStep journeyStep =
    li []
        [ text
            (journeyStep.start.address
                ++ " ➡️ "
                ++ journeyStep.end.address
                ++ " ("
                ++ String.fromFloat journeyStep.crowDistance
                ++ " miles) "
            )
        ]


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
