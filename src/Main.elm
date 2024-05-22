module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder as Url


type alias Flags =
    ()


type alias PostcodeDetails =
    { parish : String
    , district : String
    , ward : String
    }


type PostcodeResponse
    = NotAsked
    | Loading
    | Success PostcodeDetails
    | Error


type alias Model =
    { postcodeInput : String
    , response : PostcodeResponse
    }


type Msg
    = PostcodeChanged String
    | LookupPostcode
    | PostcodeResult (Result Http.Error PostcodeDetails)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { postcodeInput = ""
      , response = NotAsked
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "panel" ]
        [ Html.input
            [ Attributes.type_ "text"
            , Events.onInput PostcodeChanged
            ]
            []
        , Html.button
            [ Events.onClick LookupPostcode ]
            [ Html.text "Lookup postcode" ]
        , Html.hr [] []
        , case model.response of
            NotAsked ->
                Html.text ""

            Loading ->
                Html.text "Loading..."

            Success details ->
                Html.div []
                    [ Html.div [] [ Html.text "Parish: ", Html.text details.parish ]
                    , Html.div [] [ Html.text "District: ", Html.text details.district ]
                    , Html.div [] [ Html.text "Ward: ", Html.text details.ward ]
                    ]

            Error ->
                Html.text "Something went wrong"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostcodeChanged newPostcode ->
            ( { model | postcodeInput = newPostcode }
            , Cmd.none
            )

        LookupPostcode ->
            ( { model | response = Loading }
            , Http.get
                { url =
                    Url.crossOrigin
                        "https://api.postcodes.io"
                        [ "postcodes", model.postcodeInput ]
                        []
                , expect =
                    Http.expectJson
                        PostcodeResult
                        decodePostcodeResponse
                }
            )

        PostcodeResult (Ok details) ->
            ( { model | response = Success details }, Cmd.none )

        PostcodeResult (Err a) ->
            ( model, Cmd.none )


decodePostcodeResponse : Decoder PostcodeDetails
decodePostcodeResponse =
    Decode.map3 PostcodeDetails
        (Decode.at [ "result", "parish" ] Decode.string)
        (Decode.at [ "result", "admin_district" ] Decode.string)
        (Decode.at [ "result", "admin_ward" ] Decode.string)
