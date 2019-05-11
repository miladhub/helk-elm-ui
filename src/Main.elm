module Main exposing (Model(..), Msg(..), getPerson, init, main, personDecoder, subscriptions, update, view, viewPerson)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPerson )


type Msg
    = MorePlease
    | GotPerson (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getPerson )

        GotPerson result ->
            case result of
                Ok person ->
                    ( Success person, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Helk" ]
        , viewPerson model
        ]


viewPerson : Model -> Html Msg
viewPerson model =
    case model of
        Failure ->
            div []
                [ text "I could not load the person for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success person ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , text person
                ]


getPerson : Cmd Msg
getPerson =
    Http.get
        { url = "http://localhost:9176/people/bar"
        , expect = Http.expectJson GotPerson personDecoder
        }


personDecoder : Decoder String
personDecoder =
    field "name" string
