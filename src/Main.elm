module Main exposing (Model(..), Msg(..), getPeople, init, main, personDecoder, subscriptions, update, view, viewPeople)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Person =
    { name : String
    , age : Int
    }


type Model
    = Failure
    | Loading
    | Success (List Person)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPeople )


type Msg
    = Reload
    | GotPeople (Result Http.Error (List Person))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reload ->
            ( Loading, getPeople )

        GotPeople result ->
            case result of
                Ok people ->
                    ( Success people, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Helk" ]
        , viewPeople model
        ]


viewPeople : Model -> Html Msg
viewPeople model =
    case model of
        Failure ->
            div []
                [ text "I could not load people for some reason. "
                , button [ onClick Reload ] [ text "Reload" ]
                ]

        Loading ->
            text "Loading..."

        Success people ->
            div []
                [ button [ onClick Reload, style "display" "block" ] [ text "Reload" ]
                , showPeople people
                ]


showPeople : List Person -> Html a
showPeople people =
    ul []
        (List.map
            showPerson
            people
        )


showPerson : Person -> Html a
showPerson person =
    li [] [ text person.name ]


getPeople : Cmd Msg
getPeople =
    Http.get
        { url = "http://localhost:9176/people"
        , expect = Http.expectJson GotPeople (list personDecoder)
        }


personDecoder : Decoder Person
personDecoder =
    map2 Person
        (field "name" string)
        (field "age" int)
