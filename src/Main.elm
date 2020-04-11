module Main exposing (Model(..), Msg(..), getPeople, init, main, personDecoder, subscriptions, update, view, viewPeople)

import Browser exposing (element)
import Html exposing (Html, li, text, button, ul, div, h2)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http exposing (Error)
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
    | Delete String
    | Deleted (Result Http.Error ())


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

        Delete name ->
            ( Loading, deletePerson name )

        Deleted _ ->
            ( Loading, getPeople )


getPeople : Cmd Msg
getPeople =
    Http.get
        { url = "http://localhost:9176/people"
        , expect = Http.expectJson GotPeople (list personDecoder)
        }


deletePerson : String -> Cmd Msg
deletePerson name =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:9176/people/" ++ name
        , body = Http.emptyBody
        , expect = Http.expectWhatever Deleted
        , timeout = Nothing
        , tracker = Nothing
        }


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


showPeople : List Person -> Html Msg
showPeople people =
    ul []
        (List.map
            showPerson
            people
        )


showPerson : Person -> Html Msg
showPerson person =
    li []
        [ text (person.name ++ ", age " ++ String.fromInt person.age)
        , button [ onClick (Delete person.name), style "display" "block" ] [ text "Delete" ]
        ]


personDecoder : Decoder Person
personDecoder =
    map2 Person
        (field "name" string)
        (field "age" int)
