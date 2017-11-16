module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (reverse)
import WebSocket


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : String
    , waiting : Bool
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" False [ "hello", "wat?" ], Cmd.none )



-- UPDATE
-- debug comparing with the online example!


type Msg
    = Input String
    | Send
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, waiting, messages } =
    case msg of
        Input newInput ->
            ( Model newInput waiting messages, Cmd.none )

        Send ->
            ( Model "" True messages, WebSocket.send url input )

        NewMessage str ->
            ( Model input False (str :: messages), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen url NewMessage



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input, value model.input ] []
        , button [ onClick Send ] [ text "Send" ]
        , div [] (List.map viewMessage model.messages)
        , div []
            (if model.waiting then
                [ text "waiting" ]
             else
                [ text "" ]
            )
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



-- DATA


url : String
url =
    "ws://localhost:8080"
