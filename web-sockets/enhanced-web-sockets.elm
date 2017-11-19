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
    , name : String
    , waiting : Bool
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" False [], Cmd.none )



-- UPDATE
-- debug comparing with the online example!


type Msg
    = Input String
    | Send
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { input, messages } =
            model
    in
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model | input = "", waiting = True }, WebSocket.send url input )

        NewMessage str ->
            ( { model | waiting = False, messages = str :: messages }, Cmd.none )



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



-- "wss://sdw.io:8020/chat"
