module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (static)
import Html.Keyed as Keyed
import List exposing (reverse)
import String exposing (length)
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
    = ChangeName String
    | Input String
    | Send
    | NewMessage String
    | AnotherInt Int


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

        ChangeName str ->
            ( { model | name = str }, Cmd.none )

        AnotherInt int ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen url NewMessage



-- VIEW


css : String -> Html Msg
css route =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href route
        ]
        []


type Mix
    = String
    | Int


foo0 : Int -> Mix
foo0 int =
    if int > 9 then
        String
    else
        Int


foo4 : Mix -> Int
foo4 mix =
    case mix of
        Int ->
            1

        String ->
            0


type Mix2
    = AString String
    | AnInt Int


type Fruit
    = Apple
    | Banana
    | Fish


foo : Int -> Fruit
foo int =
    if int > 3 then
        Apple
    else
        Banana


foo2 : Mix2 -> Int
foo2 mix =
    case mix of
        AnInt int ->
            2 * int

        AString string ->
            length string


foo3 : Int -> Msg
foo3 int =
    if int > 4 then
        AnotherInt (2 * int)
    else
        Send


xview : Model -> Html Msg
xview model =
    div [] [ text "Never?", static never ]


never : Html Never
never =
    div [] [ text "abc" ]


view : Model -> Html Msg
view model =
    div []
        [ css "css/Skeleton-2.0.4/css/skeleton.css"
        , css "css/normalize.css"
        , css "https://fonts.googleapis.com/css?family=Encode+Sans"
        , css "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
        , css "css/screen.css"
        , outerFrame model
        , div []
            (if model.waiting then
                [ text "waiting" ]
             else
                [ text "" ]
            )
        ]


enterYourName : Model -> Html Msg
enterYourName model =
    div []
        [ input
            [ type_ "text"
            , onInput ChangeName
            , value model.name
            ]
            []
        ]


outerFrame : Model -> Html Msg
outerFrame model =
    div [ class "outer-frame" ]
        [ div [ class "head" ]
            [ h1 []
                [ i [ class "fa fa-comments-o" ] []
                , text " Broad-Chat | "
                , small [] [ text (" hi " ++ model.name) ]
                ]
            ]
        , enterYourName model
        , mainStructure model
        ]


mainStructure : Model -> Html Msg
mainStructure model =
    div [ class "row" ]
        [ div [ class "four columns" ] [ activeUsers model ]
        , div [ class "eight columns" ] [ inputFrame model, chatHistory model ]
        ]


activeUsers : Model -> Html Msg
activeUsers model =
    div [ class "active-users" ]
        [ div [ class "active-users-heading" ] [ text "2 active users:" ]
        , div [ class "active-users-list" ]
            [ ul [] [ li [] [ text "Sam" ], li [] [ text "Mary" ] ] ]
        ]


inputFrame : Model -> Html Msg
inputFrame model =
    div [ class "input-frame" ]
        [ div [ class "input-element" ]
            [ label [ for "main-input" ]
                [ i [ class "fa fa-pencil fa-flip-horizontal" ] [] ]
            , input
                [ class "main-input"
                , id "main-input"
                , type_ "text"
                , name "main-input"
                , onInput Input
                , value model.input
                ]
                []
            ]
        , div [ class "send-button", onClick Send ]
            [ i [ class "fa fa-paper-plane-o" ] [] ]
        ]


chatHistory : Model -> Html Msg
chatHistory model =
    div [ class "chat-history" ]
        [ Keyed.ul [] (List.map chatEntry1 model.messages) ]


chatEntry : String -> Html Msg
chatEntry message =
    li [ class "chat-entry" ]
        [ div [ class "chat-time grey" ] [ text "16.11.'17 12:43" ]
        , div []
            [ span [ class "name" ] [ text "Sam: " ]
            , text message
            ]
        ]


chatEntry1 : String -> ( String, Html Msg )
chatEntry1 message =
    ( message
    , li [ class "chat-entry" ]
        [ div [ class "chat-time grey" ] [ text "16.11.'17 12:43" ]
        , div []
            [ span [ class "name" ] [ text "Sam: " ]
            , text message
            ]
        ]
    )



{--<div class="eight columns">
        <div class="input-frame">
--            <div class="input-element">
--                <label for="main-input">
--                    <i class="fa fa-pencil fa-flip-horizontal"></i>
--                </label>
--                <input class="main-input" id="main-input" type="text" name="main-input" value="" placeholer="">
--            </div>
--            <div class="send-button">
--                <i class="fa fa-paper-plane-o"></i> </div>
--            </div>
--            <div class="chat-history">
--}


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



-- DATA


url : String
url =
    "ws://localhost:8080"



-- "wss://sdw.io:8020/chat"
