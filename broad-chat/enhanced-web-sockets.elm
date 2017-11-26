module Main exposing (..)

import Date exposing (..)
import Date.Format exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (static)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode exposing (..)
import List
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


type AppState
    = NameSelection
    | Connecting
    | Connected


type alias Naming =
    { name : String, hex : String }


type alias DisplayedMessage =
    { from : Maybe String
    , text : String
    , class : String
    , timestamp : Float
    , id : Int
    }


type alias Model =
    { input : String
    , naming : Naming
    , waiting : Bool
    , messages : List DisplayedMessage
    , appState : AppState
    , clientNames : List String
    }


startingState : Model
startingState =
    { input = ""
    , naming = Naming "" ""
    , waiting = False
    , messages = []
    , appState = NameSelection
    , clientNames = []
    }


type ServerMessageType
    = MessageBroadcast { from : String, content : String, time : String, messageId : Int } -- from server
    | UserList List String -- from server


type alias StandardMessage =
    { id : Int
    , messageType : String
    , timestamp : Float
    }


standardMessageDecoder : String -> Result String StandardMessage
standardMessageDecoder json =
    Decode.decodeString
        (Decode.map3 StandardMessage
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
        )
        json


type alias InfoMessage =
    { id : Int
    , messageType : String
    , timestamp : Float
    , text : String
    }


infoMessageDecoder : String -> Result String InfoMessage
infoMessageDecoder json =
    Decode.decodeString
        (Decode.map4 InfoMessage
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
            (Decode.field "text" Decode.string)
        )
        json


type alias ChatMessage =
    { id : Int
    , messageType : String
    , timestamp : Float
    , text : String
    , from : String
    }


chatMessageDecoder : String -> Result String ChatMessage
chatMessageDecoder json =
    Decode.decodeString
        (Decode.map5 ChatMessage
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
            (Decode.field "text" Decode.string)
            (Decode.field "from" Decode.string)
        )
        json


type alias ClientNames =
    { id : Int
    , messageType : String
    , timestamp : Float
    , clientNames : List String
    }


clientNamesDecoder : String -> Result String ClientNames
clientNamesDecoder json =
    Decode.decodeString
        (Decode.map4 ClientNames
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
            (Decode.field "clientNames" (Decode.list Decode.string))
        )
        json



--type alias ChatMessageFields =
--    {id: Int,type: String,timestamp:Int, text: String, from: String}
-- client messages


type alias NewUser =
    { newUser : String }


type alias MessageSent =
    { from : String, content : String }



--


init : ( Model, Cmd Msg )
init =
    ( startingState, Cmd.none )


type Msg
    = ChangeName String
    | Input String
    | Send
    | NewMessage String
    | InitializeConnection
    | SetColor String



--    | SendName String


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
            let
                mObj =
                    Json.Encode.object
                        [ ( "type", string "ChatMessageToServer" )
                        , ( "text", string model.input )
                        ]

                chatMessage =
                    encode 0 mObj
            in
            ( { model | input = "", waiting = True }, WebSocket.send url chatMessage )

        NewMessage str ->
            evaluateNewMessage model str

        ChangeName str ->
            let
                naming =
                    model.naming
            in
            ( { model | naming = { naming | name = str } }, Cmd.none )

        InitializeConnection ->
            ( { model | appState = Connecting }, Cmd.none )

        SetColor hex ->
            let
                naming =
                    model.naming
            in
            ( { model | naming = { naming | hex = hex } }, Cmd.none )



--        SendName message ->
--            ( { model | appState = Connecting }, WebSocket.send url message )


evaluateNewMessage : Model -> String -> ( Model, Cmd Msg )
evaluateNewMessage model json =
    let
        messageTypeString =
            Decode.decodeString (Decode.field "type" Decode.string) json
    in
    case messageTypeString of
        Ok "Info" ->
            handleInfoMessage model json

        Ok "ChatMessage" ->
            handleChatMessage model json

        Ok "RequestName" ->
            sendName model

        Ok "ClientNames" ->
            setClientNames model json

        _ ->
            justAddJson model json


sendName : Model -> ( Model, Cmd Msg )
sendName model =
    let
        mObj =
            Json.Encode.object
                [ ( "type", string "SendName" )
                , ( "name", string model.naming.name )
                ]

        message =
            encode 0 mObj
    in
    ( model, WebSocket.send url message )


setClientNames : Model -> String -> ( Model, Cmd Msg )
setClientNames model json =
    case clientNamesDecoder json of
        Ok clientNamesMessage ->
            ( { model
                | appState = Connected
                , clientNames = clientNamesMessage.clientNames
              }
            , Cmd.none
            )

        Err string ->
            let
                message =
                    { from = Nothing
                    , text = json ++ string ++ "ERRÖR"
                    , class = "standard"
                    , timestamp = 0
                    , id = 0
                    }
            in
            ( { model
                | waiting = False
                , appState = Connected
                , messages = message :: model.messages
              }
            , Cmd.none
            )


handleChatMessage : Model -> String -> ( Model, Cmd Msg )
handleChatMessage model json =
    case chatMessageDecoder json of
        Ok chatMessage ->
            let
                message =
                    { from = Just chatMessage.from
                    , text = chatMessage.text
                    , class = "chat-message"
                    , timestamp = chatMessage.timestamp
                    , id = chatMessage.id
                    }
            in
            ( { model
                | waiting = False
                , appState = Connected
                , messages = message :: model.messages
              }
            , Cmd.none
            )

        Err string ->
            justAddJson model json


handleInfoMessage : Model -> String -> ( Model, Cmd Msg )
handleInfoMessage model json =
    case infoMessageDecoder json of
        Ok infoMessage ->
            let
                message =
                    { from = Nothing
                    , text = infoMessage.text
                    , class = "info-message"
                    , timestamp = infoMessage.timestamp
                    , id = infoMessage.id
                    }
            in
            ( { model
                | waiting = False
                , appState = Connected
                , messages = message :: model.messages
              }
            , Cmd.none
            )

        Err string ->
            justAddJson model json



{--
            type alias DisplayedMessage =
                { from : Maybe String
                , text : String
                , class : String
                }
--}


justAddJson : Model -> String -> ( Model, Cmd Msg )
justAddJson model json =
    let
        message =
            { from = Nothing
            , text = json ++ " DEFAULT"
            , class = "standard"
            , timestamp = 0
            , id = 0
            }
    in
    ( { model
        | waiting = False
        , appState = Connected
        , messages = message :: model.messages
      }
    , Cmd.none
    )



{--
    ( { model
        | waiting = False
        , appState = Connected
        , messages = (toString messageType ++ " " ++ json) :: model.messages
      }
    , Cmd.none
    )
    --}
-- SUBSCRIPTIONS
-- for multiple subscriptions
-- see http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#batch


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appState of
        NameSelection ->
            Sub.none

        _ ->
            WebSocket.listen url NewMessage



-- VIEW


linkCss : String -> Html Never
linkCss route =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href route
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ static <| linkCss "node_modules/skeleton-css/css/skeleton.css"
        , static <| linkCss "node_modules/skeleton-css/css//normalize.css"
        , static <| linkCss "https://fonts.googleapis.com/css?family=Encode+Sans"
        , static <| linkCss "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
        , static <| linkCss "css/screen.css"
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
            , value model.naming.name
            ]
            []
        , ul [] <| displayColors model.naming.hex
        , button
            [ namingIsValid model.naming |> not |> disabled
            , onClick InitializeConnection
            ]
            [ text "jetzt Loslegen!" ]
        ]


displayColors : String -> List (Html Msg)
displayColors hex =
    List.map
        (\color ->
            let
                activeClass =
                    if Tuple.second color == hex then
                        "active"
                    else
                        ""
            in
            li
                [ style [ ( "background-color", Tuple.second color ) ]
                , onClick (SetColor (Tuple.second color))
                , class activeClass
                ]
                [ text <| Tuple.first color ]
        )
        colors


emptyAttribute =
    style []



-- TODO: define a naming format and hand over only model.name


namingIsValid : Naming -> Bool
namingIsValid naming =
    length naming.name >= 3


outerFrame : Model -> Html Msg
outerFrame model =
    div [ class "outer-frame" ]
        [ div [ class "head" ]
            [ h1 []
                [ i [ class "fa fa-comments-o" ] []
                , text " Broad-Chat | "
                , small [] [ text (" hi " ++ model.naming.name) ]
                ]
            ]
        , selectViewByAppState model
        ]


selectViewByAppState : Model -> Html Msg
selectViewByAppState model =
    case model.appState of
        NameSelection ->
            enterYourName model

        Connecting ->
            div [] [ text "loading" ]

        Connected ->
            mainStructure model


mainStructure : Model -> Html Msg
mainStructure model =
    div [ class "row" ]
        [ div [ class "four columns" ] [ activeUsers model ]
        , div [ class "eight columns" ] [ inputFrame model, chatHistory model ]
        ]


activeUsers : Model -> Html Msg
activeUsers model =
    div [ class "active-users" ]
        [ div [ class "active-users-heading" ] [ text <| toString (List.length model.clientNames) ++ " active users:" ]
        , div [ class "active-users-list" ]
            [ ul [] (listClientNames model.clientNames) ]
        ]


listClientNames : List String -> List (Html msg)
listClientNames clientNames =
    List.map (\name -> li [] [ text name ]) clientNames


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


chatEntry1 : DisplayedMessage -> ( String, Html Msg )
chatEntry1 message =
    let
        from =
            case message.from of
                Just from ->
                    from ++ ": "

                Nothing ->
                    ""

        time2 =
            toString message.timestamp

        time =
            format "%H:%M %d.%m.'%y" (fromTime message.timestamp)
    in
    ( toString message.id
    , li [ class "chat-entry" ]
        [ div [ class "chat-time grey" ] [ text time ]
        , div []
            [ span [ class "name" ] [ text from ]
            , text message.text
            ]
        ]
    )


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



-- DATA


url : String
url =
    "ws://localhost:8080"


colors : List ( String, String )
colors =
    [ ( "Greensea", "#16a085" )
    , ( "Nephritis", "#27ae60" )
    , ( "Belizehole", "#2980b9" )
    , ( "Wisteria", "#8e44ad" )
    , ( "Midnightblue", "#2c3e50" )
    , ( "Sunflower", "#f1c40f" )
    , ( "Orange", "#f39c12" )
    , ( "Pumpkin", "#d35400" )
    , ( "Pomegranate", "#c0392b" )
    , ( "Asbestor", "#7f8c8d" )
    ]



-- "wss://sdw.io:8020/chat"
