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


--- Continue:
-- add browser notifications
-- remove name
-- refactor into several files
-- add an id to Naming to let the right face pulse on input
-- refactor userId to clientId
-- collect message types
----------
-- Done:
-- style registration


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


type alias DisplayedMessage =
    { from : Maybe String
    , naming : Maybe Naming
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
    , clientNaming : List Naming
    }


type alias Naming =
    { pulseClass : String
    , name : String
    , hex : String
    , face : String
    , id : Int
    }


startingState : Model
startingState =
    { input = ""
    , naming = Naming "pulse-1" "" "" "" -1
    , waiting = False
    , messages = []
    , appState = NameSelection
    , clientNames = []
    , clientNaming = []
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
    , naming : Naming
    }


chatMessageDecoder : String -> Result String ChatMessage
chatMessageDecoder json =
    Decode.decodeString
        (Decode.map6 ChatMessage
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
            (Decode.field "text" Decode.string)
            (Decode.field "from" Decode.string)
            (Decode.field "naming" namingDecoder)
        )
        json


type alias ClientNames =
    { id : Int
    , messageType : String
    , timestamp : Float
    , clientNames : List String
    }


type alias ClientNaming =
    { id : Int
    , messageType : String
    , timestamp : Float
    , clientNamings : List Naming
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


namingDecoder =
    Decode.map4 (Naming "pulse-1")
        (Decode.field "name" Decode.string)
        (Decode.field "hex" Decode.string)
        (Decode.field "face" Decode.string)
        (Decode.field "id" Decode.int)


clientNamingDecoder : String -> Result String ClientNaming
clientNamingDecoder json =
    Decode.decodeString
        (Decode.map4 ClientNaming
            (Decode.field "id" Decode.int)
            (Decode.field "type" Decode.string)
            (Decode.field "timestamp" Decode.float)
            (Decode.field "clientNamings" (Decode.list namingDecoder))
        )
        json


requestNameDecoder : String -> Result String Int
requestNameDecoder json =
    Decode.decodeString (Decode.field "userId" Decode.int) json



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
    | CheckForEnter Int
    | Input String
    | Send
    | NewMessage String
    | InitializeConnection
    | SetColor String
    | SetFace String
    | None



--    | SendName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { input, messages } =
            model
    in
    case msg of
        CheckForEnter key ->
            if key == 13 then
                sendMessage model
            else
                ( model, Cmd.none )

        Input newInput ->
            ( { model | input = newInput }
            , WebSocket.send url <| encodeTypingMessage model.naming
            )

        Send ->
            sendMessage model

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

        SetFace face ->
            let
                naming =
                    model.naming
            in
            ( { model | naming = { naming | face = face } }, Cmd.none )

        None ->
            ( model, Cmd.none )



--        SendName message ->
--            ( { model | appState = Connecting }, WebSocket.send url message )


encodeTypingMessage : Naming -> String
encodeTypingMessage { name, hex, face, id } =
    encode 0 <|
        Json.Encode.object
            [ ( "type", string "Typing" )
            , ( "name", string name )
            , ( "hex", string hex )
            , ( "face", string face )
            , ( "id", int id )
            ]


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
            sendName model json

        Ok "ClientNames" ->
            setClientNames model json

        Ok "ClientNamings" ->
            setClientNamings model json

        Ok "Typing" ->
            pulseIcon model json

        _ ->
            justAddJson model json


pulseIcon : Model -> String -> ( Model, Cmd Msg )
pulseIcon model json =
    let
        pulsingId =
            getUserId json

        clientNaming =
            List.map
                (\naming ->
                    if naming.id == pulsingId then
                        let
                            pulseClass =
                                if naming.pulseClass == "pulse-1" then
                                    "pulse-2"
                                else
                                    "pulse-1"
                        in
                        { naming | pulseClass = pulseClass }
                    else
                        naming
                )
                model.clientNaming
    in
    ( { model | clientNaming = clientNaming }, Cmd.none )


sendName : Model -> String -> ( Model, Cmd Msg )
sendName model json =
    let
        message =
            encode 0 <|
                Json.Encode.object
                    [ ( "type", string "SendName" )
                    , ( "name", string model.naming.name )
                    , ( "hex", string model.naming.hex )
                    , ( "face", string model.naming.face )
                    ]

        naming =
            model.naming

        newNaming =
            { naming | id = getUserId json }
    in
    ( { model | naming = newNaming }, WebSocket.send url message )


getUserId : String -> Int
getUserId json =
    case requestNameDecoder json of
        Ok id ->
            id

        Err string ->
            -2


sendMessage : Model -> ( Model, Cmd Msg )
sendMessage model =
    if model.input == "" then
        ( model, Cmd.none )
    else
        let
            mObj =
                Json.Encode.object
                    [ ( "type", string "ChatMessageToServer" )
                    , ( "text", string model.input )
                    ]

            chatMessage =
                encode 0 mObj
        in
        ( { model | input = "", waiting = True }
        , Cmd.batch
            [ WebSocket.send url chatMessage
            , WebSocket.send url <| encodeTypingMessage model.naming
            ]
        )


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
                    , naming = Nothing
                    , text = json ++ string ++ "ERROR"
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


setClientNamings : Model -> String -> ( Model, Cmd Msg )
setClientNamings model json =
    case clientNamingDecoder json of
        Ok clientNamingsMessage ->
            ( { model
                | appState = Connected
                , clientNaming = clientNamingsMessage.clientNamings
              }
            , Cmd.none
            )

        Err string ->
            let
                message =
                    { from = Nothing
                    , naming = Nothing
                    , text = json ++ string ++ "ERROR"
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
                    , naming = Just chatMessage.naming
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
            justAddJson model string


handleInfoMessage : Model -> String -> ( Model, Cmd Msg )
handleInfoMessage model json =
    case infoMessageDecoder json of
        Ok infoMessage ->
            let
                message =
                    { from = Nothing
                    , naming = Nothing
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


justAddJson : Model -> String -> ( Model, Cmd Msg )
justAddJson model json =
    let
        message =
            { from = Nothing
            , naming = Nothing
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


enterYourName : Naming -> Html Msg
enterYourName naming =
    let
        hex =
            getSelectedOrNeutralColor naming.hex

        ( helloClass, nameClass ) =
            if String.length naming.name < 3 then
                ( "show", "fade-out" )
            else
                ( "fade-out", "show" )
    in
    div [ class "name-selection" ]
        [ div [ class "name-container" ]
            [ div [ class <| "name-headline " ++ helloClass ]
                [ text "What is your name?" ]
            , div [ class <| "name-headline " ++ nameClass ]
                [ text <| "Hello "
                , span [ style [ ( "color", hex ), ( "transition", "color 1s" ) ] ]
                    [ text naming.name ]
                , text ", nice to see you :D"
                ]
            ]
        , div [ class "input-frame" ]
            [ div [ class "input-element" ]
                [ label [ for "main-input" ]
                    [ i [ class "fa fa-pencil fa-flip-horizontal" ] [] ]
                , input
                    [ class "main-input"
                    , id "main-input"
                    , type_ "text"
                    , name "main-input"
                    , onInput ChangeName
                    , value naming.name
                    ]
                    []
                ]
            ]
        , colorFrame naming
        , faceFrame naming
        , button
            [ class "start-it"
            , style [ ( "background-color", naming.hex ), ( "transition", "background-color 1s, opacity 0.5s    " ) ]
            , namingIsValid naming |> not |> disabled
            , onClick InitializeConnection
            ]
            [ text "Start it!" ]
        ]


colorFrame : Naming -> Html Msg
colorFrame naming =
    let
        ( classes, update ) =
            if nameIsValid naming.name then
                ( "show", Just (\color -> onClick (SetColor color)) )
            else
                ( "fade", Nothing )
    in
    div [ class classes ]
        [ div [ class "centered" ] [ text "select a color" ]
        , div [ class "color-selection" ] <| displayColors naming.hex update
        ]


faceFrame : Naming -> Html Msg
faceFrame naming =
    let
        ( classes, update ) =
            if hexIsValid naming.hex && nameIsValid naming.name then
                ( "show", Just <| \face -> onClick <| SetFace face )
            else
                ( "fade", Nothing )
    in
    div
        [ class classes ]
        [ div [ class "centered" ] [ text "select a face" ]
        , div [ class "square-face" ] <| faceSelection naming update
        ]



--displayColors : String -> List (Html Msg)


displayColors hex update =
    List.map
        (\color ->
            let
                activeClass =
                    if Tuple.second color == hex then
                        "active"
                    else
                        ""

                onClick =
                    case update of
                        Just fn ->
                            [ fn <| Tuple.second color ]

                        Nothing ->
                            []
            in
            div
                ([ style [ ( "background-color", Tuple.second color ) ]
                 , class ("color " ++ activeClass)
                 ]
                    ++ onClick
                )
                [ text "" ]
        )
        colors


emptyAttribute =
    style []



-- TODO: define a naming format and hand over only model.name


namingIsValid : Naming -> Bool
namingIsValid { name, hex, face } =
    nameIsValid name && faceIsValid face && hexIsValid hex


nameIsValid : String -> Bool
nameIsValid name =
    length name >= 3


faceIsValid : String -> Bool
faceIsValid face =
    face /= ""


hexIsValid : String -> Bool
hexIsValid hex =
    hex /= ""


outerFrame : Model -> Html Msg
outerFrame model =
    let
        hex =
            getSelectedOrNeutralColor model.naming.hex
    in
    div [ class "outer-frame" ]
        [ div [ class "head", style [ ( "background-color", hex ) ] ]
            [ h1 []
                [ i [ class "fa fa-comments-o" ] []
                , text " Broad-Chat"
                ]
            ]
        , selectViewByAppState model
        ]


selectViewByAppState : Model -> Html Msg
selectViewByAppState model =
    case model.appState of
        NameSelection ->
            enterYourName model.naming

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
        [ div [ class "active-users-heading" ]
            [ text <|
                toString (List.length model.clientNaming)
                    ++ " active users:"
            ]
        , div [ class "active-users-list" ]
            [ ul [] (listClientNames model.clientNaming) ]
        ]


listClientNames : List Naming -> List (Html msg)
listClientNames clientNaming =
    List.map
        (\{ pulseClass, hex, name, face } ->
            li
                [ style [ ( "color", hex ) ], class "listed-name" ]
                [ span
                    [ class "square-face-list-icon" ]
                    [ span
                        [ class <| "square-face-icon " ++ pulseClass ]
                        [ text face ]
                    ]
                , span [ class "name" ] [ text <| " " ++ name ]
                ]
        )
        clientNaming


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
                , onKeyDown CheckForEnter
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
        [ Keyed.ul [] (List.map chatEntry model.messages) ]


chatEntry : DisplayedMessage -> ( String, Html Msg )
chatEntry message =
    let
        ( showNaming, name, hex, face ) =
            case message.naming of
                Just naming ->
                    ( True, naming.name ++ ": ", naming.hex, naming.face )

                Nothing ->
                    ( False, "", "", "" )

        time =
            format "%H:%M %d.%m.'%y" (fromTime message.timestamp)
    in
    ( toString message.id
    , li [ class "chat-entry" ]
        [ div [ class "chat-time grey" ] [ text time ]
        , div [ class "chat-content" ]
            [ span
                -- TODO add pulse-1 with appropriate styling (?)
                [ class "square-face-icon"
                , style [ ( "color", hex ) ]
                ]
                [ text face ]
            , div []
                [ span [ class "name", style [ ( "color", hex ) ] ] [ text name ]
                , text message.text
                ]
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


neutralColor =
    "#34495e"


getSelectedOrNeutralColor : String -> String
getSelectedOrNeutralColor hex =
    if hex /= "" then
        hex
    else
        neutralColor


colors : List ( String, String )
colors =
    [ ( "Midnightblue", "#2c3e50" )
    , ( "Wetapshalt", "#34495e" )
    , ( "Belizehole", "#2980b9" )
    , ( "Peterriver", "#3498db" )
    , ( "Greensea", "#16a085" )
    , ( "Nephritis", "#27ae60" )
    , ( "Sunflower", "#f1c40f" )

    --, ( "Sunflower", "#dbb20d" ) -- darkened
    --, ( "Orange", "#f39c12" ) -- darkened
    , ( "Orange", "#dd8d0d" )
    , ( "Pumpkin", "#d35400" )
    , ( "Pomegranate", "#c0392b" )
    , ( "Wisteria", "#8e44ad" )
    , ( "Amethyst", "#9b59b6" )

    --, ( "Alizarin", "#e74c3c" )
    --, ( "Asbestor", "#7f8c8d" )
    ]


faces =
    [ "d", "f", "v", "x", "i", "b", "r", "c", "h", "u", "k", "n", "l", "m", "p" ]



--faceSelection : Naming -> List (Html Msg)


faceSelection naming update =
    List.map
        (\face ->
            let
                activeStyle =
                    if face == naming.face then
                        [ ( "color", naming.hex ) ]
                    else
                        []

                onClick =
                    case update of
                        Nothing ->
                            []

                        Just fn ->
                            [ fn face ]
            in
            span
                ([ class "selectable-face"
                 , style activeStyle
                 ]
                    ++ onClick
                )
                [ text face ]
        )
        faces


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)
