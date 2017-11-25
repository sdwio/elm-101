module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (reverse)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { input, messages } =
            model
    in
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ outerFrame model ]



-- Typo at _M_odel


outerFrame : model -> Html Msg
outerFrame model =
    div [] [ mainStructure model ]


mainStructure : Model -> Html Msg
mainStructure model =
    div [] []
