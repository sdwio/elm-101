module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import String exposing (length)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    {}


model : Model
model =
    Model



-- UPDATE


type Msg
    = Id


update : Msg -> Model -> Model
update msg model =
    case msg of
        Id ->
            model



-- VIEW
--


view : Model -> Html Msg
view model =
    let
        a =
            Decode.decodeString (Decode.field "x" Decode.string) """{"x":"foo"}"""

        b =
            case a of
                Ok int ->
                    int

                Err string ->
                    "#"
    in
    div [] [ text <| b ]
