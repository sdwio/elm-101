import Html exposing (..)
import String exposing (length)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main = 
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
  
--== INIT ==--

type alias Model =
  { aNumber : Int
  }

init : (Model, Cmd Msg)
init =
  (Model 99, Cmd.none)

--== UPDATE ==--

type Msg 
  = Roll
  | NewFace Int
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))
    NewFace newFace ->
      ({ model | aNumber = model.aNumber + newFace }, Cmd.none)
    

--== SUBSCRIPTIONS ==--

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--== VIEW ==--

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.aNumber) ]
    , button [ onClick Roll ] [ text "NUM!" ]
    ]











