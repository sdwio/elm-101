import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode


main = Html.program
  { init = initialize
  , view = mainView
  , update = updateElements
  , subscriptions = subscribedEvents
  }
  
---- MODEL

type alias Model =
  { baseUrl: String
  , topic: String
  , imageUrl: String
  , anInt: Int
  }

initialize : (Model, Cmd Msg)
initialize =
  ( Model 
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="
    "women"
    ""
    0
  , Cmd.none
  )
  
---- View

mainView : Model -> Html Msg
mainView model =
  div []
  [ h1      [] [ text "replaying the http example" ]
  , div     [] [ text ( "topic " ++ model.topic ) ]
  , button  [ onClick RequestImageUrl ] [ text "A NEW ONE PLS!" ]
  , img     [ src model.imageUrl ] []
  , div     [] [ text "this part is merely to repeat the very basics" ]
  , button  [ onClick Increase ] [ text ("INCREASE THIS NUMBER: " ++ (toString  model.anInt)) ]
  ]
  
---- SUBSCRIPTIONS

subscribedEvents : Model -> Sub Msg
subscribedEvents model =
  Sub.none

---- UPDATE

type Msg 
  = Increase
  | RequestImageUrl
  | ImageUrlLoaded (Result Http.Error String)
  -- TODO add the change topic update
  | ChangeTopic String

updateElements : Msg -> Model -> (Model, Cmd Msg)
updateElements msg model = 
  case msg of
    Increase ->
      ({ model | anInt = model.anInt + 1 }, Cmd.none)
      
    RequestImageUrl ->
      (model, getImageUrl model.baseUrl model.topic)
      
    ImageUrlLoaded (Ok imageUrl) ->
      ( { model | imageUrl = imageUrl }, Cmd.none)
      
    ImageUrlLoaded (Err _) ->
      (model, Cmd.none)
      
    ChangeTopic

---- FUNCTIONS

getImageUrl : String -> String -> Cmd Msg
getImageUrl baseUrl topic =
  Http.send ImageUrlLoaded ( Http.get (baseUrl ++ topic) (Json.Decode.at ["data", "image_original_url"] Json.Decode.string))
  


