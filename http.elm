-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main =
  Html.program 
    { init =init
    , update = update
    , subscriptions = subscriptions 
    , view = view
    }

---- MODEL

type alias Model = 
  { topic : String
  , gifUrl : String
  , errorMessage : String
  , url : String
  , loading: Bool
  }
  
init : (Model, Cmd Msg)
init =
  (Model 
    "blue" 
    "waiting.gif" 
    "" 
    "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="
    False
  , Cmd.none
  )
  
  
  
---- UPDATE

type Msg 
  = MorePlease
  | NewGif (Result Http.Error String)
  | TopicChange String
  | UrlChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      ( { model | loading = True, gifUrl = "" }, getRandomGif model.url model.topic )
      
    NewGif (Ok newUrl) ->
      ( { model | loading = False, errorMessage = "",  gifUrl = newUrl }, Cmd.none )
      
    NewGif (Err _) ->
      ( { model | loading = False, errorMessage = "We encoutered an error", gifUrl = "" }, Cmd.none )
      
    TopicChange topic ->
      ( { model | topic = topic }, Cmd.none )
      
    UrlChange url ->
      ( { model | url = url }, Cmd.none )
      
---- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



---- VIEW

view: Model -> Html Msg
view model =
  let 
    loadingIndicator = if model.loading then "LOADING" else ""
  in
    div []
    [ h2      [] [ text model.topic ]
    , button  [ onClick MorePlease ] [ text "Another one!" ]
    , input   [ type_ "text", placeholder "Foo", onInput TopicChange, value model.topic ] []
    , br      [] []
    , input   [ type_ "text", placeholder "Foo", onInput UrlChange, value model.url ] []
    , div     [] [ text model.errorMessage ]
    , div     [] [ text loadingIndicator ]
    , br      [] []  
    , img [ src model.gifUrl ] []
    ]



---- FUNCTIONS

getRandomGif : String -> String -> Cmd Msg
getRandomGif url topic =
  let
    fullUrl = 
      url ++ topic
    
    request =
      Http.get fullUrl decodeGifUrl
      
  in 
    Http.send NewGif request
    

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at [ "data", "image_url" ] Decode.string




































