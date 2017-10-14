import Html exposing (..)
import String exposing (length)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main = 
  Html.beginnerProgram
  { view = view
  , model = model
  , update = update
  }
  

--== MODEL ==--

type alias Model =
  { number: Int
  , text: String
  }
  
model : Model
model =
  Model 2 ""
  
--== UPDATE ==--

type Msg
  = Add
  | Text String
  | AddFoo
  
update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model | number = model.number + 1 }
    Text text ->
      { model | text = text }
    AddFoo -> 
      { model | text = model.text ++ "--Foo--" }
    
--== VIEW ==--

view : Model -> Html Msg
view model =
  div []
    [ div    [] [ text "Test" ]
    , button [ onClick Add ] [ text "MORE" ]
    , div    [] [ text (toString model.number) ]
    , input  [ type_ "text", placeholder "Foo", onInput Text, value model.text ] []
    , div    [] [ text model.text ]
    , subview model
    ]

subview : Model -> Html Msg
subview model =
  div [] 
  [ button [ onClick Add ] [ text "another more" ] 
  , button [ onClick AddFoo ] [ text "add --Foo--" ]
  ] 