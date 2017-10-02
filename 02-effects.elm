import Html exposing (..)
import String exposing (length)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random

-- further readings:
-- https://stackoverflow.com/questions/37227421/how-do-i-add-a-second-die-to-this-elm-effects-example
-- https://www.reddit.com/r/elm/comments/4t61s6/is_there_a_repo_with_all_the_exercises_answers_of/


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
--== MODEL ==--

type alias Model = 
  { dieFace : Int
  , secondFace : Int
  }

init : (Model, Cmd Msg)
init = 
  (Model 1 2, Cmd.none)

--== UPDATE ==--

type Msg 
  = Roll
  | NewFace (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model
      , Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6))
      )
    
    NewFace (newFace, anotherFace) ->
      (Model newFace anotherFace, Cmd.none)

--== VIEW ==--

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.dieFace ++ " " ++ toString model.secondFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
    
--== SUBSCRIPTIONS ==--

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
    