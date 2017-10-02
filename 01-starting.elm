import Html exposing (..)
import String exposing (length)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , errorMessage : String
  }


model : Model
model =
  Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Validate


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }
      
    Validate ->
      { model | errorMessage 
        = if 3 > 4 then "bar" else "foo"
      }



-- VIEW

-- 


view : Model -> Html Msg
view model =
  div []
    [ input  [ type_ "text", placeholder "Name", onInput Name ] []
    , input  [ type_ "password", placeholder "Password", onInput Password ] []
    , input  [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [ onClick Validate ] [text "Submit"]
    , div    [] [ text model.errorMessage ]
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password /= model.passwordAgain then
        ("red", "Passwords do not match!")
      else if (length model.password) < 8 then
        ("red", "Passwords is too short!") 
      else
        ("green", "OK") 
        
  in
  div [ style [("color", color)] ] [ text message ]
