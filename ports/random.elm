port module Random exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { howManyNumbers : Int
    , randomNumbers : List Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | Reduce
    | RequestNumbers
    | GetNumbers (List Float)


port requestNumbers : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( { model | howManyNumbers = model.howManyNumbers + 1 }, Cmd.none )

        Reduce ->
            ( { model | howManyNumbers = model.howManyNumbers - 1 }, Cmd.none )

        RequestNumbers ->
            ( model, requestNumbers model.howManyNumbers )

        GetNumbers newNumbers ->
            ( { model
                | randomNumbers = newNumbers
                , howManyNumbers = model.howManyNumbers + 1
              }
            , Cmd.none
            )



{--



--}
-- SUBSCRIPTIONS


port getNumbers : (List Float -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getNumbers GetNumbers



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reduce ] [ text "-1" ]
        , button [ onClick Add ] [ text "+1" ]
        , div [] [ text <| toString model.howManyNumbers ]
        , button [ onClick RequestNumbers ] [ text "Get Numbers!" ]
        , div [] [ text (List.foldl (\n acc -> acc ++ " " ++ toString n) "" model.randomNumbers) ]

        --, div [] [ text (List.foldr (\ n acc -> acc ++ " " ++ (toString n)) "" model.randomNumbers) ]
        ]
