module Main exposing (..)

import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import String


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { counter : Int
    , num : String
    }


model : Model
model =
    { counter = 0
    , num = "1"
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + strToInt model.num }

        Decrement ->
            { model | counter = model.counter - strToInt model.num }

        Reset ->
            { model | counter = 0 }

        Change newNum ->
            { model | num = newNum }


strToInt : String -> Int
strToInt n =
    Result.withDefault 1 (String.toInt n)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "number", placeholder "1", onInput Change ] []
        , br [] []
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , br [] []
        , button [ onClick Reset ] [ text "reset" ]
        ]
