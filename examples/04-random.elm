module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFaces : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( Model ( 1, 1 ), Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)) )

        NewFace newFace ->
            ( Model newFace, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( dieFace1, dieFace2 ) =
            model.dieFaces
    in
    div [ wrapperStyles ]
        [ img [ src ("../static/die-" ++ toString dieFace1 ++ ".png") ] []
        , img [ src ("../static/die-" ++ toString dieFace2 ++ ".png") ] []
        , button [ buttonStyles, onClick Roll ] [ text "Roll" ]
        ]


wrapperStyles =
    style
        [ ( "background", "#0e0e0e" )
        , ( "width", "100vw" )
        , ( "height", "100vh" )
        , ( "text-align", "center" )
        , ( "padding", "15%" )
        , ( "box-sizing", "border-box" )
        , ( "font", "1em/.8em cursive, sans-serif" )
        ]


buttonStyles =
    style
        [ ( "width", "128px" )
        , ( "display", "block" )
        , ( "padding", "15px 30px" )
        , ( "margin", "30px auto" )
        , ( "background", "darkgray" )
        , ( "font", "inherit" )
        , ( "border-radius", "3px" )
        , ( "border", "1px solid white" )
        ]
