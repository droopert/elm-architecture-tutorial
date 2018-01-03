module Main exposing (..)

import Date exposing (Date)
import Date.Extra exposing (fractionalDay)
import Html exposing (Html, br, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Time
    , isPaused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 0, isPaused = False }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | PausePlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        PausePlay ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPaused then
        Sub.none
    else
        Time.every second Tick



-- VIEW


view : Model -> Html Msg
view { time, isPaused } =
    let
        clockHands =
            [ { angle = turns (Time.inMinutes time)
              , handLength = 40
              , handWidth = "1"
              }
            , { angle = turns (Time.inHours time)
              , handLength = 35
              , handWidth = "3"
              }
            , { angle = turns <| (fractionalDay <| Date.fromTime time) * 2
              , handLength = 25
              , handWidth = "5"
              }
            ]
    in
    div []
        [ svg [ viewBox "0 0 100 100", width "300px", Html.Attributes.style styles ] <|
            List.concat
                [ [ circle [ cx "50", cy "50", r "45", fill colors.face ] [] ]
                , [ line [ x1 "8", y1 "50", x2 "92", y2 "50", stroke colors.axisLines ] []
                  , line [ x1 "50", y1 "8", x2 "50", y2 "92", stroke colors.axisLines ] []
                  , circle [ cx "50", cy "50", r "38", fill colors.face ] []
                  ]
                , List.map makeClockHand clockHands
                , [ circle [ cx "50", cy "50", r "10", fill colors.face ] [] ]
                ]
        , br [] []
        , button [ onClick PausePlay, Html.Attributes.style styles ]
            [ text <|
                if isPaused then
                    "Play"
                else
                    "Pause"
            ]
        ]


makeClockHand { angle, handLength, handWidth } =
    let
        x =
            toString (50 + handLength * cos (angle - pi / 2))

        y =
            toString (50 + handLength * sin (angle - pi / 2))
    in
    line [ x1 "50", y1 "50", x2 x, y2 y, stroke colors.hands, strokeWidth handWidth ] []


styles =
    [ ( "display", "block" ), ( "margin", "0 auto" ) ]


colors =
    { face = "#0B79CE"
    , hands = "#023963"
    , axisLines = "#035fa5"
    }
