module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    , error : Maybe Http.Error
    , selectOptions : List String
    }


loadingGif =
    "http://moziru.com/images/drawn-hamster-hamster-wheel-7.gif"


errorGif =
    "http://pa1.narvii.com/6508/c2dfff227b76d11810cf2aff2e5487ac17b05a3b_00.gif"


selectOptions =
    [ "cat"
    , "dog"
    , "bat"
    , "jingle"
    , "snowman"
    , "bog"
    , "hog"
    , "smaug"
    , "hitchhikers"
    , "goal"
    ]


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic loadingGif Nothing selectOptions
    , getRandomGif topic
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | TopicChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( { model | gifUrl = loadingGif }, getRandomGif model.topic )

        TopicChange topic ->
            init topic

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl, error = Nothing }, Cmd.none )

        NewGif (Err error) ->
            ( { model | gifUrl = errorGif, error = Just error }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style styles.wrapper ]
        [ input
            [ style <| List.concat [ styles.select, styles.inputs ]
            , onInput TopicChange
            , placeholder "cats"
            ]
            []
        , select
            [ style <| List.concat [ styles.select, styles.inputs ]
            , onInput TopicChange
            ]
          <|
            List.map selectOption model.selectOptions
        , button
            [ style <| List.concat [ styles.inputs, styles.button ]
            , onClick MorePlease
            ]
            [ text "Find Gif!" ]
        , br [] []
        , errorBlock model
        , img [ style styles.img, src model.gifUrl ] []
        ]


selectOption value =
    option [] [ text value ]


errorBlock model =
    case model.error of
        Nothing ->
            text ""

        Just error ->
            div [ style styles.errorWrapper ]
                [ h1 [] [ text "Shit! Something screwy happened..." ]
                , p [ style styles.errorMessage ] [ text <| errorMessage error ]
                , sub [] [ text <| toString error ]
                ]


errorMessage error =
    case error of
        Http.BadUrl error ->
            "The image URL Giphy gave us is bullshit, man."

        Http.Timeout ->
            "The internets were wasting our time, so we gave up."

        Http.NetworkError ->
            "The network is being a dick right now. Trying again will probably work."

        Http.BadStatus response ->
            "Giphy is effed. Screw them anyway."

        Http.BadPayload error response ->
            "We asked Giphy for an image. They gave us something else. WTF?"


styles =
    { wrapper =
        [ ( "text-align", "center" )
        , ( "font", "1em/.8em monospace" )
        , ( "background", "black" )
        , ( "width", "100vw" )
        , ( "height", "100vh" )
        , ( "padding", "30px 15px" )
        , ( "color", "coral" )
        ]
    , img =
        [ ( "border", "3px solid white" )
        , ( "margin", "30px" )
        ]
    , inputs =
        [ ( "display", "block" )
        , ( "margin", "15px auto" )
        , ( "width", "250px" )
        , ( "box-sizing", "border-box" )
        ]
    , select =
        [ ( "height", "2.5em" ) ]
    , button =
        [ ( "background", "darkgray" )
        , ( "padding", "10px" )
        , ( "font", "inherit" )
        , ( "border-radius", "3px" )
        , ( "border", "1px solid white" )
        ]
    , errorWrapper =
        [ ( "max-width", "600px" )
        , ( "margin", "0 auto" )
        ]
    , errorMessage =
        [ ( "font-size", "1.25em" )
        , ( "line-height", "1.25em" )
        , ( "margin", "2.5em" )
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
