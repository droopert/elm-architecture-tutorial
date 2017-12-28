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
    , error : String
    }


init : String -> ( Model, Cmd Msg )
init topic =
    let
        loadingGif =
            "http://moziru.com/images/drawn-hamster-hamster-wheel-7.gif"
    in
    ( Model topic loadingGif ""
    , getRandomGif topic
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        NewGif (Ok newUrl) ->
            ( Model model.topic newUrl "", Cmd.none )

        NewGif (Err error) ->
            let
                errorGif =
                    "http://pa1.narvii.com/6508/c2dfff227b76d11810cf2aff2e5487ac17b05a3b_00.gif"
            in
            ( { model | gifUrl = errorGif, error = toString error }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , errorBlock model
        , img [ src model.gifUrl ] []
        ]


errorBlock model =
    if String.isEmpty model.error then
        text ""
    else
        div []
            [ h3 [] [ text "Aw, crap--an error!" ]
            , p [] [ text model.error ]
            ]


errorMessage error =
    case error of
        Http.BadUrl error ->
            "The image URL is bogus, man"

        Http.Timeout ->
            "The internets are wasting our time - we gave up"

        Http.NetworkError ->
            "The network is very grumpy"

        Http.BadStatus response ->
            "The image status is effed"

        Http.BadPayload error response ->
            "We asked for an image and got something else, wtf"



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
