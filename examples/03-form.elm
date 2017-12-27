module Main exposing (..)

import Char exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias FormData =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    }


type alias Model =
    { data : FormData
    , validate : Bool
    , errors : List String
    }


model : Model
model =
    Model (FormData "" "" "" "") False []



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Validate FormData


update : Msg -> Model -> Model
update msg { data, validate, errors } =
    case msg of
        Name name ->
            let
                newData =
                    { data | name = name }
            in
            { data = newData, validate = validate, errors = validateInput newData }

        Password password ->
            let
                newData =
                    { data | password = password }
            in
            { data = newData, validate = validate, errors = validateInput newData }

        PasswordAgain password ->
            let
                newData =
                    { data | passwordAgain = password }
            in
            { data = newData, validate = validate, errors = validateInput newData }

        Age age ->
            let
                newData =
                    { data | age = age }
            in
            { data = newData, validate = validate, errors = validateInput newData }

        Validate data ->
            { data = data, validate = True, errors = validateInput data }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "number", placeholder "Age", onInput Age ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , button [ onClick (Validate model.data) ]
            [ text "Submit"
            ]
        , viewValidation model
        ]


viewValidation : Model -> Html a
viewValidation model =
    if model.validate && List.isEmpty model.errors then
        ul [] [ validationItem "green" "OK" ]
    else if model.validate then
        ul [] <| List.map (validationItem "red") model.errors
    else
        text ""


validateInput : FormData -> List String
validateInput data =
    let
        errors =
            [ ( "Please enter a name", String.isEmpty data.name )
            , ( "Please enter an age", String.isEmpty data.age )
            , ( "Age must be a whole number", not (String.all Char.isDigit data.age) )
            , ( "Passwords do not match", data.password /= data.passwordAgain )
            , ( "Password must contain at least one number", not (String.any Char.isDigit data.password) )
            , ( "Password must be at least 8 characters", String.length data.password < 8 )
            , ( "Password must contain at least one lowercase letter", not (String.any Char.isLower data.password) )
            , ( "Password must contain at least one uppercase letter", not (String.any Char.isUpper data.password) )
            ]
    in
    List.filterMap maybeError errors


maybeError : ( String, Bool ) -> Maybe String
maybeError ( errMsg, condition ) =
    if condition then
        Just errMsg
    else
        Nothing


validationItem : String -> String -> Html a
validationItem color message =
    li [ style [ ( "color", color ) ] ] [ text message ]
