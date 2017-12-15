# Elm FutureForm
Simple, clean and extendable form manager for Elm

```
elm package install Futureworkz/FutureForm
```

## Basic Usage
```elm
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Form exposing (Form, FieldMsg(..), createForm, fieldString, fieldBool, updateForm)
import Form.Validators exposing (validateEmail, validatePassword)
import Form.Helpers exposing (getErrorMessage)


-- Model


type Msg
    = SubmitForm
    | FormMsg FieldMsg -- Add FieldMsg to your Msg



-- Add Form to your model


type alias Model =
    { form : Form }



-- Create the form with fields and their associated validations


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { form =
            createForm
                [ fieldString "email" "" [ validateEmail ]
                , fieldString "password" "" [ validatePassword ]
                , fieldBool "remember" False []
                ]
      }
    , Cmd.none
    )



{-
   Two things need to be handled in your update:
   1) Forward the form update to FutureForm
   2) Handle the submitted form
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        form =
            model.form
    in
        case action of
            FormMsg fieldMsg ->
                ( { model | form = updateForm form fieldMsg }, Cmd.none )

            SubmitForm ->
                ( model, Cmd.none )



-- Render your form but pass the correct FieldMsg in event


view : Model -> Html Msg
view model =
    let
        { form } =
            model

        { error, submitting } =
            form
    in
        div []
            [ h1 [] [ text "Login Form" ]
            , Html.form [ onSubmit SubmitForm ]
                [ div [] [ text "Email" ]
                , input [ onInput (FormMsg << AsString "email") ] []
                , displayValidationError (getErrorMessage form "email")
                , div [] [ text "Password" ]
                , input [ type_ "password", onInput (FormMsg << AsString "password") ] []
                , displayValidationError (getErrorMessage form "password")
                , input [ type_ "checkbox", onClick (FormMsg (AsBool "remember")) ]
                    [ text "Remember me"
                    ]
                , button [] [ text "Submit" ]
                ]
            ]


displayValidationError : Maybe String -> Html Msg
displayValidationError message =
    case message of
        Nothing ->
            text ""

        Just msg ->
            p [] [ text msg ]


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
```

## Testing
- Install [`elm-test`](https://github.com/elm-community/elm-test)
- Run `elm-test`

## TODO
- Add tests for validators
- Add CI
