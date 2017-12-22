module Form.Helpers exposing (getErrorMessage)

{-| Helpers function for View


# Helpers

@docs getErrorMessage

-}

import Dict exposing (Dict)
import Form exposing (Form, Field(..), FieldOf)


{-| Returns the error message of a field (if any)
-}
getErrorMessage : Form -> String -> Maybe String
getErrorMessage form fieldName =
    let
        field =
            Dict.get fieldName form.fields
    in
        case field of
            Nothing ->
                Nothing

            Just field ->
                case field of
                    IsString field ->
                        getFieldErrorMessage field

                    IsBool field ->
                        getFieldErrorMessage field

                    IsListString field ->
                        getFieldErrorMessage field

                    IsDate field ->
                        getFieldErrorMessage field


getFieldErrorMessage : FieldOf a -> Maybe String
getFieldErrorMessage field =
    case field.validationResult of
        Ok _ ->
            Nothing

        Err message ->
            Just message
