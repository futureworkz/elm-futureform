module Form.Validators exposing (..)

{-| Common validators


# Types

@docs Validator


# Validators

@docs isNotEmpty, validateEmail, validatePassword

-}

import Regex


{-| All validator must be of type Validator
-}
type alias Validator a =
    a -> Result String a


{-| Checks that a string is not empty
-}
isNotEmpty : Validator (Maybe String)
isNotEmpty string =
    case string of
        Just string ->
            if String.length string > 0 then
                Ok (Just string)
            else
                Err "This field cannot be empty"

        Nothing ->
            Err "This field cannot be empty"


{-| Checks that a string is a valid email
-}
validateEmail : Validator (Maybe String)
validateEmail email =
    case email of
        Just email ->
            if Regex.contains (Regex.regex "\\S+@\\S+\\.\\S+") email then
                Ok (Just email)
            else
                Err "Please enter a valid email."

        Nothing ->
            Err ""


{-| Checks that a string is a valid password
-}
validatePassword : Validator (Maybe String)
validatePassword password =
    case password of
        Just password ->
            if String.length password >= 6 then
                Ok (Just password)
            else
                Err "Password must be at least 6 characters long."

        Nothing ->
            Err ""
