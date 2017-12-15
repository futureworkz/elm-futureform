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
isNotEmpty : Validator String
isNotEmpty string =
    if String.length string > 0 then
        Ok string
    else
        Err "This field cannot be empty"


{-| Checks that a string is a valid email
-}
validateEmail : Validator String
validateEmail email =
    if Regex.contains (Regex.regex "\\S+@\\S+\\.\\S+") email then
        Ok email
    else
        Err "Please enter a valid email."


{-| Checks that a string is a valid password
-}
validatePassword : Validator String
validatePassword password =
    if String.length password >= 6 then
        Ok password
    else
        Err "Password must be at least 6 characters long."
