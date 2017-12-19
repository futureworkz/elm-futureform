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
    validateMinChars 6 password


{-| Check if string length is larger than an input number parameter
-}
validateMinChars : Int -> Validator String
validateMinChars minimumLength value =
    if String.length value < minimumLength then
        Err ("Field must be at least " ++ (toString minimumLength) ++ " characters long.")
    else
        Ok value


{-| Check if string is only contain Alphabet and Numeric
-}
validateIsAlphaNumeric : Validator String
validateIsAlphaNumeric value =
    let
        regex =
            \char -> Regex.contains (Regex.regex "[a-zA-Z0-9]") (toString char)
    in
        if String.all regex value then
            Ok value
        else
            Err ("Field can only contains letters and numbers.")


{-| Checks that a string only contain Alphabet and Numeric and string length larger than 2
-}
validateUsername : Validator String
validateUsername username =
    validateMinChars 3 username
        |> Result.andThen validateIsAlphaNumeric