module Form
    exposing
        ( FieldMsg(..)
        , Form
        , Field(..)
        , FieldOf
        , FormConfig
        , createForm
        , fieldString
        , fieldBool
        , fieldListString
        , fieldDate
        , updateForm
        , validate
        , isFormValid
        , getRawBool
        , getRawString
        , getRawListString
        , getRawDate
        , addFieldError
        )

{-| Simple, clean and extendable form manager for Elm


# Types

@docs FieldMsg , Form , Field, FieldOf , FormConfig


# Create form and fields

@docs createForm, fieldString, fieldBool, fieldListString


# Update form and fields state

@docs updateForm


# Check if form is valid

@docs isFormValid

-}

import Dict exposing (Dict)
import Date
import Form.Validators exposing (Validator)
import Form.Functions exposing (..)


-- Types


{-| Field message to update a field
-}
type FieldMsg
    = AsString String String
    | AsBool String
    | AsListString String String


{-| Form type
-}
type alias Form =
    { fields : Dict String Field
    , error : Maybe String
    , submitting : Bool
    }


{-| Field type
-}
type Field
    = IsString (FieldOf String)
    | IsBool (FieldOf Bool)
    | IsListString (FieldOf (List String))
    | IsDate (FieldOf Date.Date)


{-| Internal type of a Field
-}
type alias FieldOf a =
    { value : a
    , validationResult : Result String a
    , validators : List (Validator a)
    }


{-| Type used for configuring the form
-}
type alias FormConfig =
    Form -> Form



-- Form and Fields


{-| Creates a form
-}
createForm : List FormConfig -> Form
createForm configs =
    let
        initialForm =
            { fields = Dict.empty
            , error = Nothing
            , submitting = False
            }
    in
        applyConfig configs initialForm


{-| Private: applies config to the form
-}
applyConfig : List FormConfig -> Form -> Form
applyConfig configs form =
    case configs of
        config :: rest ->
            applyConfig rest (config form)

        empty ->
            form


{-| Creates a field string
-}
fieldString : String -> String -> List (Validator String) -> FormConfig
fieldString name value validators form =
    let
        field =
            IsString { value = value, validationResult = Err "", validators = validators }
    in
        { form | fields = Dict.insert name field form.fields }


{-| Creates a field bool
-}
fieldBool : String -> Bool -> List (Validator Bool) -> FormConfig
fieldBool name value validators form =
    let
        field =
            IsBool { value = value, validationResult = Err "", validators = validators }
    in
        { form | fields = Dict.insert name field form.fields }


{-| Creates a field of list string
-}
fieldListString : String -> List String -> List (Validator (List String)) -> FormConfig
fieldListString name value validators form =
    let
        field =
            IsListString { value = value, validationResult = Err "", validators = validators }
    in
        { form | fields = Dict.insert name field form.fields }


{-| Creates a field of date
-}
fieldDate : String -> String -> List (Validator Date.Date) -> FormConfig
fieldDate name value validators form =
    let
        field =
            IsDate { value = (dateFromStringWithDefault value), validationResult = Err "", validators = validators }
    in
        { form | fields = Dict.insert name field form.fields }


{-| Update the state of a form and runs validation
-}
updateForm : Form -> FieldMsg -> Form
updateForm form fieldMsg =
    let
        update =
            updateOnFieldMsg fieldMsg
    in
        case fieldMsg of
            AsString name _ ->
                { form | fields = Dict.update name update form.fields }

            AsBool name ->
                { form | fields = Dict.update name update form.fields }

            AsListString name _ ->
                { form | fields = Dict.update name update form.fields }


{-| Private: Update a field based on a FieldMsg
-}
updateOnFieldMsg : FieldMsg -> Maybe Field -> Maybe Field
updateOnFieldMsg fieldMsg field =
    case ( field, fieldMsg ) of
        ( Just (IsString field), AsString _ value ) ->
            Just <| IsString <| updateField field <| value

        ( Just (IsBool field), AsBool _ ) ->
            Just <| IsBool <| updateField field <| not field.value

        ( Just (IsListString field), AsListString _ value ) ->
            Just <| IsListString <| updateField field <| addOrRemoveListStringItem field.value value

        ( Just (IsDate field), AsString _ value ) ->
            Just <| IsDate <| updateField field <| dateFromStringWithDefault value

        _ ->
            Nothing


{-| Add field error
-}
addFieldError : String -> Maybe Field -> Maybe Field
addFieldError errorMsg field =
    case field of
        Just (IsString field) ->
            Just (IsString { field | validationResult = (Err errorMsg) })

        Just (IsBool field) ->
            Just (IsBool { field | validationResult = (Err errorMsg) })

        Just (IsListString field) ->
            Just (IsListString { field | validationResult = (Err errorMsg) })

        Just (IsDate field) ->
            Just (IsDate { field | validationResult = (Err errorMsg) })

        Nothing ->
            Nothing


{-| Private: Update a field
-}
updateField : FieldOf a -> a -> FieldOf a
updateField field value =
    { field
        | value = value
        , validationResult = validateValue field.validators value
    }


{-| Private: Runs all validations on a value
-}
validateValue : List (Validator a) -> a -> Result String a
validateValue validators value =
    case validators of
        validator :: rest ->
            case validator value of
                Ok _ ->
                    validateValue rest value

                Err msg ->
                    Err msg

        empty ->
            Ok value


{-| Checks if a form is true
-}
isFormValid : Form -> Bool
isFormValid form =
    form
        |> validate
        |> .fields
        |> Dict.values
        |> List.all isFieldValid


{-| Force the form to run all validations
-}
validate : Form -> Form
validate form =
    { form | fields = Dict.map (always validateField) form.fields }


{-| Private: Validate a field
-}
validateField : Field -> Field
validateField field =
    case field of
        IsString field ->
            IsString (updateField field field.value)

        IsBool field ->
            IsBool (updateField field field.value)

        IsListString field ->
            IsListString (updateField field field.value)

        IsDate field ->
            IsDate (updateField field field.value)


{-| Private: Check if a field is valid
-}
isFieldValid : Field -> Bool
isFieldValid field =
    case field of
        IsString field ->
            isResultOk field.validationResult

        IsBool field ->
            isResultOk field.validationResult

        IsListString field ->
            isResultOk field.validationResult

        IsDate field ->
            isResultOk field.validationResult


{-| Private: Return true if a Result is Ok else false
-}
isResultOk : Result a b -> Bool
isResultOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


{-| Get value of a string field
-}
getRawString : Form -> String -> String
getRawString form name =
    case Dict.get name form.fields of
        Just (IsString field) ->
            field.value

        _ ->
            missingFieldWarning name "getRawString" ""


{-| Get value of a list string field
-}
getRawListString : Form -> String -> List String
getRawListString form name =
    case Dict.get name form.fields of
        Just (IsListString field) ->
            field.value

        _ ->
            missingFieldWarning name "getRawListString" []


{-| Get value of a bool field
-}
getRawBool : Form -> String -> Bool
getRawBool form name =
    case Dict.get name form.fields of
        Just (IsBool field) ->
            field.value

        _ ->
            missingFieldWarning name "getRawBool" ""
                |> always False


{-| Get value of a date field
-}
getRawDate : Form -> String -> Date.Date
getRawDate form name =
    case Dict.get name form.fields of
        Just (IsDate field) ->
            field.value

        _ ->
            missingFieldWarning name "getRawDate" (Date.fromTime 0)


{-| Write log to console about missing field
-}
missingFieldWarning : String -> String -> a -> a
missingFieldWarning fieldName functionName defaultValue =
    ("Field " ++ fieldName ++ " is not found.")
        |> Debug.log ("FutureForm - " ++ functionName)
        |> always defaultValue
