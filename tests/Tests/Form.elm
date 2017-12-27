module Tests.Form exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Dict exposing (Dict)
import Form exposing (..)
import Form.Validators exposing (..)
import Form.Functions exposing (..)
import Date


suite : Test
suite =
    describe "Form"
        [ describe "createForm"
            [ test "form is initialised properly" <|
                \_ ->
                    let
                        form =
                            createForm []
                    in
                        equal form
                            { fields = Dict.empty
                            , error = Nothing
                            , submitting = False
                            }
            ]
        , describe "fieldString"
            [ fuzz string "defines a String field in a form" <|
                \key ->
                    let
                        form =
                            createForm [ fieldString key "" [] ]
                    in
                        Dict.get key form.fields
                            |> equal (Just (IsString (FieldOf "" (Err "") [])))
            ]
        , describe "fieldBool"
            [ fuzz string "defines a Bool field in a form" <|
                \key ->
                    let
                        form =
                            createForm [ fieldBool key True [] ]
                    in
                        Dict.get key form.fields
                            |> equal (Just (IsBool (FieldOf True (Err "") [])))
            ]
        , describe "fieldListString"
            [ fuzz2 string (list string) "defines a List String field in a form" <|
                \key values ->
                    let
                        form =
                            createForm [ fieldListString key values [] ]
                    in
                        Dict.get key form.fields
                            |> equal (Just (IsListString (FieldOf values (Err "") [])))
            ]
        , describe "fieldDate"
            [ fuzz string "defines a date field in a form" <|
                \key ->
                    let
                        dateInput =
                            "20-01-2017"

                        dateValue =
                            dateFromStringWithDefault dateInput

                        form =
                            createForm [ fieldDate key dateInput [] ]
                    in
                        Dict.get key form.fields
                            |> equal (Just (IsDate (FieldOf dateValue (Err "") [])))
            ]
        , describe "updateForm"
            [ fuzz string "update a string field from FieldMsg AsString" <|
                \newValue ->
                    let
                        key =
                            "email"
                    in
                        createForm [ fieldString key "" [] ]
                            |> (flip updateForm) (AsString key newValue)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsString (FieldOf newValue (Ok newValue) [])))
            , test "update a bool field from FieldMsg AsBool" <|
                \_ ->
                    let
                        key =
                            "isSexy"
                    in
                        createForm [ fieldBool key False [] ]
                            |> (flip updateForm) (AsBool key)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsBool (FieldOf True (Ok True) [])))
            , fuzz string "add an item to a List String field from FieldMsg AsListString" <|
                \string ->
                    let
                        key =
                            "hobbies"

                        newValues =
                            [ string ]
                    in
                        createForm [ fieldListString key [] [] ]
                            |> (flip updateForm) (AsListString key string)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsListString (FieldOf newValues (Ok newValues) [])))
            , fuzz string "remove an item from a List String field from FieldMsg AsListString" <|
                \string ->
                    let
                        key =
                            "hobbies"

                        newValues =
                            [ string ]
                    in
                        createForm [ fieldListString key newValues [] ]
                            |> (flip updateForm) (AsListString key string)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsListString (FieldOf [] (Ok []) [])))
            , test "update a date field from FieldMsg AsDate" <|
                \newValue ->
                    let
                        key =
                            "birthday"

                        dateInput =
                            "20-01-2017"

                        newDateInput =
                            "25-01-2017"

                        newDateValue =
                            dateFromStringWithDefault newDateInput
                    in
                        createForm [ fieldDate key dateInput [] ]
                            |> (flip updateForm) (AsString key newDateInput)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsDate (FieldOf newDateValue (Ok newDateValue) [])))
            ]
        , describe "isFormValid"
            [ describe "Invalid form"
                [ fuzz string "Returns False" <|
                    \key ->
                        createForm [ fieldString key "" [ isNotEmpty ] ]
                            |> isFormValid
                            |> equal False
                ]
            , describe "Valid form"
                [ fuzz2 string string "Returns True" <|
                    \key newValue ->
                        createForm [ fieldString key (newValue ++ "never empty") [ isNotEmpty ] ]
                            |> isFormValid
                            |> equal True
                ]
            ]
        , describe "getRawString"
            [ describe "Valid field name"
                [ fuzz string "Returns value of string field" <|
                    \value ->
                        let
                            fieldName =
                                "name"
                        in
                            createForm [ fieldString fieldName value [] ]
                                |> (flip getRawString) fieldName
                                |> equal value
                ]
            , describe "Invalid field name"
                [ fuzz string "Returns empty value of string field" <|
                    \value ->
                        let
                            fieldName =
                                "name"

                            invalidFieldName =
                                "missingName"
                        in
                            createForm [ fieldString fieldName value [] ]
                                |> (flip getRawString) invalidFieldName
                                |> equal ""
                ]
            ]
        , describe "getRawListString"
            [ describe "Valid field name"
                [ fuzz (list string) "Returns value of string field" <|
                    \value ->
                        let
                            fieldName =
                                "title"
                        in
                            createForm [ fieldListString fieldName value [] ]
                                |> (flip getRawListString) fieldName
                                |> equal value
                ]
            , describe "Invalid field name"
                [ fuzz (list string) "Returns empty list of list string field" <|
                    \value ->
                        let
                            fieldName =
                                "title"

                            invalidFieldName =
                                "missingTitle"
                        in
                            createForm [ fieldListString fieldName value [] ]
                                |> (flip getRawListString) invalidFieldName
                                |> equal []
                ]
            ]
        , describe "getRawDate"
            [ describe "Valid field name"
                [ test "Returns value of date field" <|
                    \_ ->
                        let
                            fieldName =
                                "birthday"

                            value =
                                "08-25-1987"

                            dateValue =
                                dateFromStringWithDefault value
                        in
                            createForm [ fieldDate fieldName value [] ]
                                |> (flip getRawDate) fieldName
                                |> equal dateValue
                ]
            , describe "Invalid field name"
                [ test "Returns default date of date field" <|
                    \_ ->
                        let
                            fieldName =
                                "birthday"

                            missingFieldName =
                                "missingBirthday"

                            value =
                                "08-25-1987"

                            dateValue =
                                Date.fromTime 0
                        in
                            createForm [ fieldDate fieldName value [] ]
                                |> (flip getRawDate) missingFieldName
                                |> equal dateValue
                ]
            ]
        ]
