module Tests.Form exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Dict exposing (Dict)
import Form exposing (..)
import Form.Validators exposing (..)


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
            , fuzz (list string) "update a List String field from FieldMsg AsBool" <|
                \newValues ->
                    let
                        key =
                            "hobbies"
                    in
                        createForm [ fieldListString key [] [] ]
                            |> (flip updateForm) (AsListString key newValues)
                            |> .fields
                            |> Dict.get key
                            |> equal (Just (IsListString (FieldOf newValues (Ok newValues) [])))
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
        ]
