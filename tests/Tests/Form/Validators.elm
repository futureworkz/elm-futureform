module Tests.Form.Validators exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Form.Validators exposing (..)


suite : Test
suite =
    describe "Validators"
        [ describe "validateMinChars"
            [ describe "Valid string"
                [ test "return result OK" <|
                    \_ ->
                        let
                            string =
                                "BigJon"
                        in
                            validateMinChars 3 string
                                |> equal (Ok string)
                ]
            , describe "Invalid string"
                [ test "return result Err for short string" <|
                    \_ ->
                        let
                            minimumLength =
                                3
                        in
                            validateMinChars minimumLength "Bi"
                                |> equal (Err ("Field must be at least " ++ (toString minimumLength) ++ " characters long."))
                ]
            , describe "validateIsAlphaNumeric"
                [ describe "Valid string"
                    [ test "return result OK" <|
                        \_ ->
                            let
                                string =
                                    "BigJon123"
                            in
                                validateIsAlphaNumeric string
                                    |> equal (Ok string)
                    ]
                , describe "Invalid string"
                    [ test "return result Err for short string" <|
                        \_ ->
                            validateIsAlphaNumeric "Big Jon"
                                |> equal (Err ("Field can only contains letters and numbers."))
                    ]
                ]
            ]
        , describe "validateUsername"
            [ describe "Valid username"
                [ test "return result OK" <|
                    \_ ->
                        let
                            username =
                                "BigJon"
                        in
                            validateUsername username
                                |> equal (Ok username)
                ]
            , describe "Invalid username"
                [ test "return result Err for short username" <|
                    \_ ->
                        let
                            username =
                                "Bi"
                        in
                            validateUsername username
                                |> equal (Err "Field must be at least 3 characters long.")
                , test "return result Err for invalid username" <|
                    \_ ->
                        let
                            username =
                                "Big Jon"
                        in
                            validateUsername username
                                |> equal (Err "Field can only contains letters and numbers.")
                ]
            ]
        ]
