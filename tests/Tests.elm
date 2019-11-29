module Tests exposing (..)

import Test exposing (..)
import Expect
import Lambda.Calculus exposing (..)
import Parser as P
import Lambda.Parser as LP


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Test parsing"
        [ test "Variable" <|
            \_ ->
                Expect.equal (Ok (Variable "foo")) <| P.run LP.expression "foo"
        , test "Single lambda" <|
            \_ ->
                Expect.equal (Ok (Lambda "x" (Variable "x"))) <| P.run LP.expression "\\x.x"
        , test "Nested lambdas" <|
            \_ ->
                Expect.equal (Ok (Lambda "x" (Lambda "y" (Variable "x")))) <| P.run LP.expression "\\x.\\y.x"
        , test "Basic application" <|
            \_ ->
                Expect.equal (Ok (Application (Variable "x") (Variable "y"))) <| P.run LP.expression "x y"
        ]
