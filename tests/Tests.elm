module Tests exposing (..)

import Expect
import Lambda.Calculus exposing (..)
import Lambda.Parser as LP
import Parser as P
import Test exposing (..)



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

        {- , test "Application chain building" <|
           \_ ->
               Expect.equal (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d")) <|
                   P.run
                   LP.parseApplicationChain [ Variable "a", Variable "b", Variable "c", Variable "d" ]
        -}
        , test "Nested applications" <|
            \_ ->
                Expect.equal (Ok (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d"))) <| P.run LP.expression "a b c d"
        , test "Complex expression" <|
            \_ ->
                Expect.equal (Ok y) <| P.run LP.expression "λf.(λx.f (x x)) λx.f (x x)"
        ]
