module Lambda.Parser exposing (..)

import Lambda.Calculus exposing (..)
import Maybe.Extra exposing (unwrap)
import Parser exposing (..)
import Parser.Extras exposing (parens, some)
import String exposing (contains, fromChar)


specialChars =
    "\\. ()"


mustSpaces =
    let
        isSpace =
            \c -> c == ' '
    in
    succeed ()
        |. chompIf isSpace
        |. chompWhile isSpace


isVarChar : Char -> Bool
isVarChar char =
    Basics.not <| contains (fromChar char) specialChars


varName : Parser String
varName =
    getChompedString <|
        succeed ()
            |. chompIf isVarChar
            |. chompWhile isVarChar


variable : Parser Expression
variable =
    map Variable varName


lexpression =
    lazy (\_ -> expression)


lambda : Parser Expression
lambda =
    succeed Lambda
        |. oneOf [ symbol "\\", symbol "λ" ]
        |= varName
        |. symbol "."
        |= lexpression


term : Parser Expression
term =
    oneOf
        [ parens lexpression
        , lambda
        , variable
        ]


buildApplicationChain : List Expression -> Maybe Expression
buildApplicationChain terms =
    case terms of
        [ term1, term2 ] ->
            Just <| Application term1 term2

        term1 :: term2 :: term3 :: terms4_ ->
            -- note that terms4_ might be []
            buildApplicationChain <| Application term1 term2 :: term3 :: terms4_

        _ ->
            Nothing


parseApplicationChain : List Expression -> Parser Expression
parseApplicationChain terms =
    terms
        |> buildApplicationChain
        |> unwrap (problem "not enough terms") succeed


application : Parser Expression
application =
    sequence { start = "", separator = " ", end = "", spaces = succeed (), item = term, trailing = Forbidden }
        |> andThen parseApplicationChain


expression : Parser Expression
expression =
    oneOf
        [ backtrackable <| application
        , backtrackable <| lambda
        , variable
        ]
