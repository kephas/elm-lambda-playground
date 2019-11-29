module Lambda.Parser exposing (..)

import Lambda.Calculus exposing (..)
import Parser exposing (..)
import String exposing (contains, fromChar)


specialChars =
    "\\. ()"

mustSpaces =
    let isSpace = (\c -> c == ' ')
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


lexpression = lazy (\_ -> expression)

lambda : Parser Expression
lambda =
    succeed Lambda
        |. symbol "\\"
        |= varName
        |. symbol "."
        |= lexpression

application : Parser Expression
application =
    succeed Application
        |= backtrackable lexpression
        |. mustSpaces
        |= lexpression

expression : Parser Expression
expression =
    oneOf
        [ application
        , lambda
        , variable
        ]
