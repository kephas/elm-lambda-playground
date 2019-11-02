module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Html.Attributes exposing (src)
import String.Interpolate exposing (interpolate)


---- MODEL ----

type Expression
    = Lambda String Expression
    | Variable String
    | Application Expression Expression

type alias Model =
    { expr : Expression }


---- Common expressions

l1 = Lambda "x" <| Variable "x"
ll1 = Lambda "x" <| Lambda "y" <| Variable "x"
ll2 = Lambda "x" <| Lambda "y" <| Variable "y"
lll123 = Lambda "x" <| Lambda "y"  <| Lambda "z" <| Application (Application (Variable "x") (Variable "y")) (Variable "z")
lll312 = Lambda "x" <| Lambda "y"  <| Lambda "z" <| Application (Application (Variable "z") (Variable "x")) (Variable "y")

and = Lambda "p" <| Lambda "q" <| Application (Application (Variable "p") (Variable "q")) (Variable "p")
or = Lambda "p" <| Lambda "q" <| Application (Application (Variable "p") (Variable "p")) (Variable "q")
not = Lambda "x" <| Application (Application (Variable "x") ll2) ll1

δ = Lambda "x" <| Application (Variable "x") (Variable "x")
ω = Application δ δ



---- INIT ----

init : ( Model, Cmd Msg )
init =
    ( Model ll2, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----

parenthesize str = interpolate "({0})" [str]

viewExpr : Expression -> String
viewExpr expr =
    case expr of
        Variable name ->
            name

        Application f arg ->
            let fMod = case f of
                            Lambda _ _ -> parenthesize
                            _ -> identity
                argMod = case arg of
                             Application _ _ -> parenthesize
                             _ -> identity
            in
                (fMod <| viewExpr f) ++ " " ++ (argMod <| viewExpr arg)

        Lambda var body ->
            "λ" ++ var ++ "." ++ viewExpr body

view : Model -> Html Msg
view model =
    pre [] [ text <| viewExpr model.expr ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
