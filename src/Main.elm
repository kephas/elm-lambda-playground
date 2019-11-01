module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Html.Attributes exposing (src)


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

viewExpr : Expression -> String
viewExpr expr =
    case expr of
        Variable name ->
            name

        Application f arg ->
            viewExpr f ++ " " ++ viewExpr arg

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
