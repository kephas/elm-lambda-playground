module Main exposing (..)

import Browser
import Html exposing (Html, text, div, pre, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import String.Interpolate exposing (interpolate)


---- MODEL ----

type Expression
    = Lambda String Expression
    | Variable String
    | Application Expression Expression

type alias Model =
    { mexpr : Maybe Expression }


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


y = let y0 = Lambda "x" <| Application (Variable "f") (Application (Variable "x") (Variable "x"))
    in
        Lambda "f" <| Application y0 y0


---- Beta reduction

type Direction = Body | Fun | Arg
type alias Path = List Direction

dirChar dir =
    case dir of
        Body -> 'B'
        Fun -> 'F'
        Arg -> 'A'

pathStr = String.fromList << List.map dirChar


normalOrder : Expression -> Maybe Path
normalOrder expr =
    case expr of
        Application (Lambda _ _) _ ->
            Just []

        Application fun arg ->
            case normalOrder fun of
                Just path ->
                    Just <| Fun :: path

                Nothing ->
                    normalOrder arg |> Maybe.map ((::) Arg)

        Lambda _ body ->
            normalOrder body |> Maybe.map ((::) Body)

        _ ->
            Nothing

applicativeOrder expr =
    case expr of
        Application fun arg ->
            case applicativeOrder arg of
                Just path ->
                    Just <| Arg :: path

                Nothing ->
                    case fun of
                        Lambda _ _ ->
                            Just []

                        _ ->
                            applicativeOrder fun |> Maybe.map ((::) Fun)
        Lambda _ body ->
            applicativeOrder body |> Maybe.map ((::) Body)

        _ ->
            Nothing


betaReduce : Expression -> Path -> Maybe Expression
betaReduce expr path =
    case (expr, path) of
        (Application (Lambda var body) arg, []) ->
            Just <| substitute var arg body

        (Application fun arg, Fun :: path2) ->
            betaReduce fun path2 |> Maybe.map (\f -> Application f arg)

        (Application fun arg, Arg :: path2) ->
            betaReduce arg path2 |> Maybe.map (Application fun)

        (Lambda var body, Body :: path2) ->
            betaReduce body path2 |> Maybe.map (Lambda var)

        _ ->
            Nothing


substitute var value expr =
    case expr of
        Lambda var2 body ->
                Lambda var2 (if var == var2 then
                                 body
                             else
                                 substitute var value body)

        Application fun arg ->
            Application (substitute var value fun) (substitute var value arg)

        Variable var2 ->
            if var == var2 then
                value
            else
                Variable var2


---- INIT ----

init : ( Model, Cmd Msg )
init =
    ( Model <| Just (Application y ω), Cmd.none )



---- UPDATE ----


type Msg
    = Reduce Path


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mexpr of
        Just expr ->
            case msg of
                Reduce path ->
                    ( { mexpr = betaReduce expr path }, Cmd.none )

        Nothing ->
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

reductionButton title mpath =
    case mpath of
        Just path ->
            button [ onClick <| Reduce path  ] [ text <| title ++ ": " ++ (pathStr path) ]

        Nothing ->
            button [] [ text <| title ++ ": X" ]

view : Model -> Html Msg
view model =
    case model.mexpr of
        Just expr ->
            div []
                [ div [] [ reductionButton "Normal order" <| normalOrder expr
                         , reductionButton "Applicative order" <| applicativeOrder expr
                         ]
                , pre [] [ text <| viewExpr expr ]
                ]

        Nothing ->
            pre [] [ text "Error" ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
