module Lambda.Playground exposing (..)

import Browser
import Cmd.Extra exposing (withNoCmd)
import Html exposing (Html, a, button, code, dd, div, dl, dt, h1, h2, h3, hr, li, option, pre, select, span, text, textarea, ul)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Lambda.Calculus exposing (..)
import Lambda.Church as C
import Lambda.Parser as LP
import Maybe.Extra as ME
import Parser as P
import String.Interpolate exposing (interpolate)



---- MODEL ----


type alias Model =
    { exprToEval : Maybe Expression
    , exprParsed : Result (List P.DeadEnd) Expression
    , neverSeenAnExpression : Bool
    , display : DisplayMode
    }


dirChar dir =
    case dir of
        Body ->
            'B'

        Fun ->
            'F'

        Arg ->
            'A'


pathStr =
    String.fromList << List.map dirChar



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( { exprToEval = Nothing
      , exprParsed = Err []
      , neverSeenAnExpression = True
      , display = Normal
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Reduce Path (Expression -> Maybe Path)
    | ChangeDisplay DisplayMode
    | ChangeExpr String
    | SelectExample Expression
    | LoadExpr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        switchToExpr expr =
            { model
                | exprToEval = Just expr
                , display = Normal
                , neverSeenAnExpression = False
            }
                |> withNoCmd
    in
    case msg of
        Reduce path strategy ->
            case model.exprToEval of
                Just expr ->
                    let
                        reduction =
                            betaReduce expr path

                        mpath =
                            Maybe.map strategy reduction |> ME.join
                    in
                    { model | exprToEval = reduction, display = mpath |> Maybe.map Follow |> Maybe.withDefault Normal } |> withNoCmd

                Nothing ->
                    model |> withNoCmd

        ChangeDisplay display ->
            { model | display = display } |> withNoCmd

        ChangeExpr code ->
            { model | exprParsed = code |> P.run LP.expression } |> withNoCmd

        SelectExample expr ->
            switchToExpr expr

        LoadExpr ->
            case model.exprParsed of
                Ok expr ->
                    switchToExpr expr

                Err _ ->
                    model |> withNoCmd



---- VIEW ----


parenthesize htmls =
    [ text "(" ] ++ htmls ++ [ text ")" ]


addColor : String -> List (Html Msg) -> List (Html Msg)
addColor color contents =
    [ span [ style "background" color ] contents ]


highlightValue =
    addColor "#5f5"


highlightUse =
    addColor "orange"


highlightVar =
    addColor "#f55"


type DisplayMode
    = Follow Path
    | Highlight String
    | Normal


type Placement
    = Anywhere
    | InAppliedFunction
    | RightmostInAppliedFunction


space =
    [ text " " ]


viewExpr : Expression -> Placement -> DisplayMode -> List (Html Msg)
viewExpr expr placement display =
    let
        lambdaMod lambdaView =
            if placement == RightmostInAppliedFunction then
                parenthesize lambdaView

            else
                lambdaView

        argPlacement =
            if placement == InAppliedFunction then
                RightmostInAppliedFunction

            else
                Anywhere
    in
    case ( expr, display ) of
        ( Variable name1, Highlight name2 ) ->
            if name1 == name2 then
                highlightUse <| [ text name1 ]

            else
                [ text name1 ]

        ( Variable name, _ ) ->
            [ text <| name ]

        ( Application (Lambda var body) arg, Follow [] ) ->
            let
                argMod =
                    case arg of
                        Application _ _ ->
                            parenthesize

                        _ ->
                            identity
            in
            (parenthesize <| viewExpr (Lambda var body) Anywhere <| Highlight var) ++ space ++ highlightValue (argMod <| viewExpr arg argPlacement Normal)

        ( Application f arg, _ ) ->
            let
                fMod =
                    case f of
                        Lambda var _ ->
                            parenthesize

                        _ ->
                            identity

                argMod =
                    case arg of
                        Application _ _ ->
                            parenthesize

                        _ ->
                            identity
            in
            case display of
                Follow path ->
                    case path of
                        Fun :: path2 ->
                            (fMod <| viewExpr f InAppliedFunction <| Follow path2) ++ space ++ (argMod <| viewExpr arg argPlacement Normal)

                        Arg :: path2 ->
                            (fMod <| viewExpr f InAppliedFunction Normal) ++ space ++ (argMod <| viewExpr arg argPlacement <| Follow path2)

                        -- BAAD should be Nothing
                        _ ->
                            []

                Highlight hvar ->
                    case f of
                        Lambda fvar _ ->
                            if hvar == fvar then
                                (fMod <| viewExpr f InAppliedFunction Normal) ++ space ++ (argMod <| viewExpr arg argPlacement display)

                            else
                                (fMod <| viewExpr f InAppliedFunction display) ++ space ++ (argMod <| viewExpr arg argPlacement display)

                        _ ->
                            (fMod <| viewExpr f InAppliedFunction display) ++ space ++ (argMod <| viewExpr arg argPlacement display)

                Normal ->
                    (fMod <| viewExpr f InAppliedFunction Normal) ++ space ++ (argMod <| viewExpr arg argPlacement Normal)

        ( Lambda var body, Follow (Body :: path2) ) ->
            lambdaMod <| [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body Anywhere <| Follow path2)

        ( Lambda fvar body, Highlight hvar ) ->
            let
                varMod =
                    if fvar == hvar then
                        highlightVar

                    else
                        identity
            in
            lambdaMod <| [ text <| "λ" ] ++ varMod [ text fvar ] ++ [ text "." ] ++ viewExpr body Anywhere display

        ( Lambda var body, Normal ) ->
            lambdaMod <| [ text <| "λ" ++ var ++ "." ] ++ viewExpr body Anywhere Normal

        -- BAAD should be Nothing
        _ ->
            []


reductionButton title strategy expr =
    case strategy expr of
        Just path ->
            button [ onClick <| Reduce path strategy, onMouseOver <| ChangeDisplay <| Follow path, onMouseOut <| ChangeDisplay Normal ] [ text title ]

        Nothing ->
            button [] [ text title ]


exampleExprs =
    [ ( "4 - 3"
      , Application
            (Application
                (Application (Lambda "-" <| Lambda "4" <| Lambda "3" <| Application (Application (Variable "-") (Variable "4")) (Variable "3"))
                    C.sub
                )
                (C.fromInt 4)
            )
            (C.fromInt 3)
      )
    , ( "2 * 3"
      , Application
            (Application
                (Application (Lambda "*" <| Lambda "2" <| Lambda "3" <| Application (Application (Variable "*") (Variable "2")) (Variable "3"))
                    C.mult
                )
                (C.fromInt 2)
            )
            (C.fromInt 3)
      )
    ]


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h1 [] [ text "Lambda Calculus playground" ]
        , div [ style "text-align" "left", style "margin-bottom" "2em" ] <|
            [ h2 [] [ text "Syntax:" ]
            , dl []
                [ dt [] [ text "Lambda function" ]
                , dd []
                    [ code [] [ text "\\foo.foo" ]
                    , span [] [ text " or " ]
                    , code [] [ text "λbaz.baz" ]
                    ]
                , dt [] [ text "Function application" ]
                , dd [] [ code [] [ text "foo bar" ] ]
                , dt [] [ text "Parentheses delimitate expressions" ]
                , dd []
                    [ code [] [ text "f (g x)" ]
                    , span [] [ text " or " ]
                    , code [] [ text "(λa.a) λb.b" ]
                    ]
                ]
            , h3 [] [ text "Examples" ]
            , dl [] <|
                List.concatMap
                    (\( desc, example ) ->
                        [ dt [] [ text desc ]
                        , dd []
                            [ button [ onClick <| SelectExample example ] <|
                                viewExpr example Anywhere Normal
                            ]
                        ]
                    )
                    exampleExprs
            ]
        , h2 [] [ text "Input your own expression" ]
        , textarea [ onInput ChangeExpr, style "width" "100%" ] []
        , case model.exprParsed of
            Ok expr ->
                pre [] <| viewExpr expr Anywhere Normal

            Err _ ->
                pre [ style "color" "red" ] [ text "Cannot parse an expression" ]
        , button [ onClick LoadExpr ] [ text "Load" ]
        , hr [ style "margin" "1em" ] []
        , h2 [] [ text "Evaluation" ]
        , case model.exprToEval of
            Just expr ->
                div []
                    [ div []
                        [ reductionButton "Normal order" normalOrder expr
                        , reductionButton "Applicative order" applicativeOrder expr
                        ]
                    , pre [] <| viewExpr expr Anywhere model.display
                    ]

            Nothing ->
                pre []
                    [ text <|
                        if model.neverSeenAnExpression then
                            "No expression yet."

                        else
                            "Error."
                    ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
