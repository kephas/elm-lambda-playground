module Lambda.Playground exposing (..)

import Browser
import Cmd.Extra exposing (withNoCmd)
import Html exposing (Html, button, div, h1, hr, option, pre, select, span, text, textarea)
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
    | LoadExpr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        LoadExpr ->
            case model.exprParsed of
                Ok expr ->
                    { model
                        | exprToEval = Just expr
                        , display = Normal
                        , neverSeenAnExpression = False
                    }
                        |> withNoCmd

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


space =
    [ text " " ]


viewExpr : Expression -> DisplayMode -> List (Html Msg)
viewExpr expr display =
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
            (parenthesize <| viewExpr (Lambda var body) <| Highlight var) ++ space ++ highlightValue (argMod <| viewExpr arg Normal)

        ( Application f arg, _ ) ->
            let
                fMod =
                    case f of
                        Lambda var _ ->
                            parenthesize

                        -- BAAD not enough, any expression ending with a lambda needs parentheses
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
                            (fMod <| viewExpr f <| Follow path2) ++ space ++ (argMod <| viewExpr arg Normal)

                        Arg :: path2 ->
                            (fMod <| viewExpr f Normal) ++ space ++ (argMod <| viewExpr arg <| Follow path2)

                        -- BAAD should be Nothing
                        _ ->
                            []

                Highlight hvar ->
                    case f of
                        Lambda fvar _ ->
                            if hvar == fvar then
                                (fMod <| viewExpr f Normal) ++ space ++ (argMod <| viewExpr arg display)

                            else
                                (fMod <| viewExpr f display) ++ space ++ (argMod <| viewExpr arg display)

                        _ ->
                            (fMod <| viewExpr f display) ++ space ++ (argMod <| viewExpr arg display)

                Normal ->
                    (fMod <| viewExpr f Normal) ++ space ++ (argMod <| viewExpr arg Normal)

        ( Lambda var body, Follow (Body :: path2) ) ->
            [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body <| Follow path2)

        ( Lambda fvar body, Highlight hvar ) ->
            let
                varMod =
                    if fvar == hvar then
                        highlightVar

                    else
                        identity
            in
            [ text <| "λ" ] ++ varMod [ text fvar ] ++ [ text "." ] ++ viewExpr body display

        ( Lambda var body, Normal ) ->
            [ text <| "λ" ++ var ++ "." ] ++ viewExpr body Normal

        -- BAAD should be Nothing
        _ ->
            []


reductionButton title strategy expr =
    case strategy expr of
        Just path ->
            button [ onClick <| Reduce path strategy, onMouseOver <| ChangeDisplay <| Follow path, onMouseOut <| ChangeDisplay Normal ] [ text title ]

        Nothing ->
            button [] [ text title ]


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h1 [] [ text "Lambda Calculus playground" ]
        , textarea [ onInput ChangeExpr, style "width" "100%" ] []
        , case model.exprParsed of
            Ok expr ->
                pre [] <| viewExpr expr Normal

            Err _ ->
                pre [ style "color" "red" ] [ text "Cannot parse an expression" ]
        , button [ onClick LoadExpr ] [ text "Load" ]
        , hr [ style "margin" "1em" ] []
        , case model.exprToEval of
            Just expr ->
                div []
                    [ div []
                        [ reductionButton "Normal order" normalOrder expr
                        , reductionButton "Applicative order" applicativeOrder expr
                        ]
                    , pre [] <| viewExpr expr model.display
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
