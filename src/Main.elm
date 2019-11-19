module Main exposing (..)

import Browser
import Html exposing (Html, text, div, pre, span, button)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Maybe.Extra as ME
import String.Interpolate exposing (interpolate)


---- MODEL ----

type Expression
    = Lambda String Expression
    | Variable String
    | Application Expression Expression

type alias Model =
    { mexpr : Maybe Expression
    , display : DisplayMode
    }


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

-- Church numbers

c_zero = Lambda "f0" <| Lambda "x0" <| Variable "x0"
c_is_zero = Lambda "n" <| Application (Application (Variable "n") (Lambda "x" ll2)) ll1
c_plus = Lambda "m⁺" <| Lambda "n⁺" <| Lambda "f⁺" <| Lambda "x⁺" <| Application (Application (Variable "m⁺") (Variable "f⁺")) (Application (Application (Variable "n⁺") (Variable "f⁺")) (Variable "x⁺"))
c_succ = Lambda "n˃" <| Lambda "f˃" <| Lambda "x˃" <| Application (Variable "f˃") (Application (Application (Variable "n˃") (Variable "f˃")) (Variable "x˃"))
c_pred = Lambda "n˂" <| Lambda "f˂" <| Lambda "x˂" <| Application (Application (Application (Variable "n˂") (Lambda "g˂" <| Lambda "h˂" <| Application (Variable "h˂") (Application (Variable "g˂") (Variable "f˂")))) (Lambda "u˂" <| Variable "x˂")) (Lambda "u˂" <| Variable "u˂")
c_sub = Lambda "m" <| Lambda "n⁻" <| Application (Application (Variable "n⁻") c_pred) (Variable "m")

churchNum : Int -> Expression
churchNum num =
  let loop n = if n == 0 then
                 Variable "x"
               else
                 Application (Variable "f") <| loop (n - 1)
  in
  Lambda "f" <| Lambda "x" <| loop num

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
    --( { mexpr = Just <| Application (Application c_plus (Application c_succ c_zero)) (Application c_succ <| Application c_succ c_zero), display = Normal }, Cmd.none )
  ( { mexpr = Just <| Application (Application c_sub (churchNum 4)) (churchNum 3), display = Normal }, Cmd.none )


---- UPDATE ----


type Msg
    = Reduce Path (Expression -> Maybe Path)
    | ChangeDisplay DisplayMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mexpr of
        Just expr ->
            case msg of
                Reduce path strategy ->
                  let reduction = betaReduce expr path
                      mpath = Maybe.map strategy reduction |> ME.join
                  in
                  ( { mexpr = reduction, display = mpath |> Maybe.map Follow |> Maybe.withDefault Normal}, Cmd.none )

                ChangeDisplay display ->
                  ( { model | display = display }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


---- VIEW ----

parenthesize htmls = [ text "(" ] ++ htmls ++ [ text ")" ]

addColor : String -> List (Html Msg) -> List (Html Msg)
addColor color contents =
  [ span [ style "background" color ] contents ]

highlightValue = addColor "#5f5"
highlightUse = addColor "orange"
highlightVar = addColor "#f55"


type DisplayMode = Follow Path | Highlight String | Normal

space = [ text " " ]

viewExpr : Expression -> DisplayMode -> List (Html Msg)
viewExpr expr display =
    case (expr, display) of
        (Variable name1, Highlight name2)  ->
          if name1 == name2 then
            highlightUse <| [ text name1 ]
          else
            [ text name1 ]

        (Variable name, _) ->
          [ text <| name ]

        (Application (Lambda var body) arg, Follow []) ->
          let argMod = case arg of
                         Application _ _ -> parenthesize
                         _ -> identity
          in
          (parenthesize <| viewExpr (Lambda var body) <| Highlight var) ++ space ++ highlightValue (argMod <| viewExpr arg Normal)

        (Application f arg, _) ->
          let fMod = case f of
                       Lambda var _ -> parenthesize
                       _ -> identity
              argMod = case arg of
                         Application _ _ -> parenthesize
                         _ -> identity
          in
          case display of
            Follow path ->
              case path of
                Fun :: path2 ->
                  (fMod <| viewExpr f <| Follow path2) ++ space ++ (argMod <| viewExpr arg Normal)

                Arg :: path2 ->
                  (fMod <| viewExpr f Normal) ++ space ++ (argMod <| viewExpr arg <| Follow path2)

                _ ->
                  [] -- BAAD should be Nothing

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
              (fMod <| viewExpr f display) ++ space ++ (argMod <| viewExpr arg display)

        (Lambda var body, Follow (Body :: path2)) ->
          [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body <| Follow path2)

        (Lambda fvar body, Highlight hvar) ->
          let varMod = if fvar == hvar then highlightVar else identity
          in
          [ text <| "λ" ] ++ (varMod [ text fvar ]) ++ [ text "." ] ++ (viewExpr body display)

        (Lambda var body, Normal) ->
          [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body display)

        _ ->
          [] -- BAAD should be Nothing

reductionButton title strategy expr =
    case strategy expr of
        Just path ->
            button [ onClick <| Reduce path strategy, onMouseOver <| ChangeDisplay <| Follow path, onMouseOut <| ChangeDisplay Normal ] [ text <| title ++ ": " ++ (pathStr path) ]

        Nothing ->
            button [] [ text <| title ++ ": X" ]

view : Model -> Html Msg
view model =
    case model.mexpr of
        Just expr ->
            div []
                [ div [] [ reductionButton "Normal order" normalOrder  expr
                         , reductionButton "Applicative order" applicativeOrder expr
                         ]
                , pre [] <| viewExpr expr model.display
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
