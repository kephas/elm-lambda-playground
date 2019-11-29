module Lambda.Playground exposing (..)

import Browser
import Html exposing (Html, text, div, pre, span, button)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Lambda.Calculus exposing (..)
import Lambda.Church as C
import Maybe.Extra as ME
import String.Interpolate exposing (interpolate)


---- MODEL ----


type alias Model =
    { mexpr : Maybe Expression
    , display : DisplayMode
    }



dirChar dir =
    case dir of
        Body -> 'B'
        Fun -> 'F'
        Arg -> 'A'

pathStr = String.fromList << List.map dirChar




---- INIT ----

init : ( Model, Cmd Msg )
init =
  ( { mexpr = Just <|
        Application (Application (Application (Lambda "-" <| Lambda "4" <| Lambda "3" <| Application (Application (Variable "-") (Variable "4")) (Variable "3"))
                                    C.sub)
                       (C.fromInt 4))
          (C.fromInt 3)
    , display = Normal }, Cmd.none )


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
                       Lambda var _ -> parenthesize -- BAAD not enough, any expression ending with a lambda needs parentheses
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
              (fMod <| viewExpr f Normal) ++ space ++ (argMod <| viewExpr arg Normal)

        (Lambda var body, Follow (Body :: path2)) ->
          [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body <| Follow path2)

        (Lambda fvar body, Highlight hvar) ->
          let varMod = if fvar == hvar then highlightVar else identity
          in
          [ text <| "λ" ] ++ (varMod [ text fvar ]) ++ [ text "." ] ++ (viewExpr body display)

        (Lambda var body, Normal) ->
          [ text <| "λ" ++ var ++ "." ] ++ (viewExpr body Normal)

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
