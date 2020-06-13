module Lambda.Church exposing (..)

import Lambda.Calculus exposing (..)


{-| Church encoding for numbers
-}
zero =
    Lambda "f0" <| Lambda "x0" <| Variable "x0"


is_zero =
    Lambda "n" <| Application (Application (Variable "n") (Lambda "x" ll2)) ll1


plus =
    Lambda "m⁺" <| Lambda "n⁺" <| Lambda "f⁺" <| Lambda "x⁺" <| Application (Application (Variable "m⁺") (Variable "f⁺")) (Application (Application (Variable "n⁺") (Variable "f⁺")) (Variable "x⁺"))


succ =
    Lambda "n˃" <| Lambda "f˃" <| Lambda "x˃" <| Application (Variable "f˃") (Application (Application (Variable "n˃") (Variable "f˃")) (Variable "x˃"))


pred =
    Lambda "n˂" <| Lambda "f˂" <| Lambda "x˂" <| Application (Application (Application (Variable "n˂") (Lambda "g˂" <| Lambda "h˂" <| Application (Variable "h˂") (Application (Variable "g˂") (Variable "f˂")))) (Lambda "u˂" <| Variable "x˂")) (Lambda "u˂" <| Variable "u˂")


sub =
    Lambda "m" <| Lambda "n⁻" <| Application (Application (Variable "n⁻") pred) (Variable "m")


fromInt : Int -> Expression
fromInt num =
    let
        loop n =
            if n == 0 then
                Variable "x"

            else
                Application (Variable "f") <| loop (n - 1)
    in
    Lambda "f" <| Lambda "x" <| loop num
