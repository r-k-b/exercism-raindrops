module Raindrops exposing (raindrops)

import Maybe.Extra


raindrops : Int -> String
raindrops number =
    let
        results =
            number
                |> fmap
                    [ check 2 "pling"
                    , check 5 "plang"
                    , check 7 "plong"
                    ]
                |> Maybe.Extra.values
    in
    case results of
        [] ->
            String.fromInt number

        _ ->
            results |> String.join ""


check : Int -> a -> Int -> Maybe a
check modulus result number =
    if (number |> modBy modulus) == 0 then
        Just result

    else
        Nothing


{-| Where `List.map` works on one function and several values, `fmap` works on
one value and several functions.

    "elephant"
        |> fmap [ String.reverse, String.left 3, String.toUpper ]
        == [ "tnahpele", "ele", "ELEPHANT" ]

-}
fmap : List (a -> b) -> a -> List b
fmap functions a =
    functions |> List.map (\func -> func a)
