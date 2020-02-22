module Raindrops exposing (raindrops)

import Maybe.Extra


raindrops : Int -> String
raindrops number =
    number
        |> fmap
            [ check 2 "pling"
            , check 5 "plang"
            , check 7 "plong"
            ]
        |> Maybe.Extra.values
        |> collapse
            (String.join "")
            (number |> String.fromInt)


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


{-| Turn a list of things into one thing, but use a default when the list is
empty.

    []
        |> collapse (Html.div []) (Html.text "No items")
        == Html.text "No items"

    [ text "foo", text "bar" ]
        |> collapse (Html.div []) (Html.text "No items")
        == Html.div [] [ text "foo", text "bar" ]

-}
collapse : (List a -> b) -> b -> List a -> b
collapse collapser default list =
    if list == [] then
        default

    else
        list |> collapser
