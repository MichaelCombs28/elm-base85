module Base85 exposing (encode, decode)

{-| Library for base85 encoding and decoding of strings.
Uses 5 ascii characters to represent 4 bytes of binary data.

[Wikipedia](https://en.wikipedia.org/wiki/Ascii85) page for
more information.


# Functions

@docs encode, decode

-}

import Bitwise
import Char
import Dict exposing (Dict)
import String


{-| Encodes a string into ascii85 (base85)

    encode "easy" == "<~ARTY*~>"

-}
encode : String -> String
encode data =
    case Dict.get data nulls of
        Just s ->
            s

        Nothing ->
            encode_ data


{-| Decodes a string of ascii characters into the original chars.
Can only be codepoints between 33 - 117 as well as 'z' for compression.
String to be decoded must include the two delimiters at the start and end of the string

    decode "<~ARTY*~>" == "easy"

-}
decode : String -> Result String String
decode data =
    if String.length data < 1 then
        Ok ""

    else
        case Dict.get data nulls_ of
            Just s ->
                Ok s

            Nothing ->
                if String.left 2 data == "<~" && String.right 2 data == "~>" then
                    decode_ data

                else
                    Err "Base85 strings require delimiters <~ and ~>"



-- Private


mappings : List ( String, String )
mappings =
    [ ( "", "<~~>" )
    , ( "\u{0000}", "<~!!~>" )
    , ( "\u{0000}\u{0000}", "<~!!!~>" )
    , ( "\u{0000}\u{0000}\u{0000}", "<~!!!!~>" )
    , ( "\u{0000}\u{0000}\u{0000}\u{0000}", "<~z~>" )
    ]


rmDelims : String -> String
rmDelims s =
    String.dropLeft 2 s
        |> String.dropRight 2


nulls : Dict String String
nulls =
    Dict.fromList mappings


nulls_ : Dict String String
nulls_ =
    List.map (\( a, b ) -> ( b, a )) mappings
        |> Dict.fromList


addDelimeters : String -> String
addDelimeters s =
    "<~" ++ s ++ "~>"


partition : Int -> List a -> List (List a)
partition n list =
    case list of
        [] ->
            []

        xs ->
            [ List.take n xs ] ++ (partition n <| List.drop n xs)


binconcat : List Int -> Int
binconcat xs =
    case xs of
        w :: x :: y :: z :: [] ->
            Bitwise.shiftLeftBy 24 w
                |> (+) (Bitwise.shiftLeftBy 16 x)
                |> (+) (Bitwise.shiftLeftBy 8 y)
                |> (+) z

        _ ->
            0


radixReduce : Int -> String
radixReduce n =
    if n == 0 then
        "z"

    else
        let
            v1 =
                modBy 85 n

            newTot =
                (n - v1) // 85

            v2 =
                modBy 85 newTot

            newTot2 =
                (newTot - v2) // 85

            v3 =
                modBy 85 newTot2

            newTot3 =
                (newTot2 - v3) // 85

            v4 =
                modBy 85 newTot3

            newTot4 =
                (newTot3 - v4) // 85

            v5 =
                modBy 85 newTot4
        in
        List.map (Char.fromCode << (+) 33) [ v5, v4, v3, v2, v1 ]
            |> String.fromList


radixInflate : List Int -> String
radixInflate ints =
    case ints of
        v1 :: v2 :: v3 :: v4 :: v5 :: xs ->
            let
                w1 =
                    v1 * 85 ^ 4

                w2 =
                    v2 * 85 ^ 3

                w3 =
                    v3 * 85 ^ 2

                w4 =
                    v4 * 85 ^ 1

                w5 =
                    v5

                total =
                    w1 + w2 + w3 + w4 + w5
            in
            [ Bitwise.shiftRightBy 24 total |> Bitwise.and 255 |> Char.fromCode
            , Bitwise.shiftRightBy 16 total |> Bitwise.and 255 |> Char.fromCode
            , Bitwise.shiftRightBy 8 total |> Bitwise.and 255 |> Char.fromCode
            , Bitwise.and 255 total |> Char.fromCode
            ]
                |> String.fromList

        _ ->
            ""


expand : String -> Result String (List Int)
expand =
    String.foldl
        (\c ->
            Result.andThen
                (\xs ->
                    let
                        code =
                            Char.toCode c
                    in
                    if code == 122 then
                        Ok <| xs ++ List.repeat 5 0

                    else if code == 32 || code == 10 || code == 13 then
                        Ok xs

                    else if code < 33 || code > 117 then
                        Err ("Codepoint out of range." ++ String.fromChar c)

                    else
                        Ok <| xs ++ [ Char.toCode c - 33 ]
                )
        )
        (Ok [])


encode_ : String -> String
encode_ s =
    let
        padding =
            negate (String.length s)
                |> modBy 4

        data =
            s ++ String.repeat padding "\u{0000}"
    in
    String.toList data
        |> List.map Char.toCode
        |> partition 4
        |> List.foldl (\v acc -> acc ++ (radixReduce <| binconcat v)) ""
        |> String.dropRight padding
        |> addDelimeters


decode_ : String -> Result String String
decode_ s =
    let
        expanded =
            expand (rmDelims s)
    in
    case expanded of
        Ok xs ->
            Ok <|
                let
                    padding =
                        negate (List.length xs)
                            |> modBy 5

                    data =
                        xs ++ List.repeat padding 117
                in
                partition 5 data
                    |> List.map radixInflate
                    |> List.foldl (\a b -> b ++ a) ""
                    |> String.dropRight padding

        Err e ->
            Err e
