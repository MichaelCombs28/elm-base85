module Base85 exposing (encode, decode)

import Dict exposing (Dict)
import String
import Char
import Bitwise


-- Private


mappings : List ( String, String )
mappings =
    [ ( "", "<~~>" )
    , ( "\x00", "<~!!~>" )
    , ( "\x00\x00", "<~!!!~>" )
    , ( "\x00\x00\x00", "<~!!!!~>" )
    , ( "\x00\x00\x00\x00", "<~z~>" )
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
    List.map (\(a, b) -> (b, a)) mappings
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
        w::x::y::z::[] ->
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
            v1 = n % 85
            newTot = (n - v1) // 85

            v2 = newTot % 85
            newTot2 = (newTot - v2) // 85

            v3 = newTot2 % 85
            newTot3 = (newTot2 - v3) // 85

            v4 = newTot3 % 85
            newTot4 = (newTot3 - v4) // 85

            v5 = newTot4 % 85
        in
            List.map (Char.fromCode << (+) 33) [v5,v4,v3,v2,v1]
                |> String.fromList


radixInflate : List Int -> String
radixInflate ints =
    case ints of
        v1::v2::v3::v4::v5::xs ->
            let
                w1 = v1 * 85 ^ 4
                w2 = v2 * 85 ^ 3
                w3 = v3 * 85 ^ 2
                w4 = v4 * 85 ^ 1
                w5 = v5
                total = w1 + w2 + w3 + w4 + w5
            in
                [ Bitwise.and 255 total |> Char.fromCode
                , Bitwise.shiftRightBy 8 total |> Bitwise.and 255 |> Char.fromCode
                , Bitwise.shiftRightBy 16 total |> Bitwise.and 255 |> Char.fromCode
                , Bitwise.shiftRightBy 24 total |> Bitwise.and 255 |> Char.fromCode
                ] |> List.reverse
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
                        code = Char.toCode c
                    in
                        if code == 122 then
                            Ok <| xs ++ List.repeat 5 0

                        else if (code == 32 || code == 10 || code == 13) then
                            Ok xs

                        else if code < 33 || code > 117 then
                            Err ("Codepoint out of range." ++ String.fromChar c)

                        else
                            Ok <| xs ++ [Char.toCode c - 33 ]
                )

        ) (Ok [])


encode_ : String -> String
encode_ s =
    let
        padding =
            negate (String.length s) % 4

        data =
            s ++ String.repeat padding "\0"
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
                            negate (List.length xs) % 5

                        data =
                            xs ++ List.repeat padding 117
                    in
                        partition 5 data
                            |> List.map radixInflate
                            |> List.foldl (flip (++)) ""
                            |> String.dropRight padding


            Err e ->
                Err e


encode : String -> String
encode data =
    case Dict.get data nulls of
        Just s ->
            s

        Nothing ->
            encode_ data


decode : String -> Result String String
decode data =
    if String.length data < 1 then
        Ok ""
    else
        case Dict.get data nulls_ of
            Just s ->
                Ok s

            Nothing ->
                decode_ data
