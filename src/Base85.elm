module Base85 exposing (encode, decode)

import Dict exposing (Dict)
import String
import Char
import Bitwise


-- Private


type Base85
    = Encode
    | Decode


mappings : List ( String, String )
mappings =
    [ ( "", "<~~>" )
    , ( "\x00", "<~!!~>" )
    , ( "\x00\x00", "<~!!!~>" )
    , ( "\x00\x00\x00", "<~!!!!~>" )
    , ( "\x00\x00\x00\x00", "<~z~>" )
    ]


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


pad : Base85 -> String -> ( Int, String )
pad b s =
    let
        length =
            String.length s
    in
        case b of
            Encode ->
                let
                    padding =
                        negate length % 4
                in
                    if padding > 0 then
                        ( padding, s ++ String.repeat padding "\x00" )
                    else
                        ( 0, s )

            Decode ->
                let
                    padding =
                        negate length % 5
                in
                    if padding > 0 then
                        ( padding, s ++ String.repeat padding "u" )
                    else
                        ( 0, s )


partition : Int -> List a -> List (List a)
partition n chars =
    case chars of
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


binreduce : Int -> String
binreduce n =
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


encode_ : String -> String
encode_ s =
    let
        ( padding, data ) =
            pad Encode s
    in
        String.toList data
            |> List.map Char.toCode
            |> partition 4
            |> List.foldl (\v acc -> acc ++ (binreduce <| binconcat v)) ""
            |> String.dropRight padding
            |> addDelimeters


encode : String -> String
encode data =
    case Dict.get data nulls of
        Just s ->
            s

        Nothing ->
            encode_ data


decode : String -> Result String String
decode data =
    Ok data
