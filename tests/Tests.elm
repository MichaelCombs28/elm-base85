module Tests exposing (all)

import Base85
import Expect
import Fuzz exposing (int, list, string, tuple)
import String
import Test exposing (..)


all : Test
all =
    describe "Base85 test suite."
        [ describe "Encoding tests"
            [ test "Easy test" <|
                \() ->
                    Expect.equal (Base85.encode "easy") "<~ARTY*~>"
            , test "Moderate test" <|
                \() ->
                    Expect.equal (Base85.encode "moderate") "<~D/WrrEaa'$~>"
            , test "Somewhat Difficult" <|
                \() ->
                    Expect.equal
                        (Base85.encode "somewhat difficult")
                        "<~F)Po,GA(E,+Co1uAnbatCif~>"
            , test "Empty test" <|
                \() ->
                    Expect.equal
                        (Base85.encode "")
                        "<~~>"
            , test "Single null test" <|
                \() ->
                    Expect.equal
                        (Base85.encode "\u{0000}")
                        "<~!!~>"
            , test "Double null test" <|
                \() ->
                    Expect.equal
                        (Base85.encode "\u{0000}\u{0000}")
                        "<~!!!~>"
            , test "Triple null test" <|
                \() ->
                    Expect.equal
                        (Base85.encode "\u{0000}\u{0000}\u{0000}")
                        "<~!!!!~>"
            , test "Full null compression test" <|
                \() ->
                    Expect.equal
                        (Base85.encode "\u{0000}\u{0000}\u{0000}\u{0000}")
                        "<~z~>"
            , test "Leviathan" <|
                \() ->
                    Expect.equal
                        (Base85.encode "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
                        "<~9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>"
            ]
        , describe "Decoding tests"
            [ test "Easy test" <|
                \() ->
                    Expect.equal (Base85.decode "<~ARTY*~>") (Ok "easy")
            , test "Moderate test" <|
                \() ->
                    Expect.equal (Base85.decode "<~D/WrrEaa'$~>") (Ok "moderate")
            , test "Somewhat Difficult" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~F)Po,GA(E,+Co1uAnbatCif~>")
                        (Ok "somewhat difficult")
            , test "Empty test" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~~>")
                        (Ok "")
            , test "Single null test" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~!!~>")
                        (Ok "\u{0000}")
            , test "Double null test" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~!!!~>")
                        (Ok "\u{0000}\u{0000}")
            , test "Triple null test" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~!!!!~>")
                        (Ok "\u{0000}\u{0000}\u{0000}")
            , test "Full null compression test" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~z~>")
                        (Ok "\u{0000}\u{0000}\u{0000}\u{0000}")
            , test "Leviathan" <|
                \() ->
                    Expect.equal
                        (Base85.decode "<~9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>")
                        (Ok "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
            ]
        ]



{--
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
--}
