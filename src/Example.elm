module Example exposing
    ( dragon
    , eyes
    , four
    , koch
    , one
    , parseAsSystem
    , plant
    , ranch
    , three
    , tweet1277757501060988929
    , tweet896797261471989760
    , tweet897416367514689536
    , tweet897597535254130690
    , tweet897839129299374082
    , tweet898382707616501760
    , two
    , city
    )

import Dict exposing (Dict)
import System exposing (System)


parseAsSystem :
    { start : String
    , angle : Int
    , iterations : Int
    , rules : List ( Char, String )
    }
    -> System
parseAsSystem definition =
    let
        start =
            String.toList definition.start

        rules =
            definition.rules
                |> List.map (\( char, str ) -> ( char, String.toList str ))
                |> Dict.fromList
    in
    { start = start
    , angle = definition.angle
    , iterations = definition.iterations
    , rules = rules
    }


one : System
one =
    parseAsSystem
        { start = "NSH"
        , angle = 90
        , iterations = 6
        , rules =
            [ ( 'F', "H-[F[-]]" )
            , ( 'D', "NNHF" )
            , ( 'N', "[]S-HFSHF" )
            , ( 'H', "[-[[N]DS-]]" )
            , ( 'S', "[N-+SHDN]" )
            ]
        }


tweet1277757501060988929 : System
tweet1277757501060988929 =
    parseAsSystem
        { start = "LFL"
        , angle = 90
        , iterations = 6
        , rules = [ ( 'F', "FFF-FLF" ) ]
        }


two : System
two =
    parseAsSystem
        { start = "S"
        , angle = 60
        , iterations = 7
        , rules =
            [ ( 'F', "JS" )
            , ( 'O', "F+F+FF" )
            , ( 'L', "JF++" )
            , ( 'V', "JL+-OSFF" )
            , ( 'J', "OF+" )
            , ( 'S', "[+S]LS+V" )
            ]
        }


three : System
three =
    parseAsSystem
        { start = "ZZ"
        , angle = 45
        , iterations = 5
        , rules =
            [ ( 'F', "-ZFFFF-FF" )
            , ( 'Z', "PA" )
            , ( 'P', "+AZ" )
            , ( 'A', "F+[ZF]AZ" )
            ]
        }


four : System
four =
    parseAsSystem
        { start = "TOT"
        , angle = 60
        , iterations = 5
        , rules =
            [ ( 'F', "O+" )
            , ( 'H', "F+F+" )
            , ( 'O', "T[OFF]+FH" )
            , ( 'T', "FF+HOTO" )
            ]
        }


tweet898382707616501760 : System
tweet898382707616501760 =
    parseAsSystem
        { start = "PFP"
        , angle = 90
        , iterations = 4
        , rules =
            [ ( 'F', "[C-F]" )
            , ( 'Q', "[]A" )
            , ( 'C', "+[A]P" )
            , ( 'A', "-F-+MQFFP" )
            , ( 'P', "AA" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/897839129299374082>
-}
tweet897839129299374082 : System
tweet897839129299374082 =
    parseAsSystem
        { start = "PFF"
        , angle = 253
        , iterations = 7
        , rules =
            [ ( 'F', "[FD]-MP+PL" )
            , ( 'L', "FF" )
            , ( 'D', "F-F+FFL+" )
            , ( 'M', "F[D+]L" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/tweet897597535254130690>
-}
tweet897597535254130690 : System
tweet897597535254130690 =
    parseAsSystem
        { start = "OFF"
        , angle = 36
        , iterations = 7
        , rules =
            [ ( 'F', "[+]FF-" )
            , ( 'O', "OFO[[-F]]" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/897416367514689536>
-}
tweet897416367514689536 : System
tweet897416367514689536 =
    parseAsSystem
        { start = "FQ"
        , angle = 45
        , iterations = 7
        , rules =
            [ ( 'F', "[+]F-FFFF" )
            , ( 'Q', "[-[+]QFF]F+" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/896797261471989760>
-}
tweet896797261471989760 : System
tweet896797261471989760 =
    parseAsSystem
        { start = "WEW"
        , angle = 243
        , iterations = 6
        , rules =
            [ ( 'F', "F[+]E-F" )
            , ( 'W', "FWFN--FN" )
            , ( 'E', "F" )
            , ( 'N', "F" )
            ]
        }


dragon : System
dragon =
    parseAsSystem
        { start = "FX"
        , angle = 90
        , iterations = 9
        , rules =
            [ ( 'X', "X+YF+" )
            , ( 'Y', "-FX-Y" )
            ]
        }


koch : System
koch =
    parseAsSystem
        { start = "F"
        , angle = 90
        , iterations = 4
        , rules =
            [ ( 'F', "F+F-F-F+F" )
            ]
        }


plant : System
plant =
    parseAsSystem
        { start = "X"
        , angle = 25
        , iterations = 6
        , rules =
            [ ( 'X', "F[][X]F[]+FX" )
            , ( 'F', "FF" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/1009998172490366977>
-}
eyes : System
eyes =
    parseAsSystem
        { start = "FTT"
        , angle = 147
        , iterations = 6
        , rules =
            [ ( 'T', "T-T-FFTF" )
            , ( 'F', "+-[-[-][]F]" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/1203785077739552768>
-}
ranch : System
ranch =
    parseAsSystem
        { start = "FF"
        , angle = 90
        , iterations = 4
        , rules =
            [ ( 'F', "[]CFC+" )
            , ( 'C', "[[]]-F[C-F]F" )
            ]
        }


{-| <https://twitter.com/LSystemBot/status/1210745947291975680>
-}
city : System
city =
    parseAsSystem
        { start = "F"
        , rules =
            [ ( 'F', "FFSF-FN+" )
            , ( 'V', "[]F" )
            , ( 'N', "FNSYN" )
            , ( 'S', "[+N]++-+-" )
            , ( 'Y', "V" )
            ]
        , angle = 45
        , iterations = 5
        }
