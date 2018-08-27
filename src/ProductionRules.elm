module ProductionRules exposing (ProductionRules, addRule)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)


type Operation
    = Forward
    | RotateClockwise
    | RotateCounterClockwise
    | StackPush
    | StackPop
    | Rule Char


type alias Rule =
    List Operation


type ProductionRules
    = ProductionRules (Dict Char Rule)


addRule : Char -> Rule -> ProductionRules -> ProductionRules
addRule char rule (ProductionRules rules) =
    ProductionRules (Dict.insert char rule rules)


ruleParser : Parser Rule
ruleParser =
    Parser.oneOf
        [ Parser.succeed Forward |. Parser.symbol "F"
        , Parser.succeed RotateClockwise |. Parser.symbol "+"
        ]
