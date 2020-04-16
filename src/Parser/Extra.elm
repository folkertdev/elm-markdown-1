module Parser.Extra exposing (end, newline, oneOrMore, positiveInteger, tokenHelp, zeroOrMore)

import Parser
import Parser.Advanced as Advanced exposing ((|.), Parser, chompIf, chompWhile, mapChompedString, succeed)


oneOrMore : (Char -> Bool) -> Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Parser c x ()
zeroOrMore condition =
    chompWhile condition


positiveInteger : Parser c Parser.Problem Int
positiveInteger =
    succeed ()
        |. oneOrMore Char.isDigit
        |> mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


tokenHelp : String -> Parser c Parser.Problem ()
tokenHelp char =
    Advanced.token (Advanced.Token char (Parser.Expecting char))


newline : Parser c Parser.Problem ()
newline =
    Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))


end : Parser c Parser.Problem ()
end =
    Advanced.end (Parser.Expecting "End of input")
