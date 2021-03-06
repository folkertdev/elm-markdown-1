module Markdown.LinkReferenceDefinition exposing (..)

import Helpers
import LineEnding
import Markdown.Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Url


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser LinkReferenceDefinition
parser =
    inContext "link reference definition" <|
        succeed
            (\label destination title ->
                ( Markdown.Helpers.prepareRefLabel label
                , { destination = destination, title = title }
                )
            )
            -- TODO indented up to 3 spaces
            |. token (toToken "[")
            |= getChompedString (chompUntilEndOr "]")
            |. token (toToken "]:")
            -- TODO up to 1 line ending
            |. LineEnding.optionalWhitespaceUpToOneLineEnding
            |= destinationParser
            |= titleParser


destinationParser : Parser String
destinationParser =
    inContext "link destination" <|
        oneOf
            [ succeed Url.percentEncode
                |. token (toToken "<")
                |= getChompedString (chompUntilEndOr ">")
                |. token (toToken ">")
            , succeed (\first second -> first ++ second)
                |= getChompedString (chompIf (\c -> not <| Helpers.isGfmWhitespace c) (Parser.Expecting "non-whitespace character"))
                |= getChompedString (chompWhile (\c -> not <| Helpers.isGfmWhitespace c))
            ]


titleParser : Parser (Maybe String)
titleParser =
    inContext "title" <|
        oneOf
            [ succeed identity
                |. Helpers.requiredWhitespace
                |= oneOf
                    [ succeed Just
                        |. token (toToken "\"")
                        |= getChompedString (chompUntilEndOr "\"")
                        |. token (toToken "\"")
                    , succeed Just
                        |. token (toToken "'")
                        |= getChompedString (chompUntilEndOr "'")
                        |. token (toToken "'")
                    , succeed Nothing
                    ]
            , succeed Nothing
            ]


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


type alias LinkReferenceDefinition =
    ( String
    , { destination : String
      , title : Maybe String
      }
    )
