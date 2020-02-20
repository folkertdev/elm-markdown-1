module ParagraphTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.Parser.parse


suite : Test
suite =
    describe "paragraphs"
        [ test "consecutive lines are considered one paragraph" <|
            \() ->
                """Line 1
Line 2
Line 3
Line 4
"""
                    |> parse
                    |> Expect.equal (Ok [ Block.Paragraph (unstyledText "Line 1\nLine 2\nLine 3\nLine 4") ])
        , test "trailing whitespace is stripped out" <|
            \() ->
                "Line 1\t\nLine 2   \nLine 3\nLine 4\n"
                    |> parse
                    |> Expect.equal (Ok [ Block.Paragraph (unstyledText "Line 1\nLine 2\nLine 3\nLine 4") ])
        , test "new paragraphs are created by blank lines in between" <|
            \() ->
                """Line 1
Line 2

Line after blank line"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph (unstyledText "Line 1\nLine 2")
                            , Block.Paragraph (unstyledText "Line after blank line")
                            ]
                        )
        ]


unstyledText : String -> List Inline
unstyledText body =
    [ Block.Text body ]
