module Markdown.Parser exposing (parse, deadEndToString)

{-|

@docs parse, deadEndToString

-}

import Dict exposing (Dict)
import Helpers
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.CodeBlock
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition exposing (LinkReferenceDefinition)
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock as RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Markdown.Table
import Markdown.TableParser as TableParser
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompWhile, getChompedString, loop, map, oneOf, problem, succeed, symbol, token)
import Parser.Extra exposing (zeroOrMore)
import ThematicBreak


{-| Try parsing a markdown String into `Markdown.Block.Block`s.

Often you'll want to render these `Block`s directly:

    render renderer markdown =
        markdown
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.render renderer ast)

    deadEndsToString deadEnds =
        deadEnds
            |> List.map deadEndToString
            |> String.join "\n"

But you can also do a lot with the `Block`s before passing them through:

  - Transform the `Block`s ([example: make each heading one level deeper](TODO))
  - Use the blocks to gather metadata about the markdown document ([example: building a table of contents from `Block`s](TODO))

-}
parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse input =
    case Advanced.run (rawBlockParser |. succeed Advanced.end) input of
        Err e ->
            Err e

        Ok v ->
            case parseAllInlines v of
                Err e ->
                    -- NOTE these messages get an incorrect location
                    -- but they always did, because the inlines are not parsed in the original source string.
                    -- Rather the blocks are sliced from the source string, then parsed. So the errors in that sliced
                    -- string would not map correctly to locations in the original source string
                    Advanced.run (problem e) ""

                Ok items ->
                    Ok
                        (List.filter
                            (\item ->
                                case item of
                                    Block.Paragraph [] ->
                                        False

                                    _ ->
                                        True
                            )
                            items
                        )


deadEndsToString : List (Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


{-| Turn a parsing problem into the default String representation.
-}
deadEndToString : Advanced.DeadEnd String Parser.Problem -> String
deadEndToString deadEnd =
    "Problem at row " ++ String.fromInt deadEnd.row ++ "\n" ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting string ->
            "Expecting " ++ string

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol string ->
            "Expecting symbol " ++ string

        Parser.ExpectingKeyword string ->
            "Expecting keyword " ++ string

        Parser.ExpectingEnd ->
            "Expecting keyword end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem problemDescription ->
            problemDescription

        Parser.BadRepeat ->
            "Bad repeat"


type alias Parser a =
    Advanced.Parser String Parser.Problem a


inlineParseHelper : LinkReferenceDefinitions -> UnparsedInlines -> List Block.Inline
inlineParseHelper referencesDict (UnparsedInlines unparsedInlines) =
    let
        referencesDict2 =
            referencesDict
                |> List.map (Tuple.mapSecond (\{ destination, title } -> ( destination, title )))
                |> Dict.fromList

        --
        --myReferences =
        --    Dict.fromList
        --        [ ( "foo", { destination = "/url", title = Just "title" } )
        --        ]
    in
    Markdown.InlineParser.parse referencesDict2 unparsedInlines
        |> List.map mapInline


mapInline : Inline.Inline -> Block.Inline
mapInline inline =
    case inline of
        Inline.Text string ->
            Block.Text string

        Inline.HardLineBreak ->
            Block.HardLineBreak

        Inline.CodeInline string ->
            Block.CodeSpan string

        Inline.Link string maybeString inlines ->
            Block.Link string maybeString (inlines |> List.map mapInline)

        Inline.Image string maybeString inlines ->
            Block.Image string maybeString (inlines |> List.map mapInline)

        Inline.HtmlInline node ->
            node
                |> nodeToRawBlock
                |> Block.HtmlInline

        Inline.Emphasis level inlines ->
            case level of
                1 ->
                    Block.Emphasis (inlines |> List.map mapInline)

                2 ->
                    Block.Strong (inlines |> List.map mapInline)

                _ ->
                    -- TODO fix this
                    Block.Strong (inlines |> List.map mapInline)


toHeadingLevel : Int -> Result Parser.Problem Block.HeadingLevel
toHeadingLevel level =
    case level of
        1 ->
            Ok Block.H1

        2 ->
            Ok Block.H2

        3 ->
            Ok Block.H3

        4 ->
            Ok Block.H4

        5 ->
            Ok Block.H5

        6 ->
            Ok Block.H6

        _ ->
            Err ("A heading with 1 to 6 #'s, but found " ++ String.fromInt level |> Parser.Expecting)


type InlineResult
    = EmptyBlock
    | ParsedBlock Block
    | InlineProblem Parser.Problem


parseInlines : LinkReferenceDefinitions -> RawBlock -> InlineResult
parseInlines linkReferences rawBlock =
    case rawBlock of
        Heading level unparsedInlines ->
            case toHeadingLevel level of
                Ok parsedLevel ->
                    inlineParseHelper linkReferences unparsedInlines
                        |> Block.Heading parsedLevel
                        |> ParsedBlock

                Err err ->
                    InlineProblem err

        Body unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper linkReferences
                |> Block.Paragraph
                |> ParsedBlock

        Html html ->
            Block.HtmlBlock html
                |> ParsedBlock

        UnorderedListBlock unparsedItems ->
            let
                parseItem unparsed =
                    let
                        parsedInlines =
                            parseRawInline linkReferences identity unparsed.body

                        task =
                            case unparsed.task of
                                Just False ->
                                    Block.IncompleteTask

                                Just True ->
                                    Block.CompletedTask

                                Nothing ->
                                    Block.NoTask
                    in
                    Block.ListItem task parsedInlines
            in
            unparsedItems
                |> List.map parseItem
                |> Block.UnorderedList
                |> ParsedBlock

        OrderedListBlock startingIndex unparsedInlines ->
            unparsedInlines
                |> List.map (parseRawInline linkReferences identity)
                |> Block.OrderedList startingIndex
                |> ParsedBlock

        CodeBlock codeBlock ->
            Block.CodeBlock codeBlock
                |> ParsedBlock

        ThematicBreak ->
            Block.ThematicBreak
                |> ParsedBlock

        BlankLine ->
            EmptyBlock

        BlockQuote rawBlocks ->
            case Advanced.run rawBlockParser rawBlocks of
                Ok value ->
                    case parseAllInlines value of
                        Ok parsedBlocks ->
                            ParsedBlock (Block.BlockQuote parsedBlocks)

                        Err error ->
                            InlineProblem error

                Err error ->
                    -- NOTE these errors have incorrect locations
                    InlineProblem (Parser.Problem (deadEndsToString error))

        IndentedCodeBlock codeBlockBody ->
            Block.CodeBlock { body = codeBlockBody, language = Nothing }
                |> ParsedBlock

        Table (Markdown.Table.Table header rows) ->
            let
                parseHeader { label, alignment } =
                    let
                        wrap parsedHeaderLabel =
                            { label = parsedHeaderLabel
                            , alignment = alignment
                            }
                    in
                    parseRawInline linkReferences wrap (UnparsedInlines label)
            in
            Block.Table (List.map parseHeader header) []
                |> ParsedBlock


parseRawInline : LinkReferenceDefinitions -> (List Inline -> a) -> UnparsedInlines -> a
parseRawInline linkReferences wrap unparsedInlines =
    unparsedInlines
        |> inlineParseHelper linkReferences
        |> wrap


plainLine : Parser RawBlock
plainLine =
    innerParagraphParser
        |. oneOf
            [ Advanced.chompIf Helpers.isNewline (Parser.Expecting "A single non-newline char.")
            , Advanced.end (Parser.Expecting "End")
            ]


innerParagraphParser =
    Advanced.chompIf (\c -> not <| Helpers.isNewline c) (Parser.Expecting "Not newline.")
        |. Advanced.chompUntilEndOr "\n"
        |> Advanced.mapChompedString
            (\rawLine _ ->
                rawLine
                    |> UnparsedInlines
                    |> Body
            )


blockQuoteStart =
    -- Investigate: can we re-order these? put the shortest matches first
    oneOf
        [ symbol (Advanced.Token "   > " (Parser.Expecting "   > "))
        , symbol (Advanced.Token "  > " (Parser.Expecting "  > "))
        , symbol (Advanced.Token " > " (Parser.Expecting " > "))
        , symbol (Advanced.Token "> " (Parser.Expecting "> "))
        , symbol (Advanced.Token "   >" (Parser.Expecting "   >"))
        , symbol (Advanced.Token "  >" (Parser.Expecting "  >"))
        , symbol (Advanced.Token " >" (Parser.Expecting " >"))
        , symbol (Advanced.Token ">" (Parser.Expecting ">"))
        ]


blockQuote : Parser RawBlock
blockQuote =
    succeed BlockQuote
        |. blockQuoteStart
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Problem "Expecting end")
            , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
            ]


unorderedListBlock : Parser RawBlock
unorderedListBlock =
    let
        toListItem unparsedListItem =
            case unparsedListItem of
                ListItem.TaskItem completion body ->
                    { body = UnparsedInlines body
                    , task =
                        (case completion of
                            ListItem.Complete ->
                                True

                            ListItem.Incomplete ->
                                False
                        )
                            |> Just
                    }

                ListItem.PlainItem body ->
                    { body = UnparsedInlines body
                    , task = Nothing
                    }
    in
    Markdown.UnorderedList.parser
        |> map (\items -> UnorderedListBlock (List.map toListItem items))


orderedListBlock : Maybe RawBlock -> Parser RawBlock
orderedListBlock lastBlock =
    Markdown.OrderedList.parser lastBlock
        |> map (\( startingIndex, unparsedLines ) -> OrderedListBlock startingIndex (List.map UnparsedInlines unparsedLines))


blankLine : Parser RawBlock
blankLine =
    Advanced.backtrackable (chompWhile (\c -> Helpers.isSpaceOrTab c))
        |. token (Advanced.Token "\n" (Parser.Expecting "\\n"))
        |> map (\() -> BlankLine)


htmlParser : Parser RawBlock
htmlParser =
    HtmlParser.html
        |> xmlNodeToHtmlNode


xmlNodeToHtmlNode : Parser Node -> Parser RawBlock
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                HtmlParser.Text innerText ->
                    Body
                        (UnparsedInlines innerText)
                        |> Advanced.succeed

                HtmlParser.Element tag attributes children ->
                    case nodesToBlocksParser children of
                        Ok parsedChildren ->
                            Block.HtmlElement tag attributes parsedChildren
                                |> RawBlock.Html
                                |> succeed

                        Err err ->
                            problem err

                Comment string ->
                    Block.HtmlComment string
                        |> RawBlock.Html
                        |> succeed

                Cdata string ->
                    Block.Cdata string
                        |> RawBlock.Html
                        |> succeed

                ProcessingInstruction string ->
                    Block.ProcessingInstruction string
                        |> RawBlock.Html
                        |> succeed

                Declaration declarationType content ->
                    Block.HtmlDeclaration declarationType content
                        |> RawBlock.Html
                        |> succeed
        )
        parser


textNodeToBlocks : String -> List Block
textNodeToBlocks textNodeValue =
    textNodeValue
        |> parse
        |> Result.withDefault []


nodeToRawBlock : Node -> Block.Html Block
nodeToRawBlock node =
    case node of
        HtmlParser.Text innerText ->
            Block.HtmlComment "TODO this never happens, but use types to drop this case."

        HtmlParser.Element tag attributes children ->
            let
                parsedChildren : List Block
                parsedChildren =
                    children
                        |> List.concatMap
                            (\child ->
                                case child of
                                    HtmlParser.Text text ->
                                        textNodeToBlocks text

                                    _ ->
                                        [ nodeToRawBlock child |> Block.HtmlBlock ]
                            )
            in
            Block.HtmlElement tag
                attributes
                parsedChildren

        Comment string ->
            Block.HtmlComment string

        Cdata string ->
            Block.Cdata string

        ProcessingInstruction string ->
            Block.ProcessingInstruction string

        Declaration declarationType content ->
            Block.HtmlDeclaration declarationType content


nodesToBlocksParser : List Node -> Result Parser.Problem (List Block)
nodesToBlocksParser children =
    -- Investigate: Rather than `childToParser` making many small lists,
    -- then `concat`'ing them together, could we push all the elements onto the same list
    -- essentially fusing the traverse and the concat
    children
        |> traverseResult childToParser
        |> Result.map List.concat


traverseResult : (a -> Result x b) -> List a -> Result x (List b)
traverseResult f list =
    traverseResultHelper f list []


traverseResultHelper : (a -> Result x b) -> List a -> List b -> Result x (List b)
traverseResultHelper f list accum =
    case list of
        first :: rest ->
            case f first of
                Err e ->
                    Err e

                Ok new ->
                    traverseResultHelper f rest (new :: accum)

        [] ->
            Ok (List.reverse accum)


childToParser : Node -> Result Parser.Problem (List Block)
childToParser node =
    case node of
        Element tag attributes children ->
            case nodesToBlocksParser children of
                Ok childrenAsBlocks ->
                    Ok [ Block.HtmlElement tag attributes childrenAsBlocks |> Block.HtmlBlock ]

                Err err ->
                    Err err

        Text innerText ->
            case parse innerText of
                Ok value ->
                    Ok value

                Err error ->
                    Err
                        (Parser.Expecting
                            (error
                                |> List.map deadEndToString
                                |> String.join "\n"
                            )
                        )

        Comment string ->
            Ok [ Block.HtmlComment string |> Block.HtmlBlock ]

        Cdata string ->
            Ok [ Block.Cdata string |> Block.HtmlBlock ]

        ProcessingInstruction string ->
            Ok [ Block.ProcessingInstruction string |> Block.HtmlBlock ]

        Declaration declarationType content ->
            Ok [ Block.HtmlDeclaration declarationType content |> Block.HtmlBlock ]


type alias LinkReferenceDefinitions =
    List ( String, { destination : String, title : Maybe String } )


type alias State =
    { linkReferenceDefinitions : LinkReferenceDefinitions
    , rawBlocks : List RawBlock
    }


addReference : State -> LinkReferenceDefinition -> State
addReference state linkRef =
    { linkReferenceDefinitions = linkRef :: state.linkReferenceDefinitions
    , rawBlocks = state.rawBlocks
    }


rawBlockParser : Parser State
rawBlockParser =
    loop
        { linkReferenceDefinitions = []
        , rawBlocks = []
        }
        statementsHelp2


parseAllInlines : State -> Result Parser.Problem (List Block)
parseAllInlines state =
    parseAllInlinesHelp state state.rawBlocks []


parseAllInlinesHelp : State -> List RawBlock -> List Block -> Result Parser.Problem (List Block)
parseAllInlinesHelp state rawBlocks parsedBlocks =
    case rawBlocks of
        rawBlock :: rest ->
            case parseInlines state.linkReferenceDefinitions rawBlock of
                ParsedBlock newParsedBlock ->
                    parseAllInlinesHelp state rest (newParsedBlock :: parsedBlocks)

                EmptyBlock ->
                    parseAllInlinesHelp state rest parsedBlocks

                InlineProblem e ->
                    Err e

        [] ->
            Ok parsedBlocks


possiblyMergeBlocks : State -> RawBlock -> State
possiblyMergeBlocks state newRawBlock =
    { linkReferenceDefinitions = state.linkReferenceDefinitions
    , rawBlocks =
        case
            ( newRawBlock
            , state.rawBlocks
            )
        of
            ( CodeBlock block1, (CodeBlock block2) :: rest ) ->
                CodeBlock
                    { body = joinStringsPreserveAll block2.body block1.body
                    , language = Nothing
                    }
                    :: rest

            ( IndentedCodeBlock block1, (IndentedCodeBlock block2) :: rest ) ->
                IndentedCodeBlock (joinStringsPreserveAll block2 block1)
                    :: rest

            ( Body (UnparsedInlines body1), (BlockQuote body2) :: rest ) ->
                BlockQuote (joinRawStringsWith "\n" body2 body1)
                    :: rest

            ( BlockQuote body1, (BlockQuote body2) :: rest ) ->
                BlockQuote (joinStringsPreserveAll body2 body1)
                    :: rest

            ( Body (UnparsedInlines body1), (Body (UnparsedInlines body2)) :: rest ) ->
                Body (UnparsedInlines (joinRawStringsWith "\n" body2 body1))
                    :: rest

            _ ->
                newRawBlock :: state.rawBlocks
    }


statementsHelp2 : State -> Parser (Step State State)
statementsHelp2 revStmts =
    let
        indentedCodeParser =
            case revStmts.rawBlocks of
                (Body _) :: _ ->
                    oneOf []

                _ ->
                    indentedCodeBlock
    in
    oneOf
        [ Advanced.end (Parser.Expecting "End") |> map (\() -> Done revStmts)
        , parseAsParagraphInsteadOfHtmlBlock
            |> map (possiblyMergeBlocks revStmts >> Loop)
        , LinkReferenceDefinition.parser
            |> Advanced.backtrackable
            |> map
                (\linkReference ->
                    linkReference
                        |> addReference revStmts
                        |> Loop
                )
        , map (possiblyMergeBlocks revStmts >> Loop)
            (oneOf
                [ blankLine
                , blockQuote
                , Markdown.CodeBlock.parser |> Advanced.backtrackable |> map CodeBlock
                , indentedCodeParser
                , ThematicBreak.parser |> Advanced.backtrackable |> map (\_ -> ThematicBreak)
                , unorderedListBlock
                , orderedListBlock (List.head revStmts.rawBlocks)
                , heading |> Advanced.backtrackable
                , htmlParser

                -- TODO re-enable this once the table parser handles rows
                --, TableParser.parser |> Advanced.backtrackable |> map Table
                , plainLine
                ]
            )
        ]


{-| HTML parsing is intentionally strict in `dillonkearns/elm-markdown`. Paragraphs are supposed to be forgiving.
This function checks to see if something might be an autolink that could be confused with an HTML block because
the line starts with `<`. But it's slightly more lenient, so that things like `<>` that aren't actually parsed as
autolinks are still parsed as paragraphs.
-}
parseAsParagraphInsteadOfHtmlBlock : Parser RawBlock
parseAsParagraphInsteadOfHtmlBlock =
    -- ^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>
    token (Advanced.Token "<" (Parser.Expecting "<"))
        |. thisIsDefinitelyNotAnHtmlTag
        |. endOfLineOrFile
        |> Advanced.mapChompedString (\rawLine _ -> rawLine |> UnparsedInlines |> Body)
        |> Advanced.backtrackable


endOfLineOrFile =
    Advanced.chompUntilEndOr "\n"
        |. oneOf
            [ Advanced.end (Parser.Expecting "End of input")
            , Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
            ]


thisIsDefinitelyNotAnHtmlTag : Parser ()
thisIsDefinitelyNotAnHtmlTag =
    oneOf
        [ token (Advanced.Token " " (Parser.Expecting " "))
        , token (Advanced.Token ">" (Parser.Expecting ">"))
        , chompIf Char.isAlpha (Parser.Expecting "Alpha")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '-')
            |. oneOf
                [ token (Advanced.Token ":" (Parser.Expecting ":"))
                , token (Advanced.Token "@" (Parser.Expecting "@"))
                , token (Advanced.Token "\\" (Parser.Expecting "\\"))
                , token (Advanced.Token "+" (Parser.Expecting "+"))
                , token (Advanced.Token "." (Parser.Expecting "."))
                ]
        ]


joinStringsPreserveAll string1 string2 =
    string1 ++ "\n" ++ string2


joinRawStringsWith joinWith string1 string2 =
    case ( string1, string2 ) of
        ( "", "" ) ->
            ""

        ( "", _ ) ->
            string2

        ( _, "" ) ->
            string1

        _ ->
            string1 ++ joinWith ++ string2


indentedCodeBlock : Parser RawBlock
indentedCodeBlock =
    succeed IndentedCodeBlock
        |. oneOf
            [ Advanced.symbol (Advanced.Token "    " (Parser.ExpectingSymbol "Indentation"))

            --tabs behave as if they were replaced by 4 spaces in places where spaces define structure
            -- see https://spec.commonmark.org/0.29/#tabs
            , Advanced.symbol (Advanced.Token "\t" (Parser.ExpectingSymbol "Indentation"))
            ]
        |= getChompedString (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
            , Advanced.end (Parser.Expecting "End of input")
            ]


heading : Parser RawBlock
heading =
    succeed Heading
        |. symbol (Advanced.Token "#" (Parser.Expecting "#"))
        |= (getChompedString
                (chompWhile
                    (\c ->
                        case c of
                            '#' ->
                                True

                            _ ->
                                False
                    )
                )
                |> andThen
                    (\additionalHashes ->
                        let
                            level =
                                String.length additionalHashes + 1
                        in
                        if level >= 7 then
                            Advanced.problem (Parser.Expecting "heading with < 7 #'s")

                        else
                            succeed level
                    )
           )
        |. chompWhile Helpers.isSpacebar
        |= (Advanced.chompUntilEndOr "\n"
                |> Advanced.mapChompedString
                    (\headingText _ ->
                        headingText
                            |> dropTrailingHashes
                            |> UnparsedInlines
                    )
           )


dropTrailingHashes headingString =
    if headingString |> String.endsWith "#" then
        String.dropRight 1 headingString
            |> String.trimRight
            |> dropTrailingHashes

    else
        headingString
