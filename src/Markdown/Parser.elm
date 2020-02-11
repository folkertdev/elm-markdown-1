module Markdown.Parser exposing
    ( Renderer, defaultHtmlRenderer, deadEndToString, parse, render
    , ListItem(..), TaskStatus(..)
    )

{-|

@docs Renderer, defaultHtmlRenderer, deadEndToString, parse, render

-}

import Helpers
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.CodeBlock
import Markdown.Html
import Markdown.HtmlRenderer
import Markdown.Inlines as Inlines
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompUntil, chompWhile, getChompedString, inContext, int, lazy, loop, map, multiComment, oneOf, problem, succeed, symbol, token)
import Parser.Extra exposing (oneOrMore, zeroOrMore)
import XmlParser exposing (Node(..))


{-| A record with functions that define how to render all possible markdown blocks.
These renderers are composed together to give you the final rendered output.

You could render to any type you want. Here are some useful things you might render to:

  - `Html` (using the `defaultHtmlRenderer` provided by this module)
  - Custom `Html`
  - `Element`s from [`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
  - Types from other custom HTML replacement libraries, like [`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/) or [`tesk9/accessible-html`](https://package.elm-lang.org/packages/tesk9/accessible-html/latest/)
  - Raw `String`s with [ANSI color codes](http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html) for setting rich colors in terminal (CLI) output
  - Plain text with any formatting stripped away (maybe for a String search feature)

-}
type alias Renderer view =
    { heading : { level : Int, rawText : String, children : List view } -> view
    , raw : List view -> view
    , blockQuote : List view -> view
    , html : Markdown.Html.Renderer (List view -> view)
    , plain : String -> view
    , code : String -> view
    , bold : String -> view
    , italic : String -> view
    , link : { title : Maybe String, destination : String } -> List view -> Result String view
    , image : { src : String } -> String -> Result String view
    , unorderedList : List (ListItem view) -> view
    , orderedList : Int -> List (List view) -> view
    , codeBlock : { body : String, language : Maybe String } -> view
    , thematicBreak : view
    }


type TaskStatus
    = Complete
    | Incomplete


type ListItem view
    = TaskItem TaskStatus (List view)
    | NonTaskItem (List view)


{-| This renders `Html` in an attempt to be as close as possible to
the HTML output in <https://github.github.com/gfm/>.
-}
defaultHtmlRenderer : Renderer (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                1 ->
                    Html.h1 [] children

                2 ->
                    Html.h2 [] children

                3 ->
                    Html.h3 [] children

                4 ->
                    Html.h4 [] children

                5 ->
                    Html.h5 [] children

                6 ->
                    Html.h6 [] children

                _ ->
                    Html.text "TODO maye use a type here to clean it up... this will never happen"
    , raw = Html.p []
    , blockQuote = Html.blockquote []
    , bold =
        \content -> Html.strong [] [ Html.text content ]
    , italic =
        \content -> Html.em [] [ Html.text content ]
    , code =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            Html.a [ Attr.href link.destination ] content
                |> Ok
    , image =
        \image content ->
            Html.img [ Attr.src image.src ] [ Html.text content ]
                |> Ok
    , plain =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                TaskItem status body ->
                                    let
                                        checkbox =
                                            Html.input
                                                [ Attr.disabled True
                                                , Attr.checked
                                                    (case status of
                                                        Complete ->
                                                            True

                                                        Incomplete ->
                                                            False
                                                    )
                                                , Attr.type_ "checkbox"
                                                ]
                                                []
                                    in
                                    Html.li [] (checkbox :: body)

                                NonTaskItem body ->
                                    Html.li [] body
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    }


renderStyled : Renderer view -> List Inline -> Result String (List view)
renderStyled renderer styledStrings =
    styledStrings
        |> List.foldr (foldThing renderer) []
        |> combineResults


foldThing : Renderer view -> Inline -> List (Result String view) -> List (Result String view)
foldThing renderer { style, string } soFar =
    case style.link of
        Just link ->
            case link.destination of
                Block.Link destination ->
                    case Advanced.run Inlines.parse string of
                        Ok styledLine ->
                            (renderStyled renderer styledLine
                                |> Result.andThen
                                    (\children ->
                                        renderer.link { title = link.title, destination = destination } children
                                    )
                            )
                                :: soFar

                        Err error ->
                            (error |> List.map deadEndToString |> List.map Err)
                                ++ soFar

                Block.Image src ->
                    renderer.image { src = src } string
                        :: soFar

        Nothing ->
            if style.isBold then
                (renderer.bold string |> Ok)
                    :: soFar

            else if style.isItalic then
                (renderer.italic string |> Ok)
                    :: soFar

            else if style.isCode then
                (renderer.code string |> Ok)
                    :: soFar

            else
                (renderer.plain string |> Ok)
                    :: soFar


renderHelper :
    Renderer view
    -> List Block
    -> List (Result String view)
renderHelper renderer blocks =
    List.map
        (\block ->
            case block of
                Block.Heading level content ->
                    renderStyled renderer content
                        |> Result.map
                            (\children ->
                                renderer.heading
                                    { level = level, rawText = Inlines.toString content, children = children }
                            )

                Block.Body content ->
                    renderStyled renderer content
                        |> Result.map renderer.raw

                Block.Html tag attributes children ->
                    renderHtmlNode renderer tag attributes children

                Block.UnorderedListBlock items ->
                    items
                        |> List.map
                            (\item ->
                                renderStyled renderer item.body
                                    |> Result.map
                                        (\renderedBody ->
                                            case item.task of
                                                Just complete ->
                                                    TaskItem
                                                        (case complete of
                                                            True ->
                                                                Complete

                                                            False ->
                                                                Incomplete
                                                        )
                                                        renderedBody

                                                Nothing ->
                                                    NonTaskItem renderedBody
                                        )
                            )
                        |> combineResults
                        |> Result.map renderer.unorderedList

                Block.OrderedListBlock startingIndex items ->
                    items
                        |> List.map (renderStyled renderer)
                        |> combineResults
                        |> Result.map (renderer.orderedList startingIndex)

                Block.CodeBlock codeBlock ->
                    codeBlock
                        |> renderer.codeBlock
                        |> Ok

                Block.ThematicBreak ->
                    Ok renderer.thematicBreak

                Block.BlockQuote nestedBlocks ->
                    renderHelper renderer nestedBlocks
                        |> combineResults
                        |> Result.map renderer.blockQuote
        )
        blocks


{-| Apply a `Markdown.Parser.Renderer` to turn parsed `Block`s into your rendered
markdown view.
-}
render :
    Renderer view
    -> List Block
    -> Result String (List view)
render renderer ast =
    ast
        |> renderHelper renderer
        |> combineResults


renderHtml :
    String
    -> List Attribute
    -> List Block
    -> Markdown.Html.Renderer (List view -> view)
    -> List (Result String view)
    -> Result String view
renderHtml tagName attributes children (Markdown.HtmlRenderer.HtmlRenderer htmlRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> Result.andThen
            (\okChildren ->
                htmlRenderer tagName attributes children
                    |> Result.map
                        (\myRenderer -> myRenderer okChildren)
            )


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


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


renderHtmlNode : Renderer view -> String -> List Attribute -> List Block -> Result String view
renderHtmlNode renderer tag attributes children =
    renderHtml tag
        attributes
        children
        renderer.html
        (renderHelper renderer children)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parseInlines : RawBlock -> Parser (Maybe Block)
parseInlines rawBlock =
    case rawBlock of
        Heading level (UnparsedInlines unparsedInlines) ->
            case Advanced.run Inlines.parse unparsedInlines of
                Ok styledLine ->
                    just (Block.Heading level styledLine)

                Err error ->
                    problem (Parser.Expecting (error |> List.map deadEndToString |> String.join "\n"))

        Body (UnparsedInlines unparsedInlines) ->
            case Advanced.run Inlines.parse unparsedInlines of
                Ok styledLine ->
                    just (Block.Body styledLine)

                Err error ->
                    problem (Parser.Expecting (error |> List.map deadEndToString |> String.join "\n"))

        Html tagName attributes children ->
            Block.Html tagName attributes children
                |> just

        UnorderedListBlock unparsedItems ->
            unparsedItems
                |> List.map
                    (\unparsedItem ->
                        unparsedItem.body
                            |> parseRawInline identity
                            |> Advanced.map
                                (\parsedInlines ->
                                    { task = unparsedItem.task
                                    , body = parsedInlines
                                    }
                                )
                    )
                |> combine
                |> map Block.UnorderedListBlock
                |> map Just

        OrderedListBlock startingIndex unparsedInlines ->
            unparsedInlines
                |> List.map (parseRawInline identity)
                |> combine
                |> map (Block.OrderedListBlock startingIndex)
                |> map Just

        CodeBlock codeBlock ->
            Block.CodeBlock codeBlock
                |> just

        ThematicBreak ->
            just Block.ThematicBreak

        BlankLine ->
            succeed Nothing

        BlockQuote rawBlocks ->
            case Advanced.run rawBlockParser rawBlocks of
                Ok value ->
                    parseAllInlines value
                        |> map
                            (\parsedBlocks ->
                                Block.BlockQuote parsedBlocks
                                    |> Just
                            )

                Err error ->
                    Advanced.problem (Parser.Problem (deadEndsToString error))


just value =
    succeed (Just value)


parseRawInline : (List Inline -> a) -> UnparsedInlines -> Advanced.Parser c Parser.Problem a
parseRawInline wrap (UnparsedInlines unparsedInlines) =
    case Advanced.run Inlines.parse unparsedInlines of
        Ok styledLine ->
            succeed (wrap styledLine)

        Err error ->
            problem (Parser.Expecting (error |> List.map deadEndToString |> String.join "\n"))


plainLine : Parser RawBlock
plainLine =
    succeed
        (\rawLine ->
            rawLine
                |> UnparsedInlines
                |> Body
        )
        |. oneOf
            [ token (Advanced.Token "   " (Parser.Expecting "   "))
            , token (Advanced.Token "  " (Parser.Expecting "  "))
            , token (Advanced.Token " " (Parser.Expecting " "))
            , succeed ()
            ]
        |= innerParagraphParser
        |. oneOf
            [ Advanced.chompIf Helpers.isNewline (Parser.Expecting "A single non-newline char.")
            , Advanced.end (Parser.Expecting "End")
            ]


innerParagraphParser =
    getChompedString <|
        succeed ()
            |. Advanced.chompIf (\c -> not <| Helpers.isSpaceOrTab c && (not <| Helpers.isNewline c)) (Parser.Expecting "Not a space or tab.")
            |. Advanced.chompUntilEndOr "\n"


blockQuote : Parser RawBlock
blockQuote =
    succeed BlockQuote
        |. oneOf
            [ symbol (Advanced.Token "   > " (Parser.Expecting "   > "))
            , symbol (Advanced.Token "  > " (Parser.Expecting "  > "))
            , symbol (Advanced.Token " > " (Parser.Expecting " > "))
            , symbol (Advanced.Token "> " (Parser.Expecting "> "))
            , symbol (Advanced.Token "   >" (Parser.Expecting "   >"))
            , symbol (Advanced.Token "  >" (Parser.Expecting "  >"))
            , symbol (Advanced.Token " >" (Parser.Expecting " >"))
            , symbol (Advanced.Token ">" (Parser.Expecting ">"))
            ]
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Problem "Expecting end")
            , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
            ]


unorderedListBlock : Parser RawBlock
unorderedListBlock =
    Markdown.UnorderedList.parser
        |> map
            (List.map
                (\unparsedListItem ->
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
                            , task = Nothing -- unparsedListItem.task
                            }
                )
            )
        |> map UnorderedListBlock


orderedListBlock : Maybe RawBlock -> Parser RawBlock
orderedListBlock lastBlock =
    Markdown.OrderedList.parser lastBlock
        |> map (\( startingIndex, unparsedLines ) -> OrderedListBlock startingIndex (List.map UnparsedInlines unparsedLines))


blankLine : Parser RawBlock
blankLine =
    token (Advanced.Token "\n" (Parser.Expecting "\\n"))
        |. chompWhile Helpers.isNewline
        |> map (\() -> BlankLine)


htmlParser : Parser RawBlock
htmlParser =
    XmlParser.element
        |> xmlNodeToHtmlNode


xmlNodeToHtmlNode : Parser Node -> Parser RawBlock
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                XmlParser.Text innerText ->
                    -- TODO is this right?
                    Body
                        (UnparsedInlines innerText)
                        |> Advanced.succeed

                XmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Advanced.succeed
                                (Html tag
                                    attributes
                                    parsedChildren
                                )
                        )
                        (nodesToBlocksParser children)
        )
        parser


nodesToBlocksParser : List Node -> Parser (List Block)
nodesToBlocksParser children =
    children
        |> List.map childToParser
        |> combine
        |> Advanced.map List.concat


combine : List (Parser a) -> Parser (List a)
combine list =
    list
        |> List.foldr
            (\parser listParser ->
                listParser
                    |> Advanced.andThen
                        (\soFar ->
                            parser
                                |> Advanced.map (\a -> a :: soFar)
                        )
            )
            (Advanced.succeed [])


childToParser : Node -> Parser (List Block)
childToParser node =
    case node of
        Element tag attributes children ->
            nodesToBlocksParser children
                |> Advanced.andThen
                    (\childrenAsBlocks ->
                        Advanced.succeed [ Block.Html tag attributes childrenAsBlocks ]
                    )

        Text innerText ->
            case Advanced.run multiParser2 innerText of
                Ok value ->
                    succeed value

                Err error ->
                    Advanced.problem
                        (Parser.Expecting
                            (error
                                |> List.map deadEndToString
                                |> String.join "\n"
                            )
                        )


multiParser2 : Parser (List Block)
multiParser2 =
    rawBlockParser
        |. succeed Advanced.end
        |> andThen parseAllInlines
        -- TODO find a more elegant way to exclude empty blocks for each blank lines
        |> map
            (List.filter
                (\item ->
                    case item of
                        Block.Body [] ->
                            False

                        _ ->
                            True
                )
            )


rawBlockParser : Parser (List RawBlock)
rawBlockParser =
    loop [] statementsHelp2


parseAllInlines : List RawBlock -> Parser (List Block)
parseAllInlines rawBlocks =
    List.foldl combineBlocks (succeed []) rawBlocks


combineBlocks : RawBlock -> Parser (List Block) -> Parser (List Block)
combineBlocks rawBlock soFar =
    soFar
        |> andThen
            (\parsedBlocks ->
                rawBlock
                    |> parseInlines
                    |> map
                        (\maybeNewParsedBlock ->
                            case maybeNewParsedBlock of
                                Just newParsedBlock ->
                                    newParsedBlock :: parsedBlocks

                                Nothing ->
                                    parsedBlocks
                        )
            )


statementsHelp2 : List RawBlock -> Parser (Step (List RawBlock) (List RawBlock))
statementsHelp2 revStmts =
    let
        keepLooping parser =
            parser
                |> map
                    (\stmts ->
                        case
                            ( stmts
                            , revStmts
                            )
                        of
                            ( CodeBlock block1, (CodeBlock block2) :: rest ) ->
                                (CodeBlock
                                    { body = joinStringsPreserveIndentation block2.body block1.body
                                    , language = Nothing
                                    }
                                    :: rest
                                )
                                    |> Loop

                            ( Body (UnparsedInlines body1), (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinRawStringsWith "\n" body2 body1)
                                    :: rest
                                )
                                    |> Loop

                            ( BlockQuote body1, (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinRawStringsWith "\n" body2 body1)
                                    :: rest
                                )
                                    |> Loop

                            ( BlockQuote children1, rest ) ->
                                Loop (stmts :: revStmts)

                            ( Body (UnparsedInlines body1), (Body (UnparsedInlines body2)) :: rest ) ->
                                Loop
                                    (Body (UnparsedInlines (joinRawStringsWith " " body2 body1))
                                        :: rest
                                    )

                            _ ->
                                Loop (stmts :: revStmts)
                    )
    in
    oneOf
        [ Advanced.end (Parser.Expecting "End") |> map (\() -> Done revStmts)
        , blankLine |> keepLooping
        , blockQuote |> keepLooping
        , Markdown.CodeBlock.parser |> map CodeBlock |> keepLooping
        , thematicBreak |> keepLooping
        , unorderedListBlock |> keepLooping
        , orderedListBlock (List.head revStmts) |> keepLooping
        , heading |> keepLooping
        , htmlParser |> keepLooping
        , plainLine |> keepLooping
        , succeed (Done revStmts)
        ]


joinStringsPreserveIndentation string1 string2 =
    let
        string1Trimmed =
            String.trimRight string1

        string2Trimmed =
            String.trimRight string2
    in
    String.concat
        [ string1Trimmed
        , "\n"
        , string2Trimmed
        ]


joinRawStringsWith joinWith string1 string2 =
    let
        string1Trimmed =
            String.trim string1

        string2Trimmed =
            String.trim string2
    in
    case ( string1Trimmed, string2Trimmed ) of
        ( "", "" ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        ( "", _ ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        ( _, "" ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        _ ->
            String.concat
                [ string1Trimmed
                , joinWith
                , string2Trimmed
                ]


thematicBreak : Parser RawBlock
thematicBreak =
    succeed ThematicBreak
        |. Advanced.backtrackable
            (oneOf
                [ symbol (Advanced.Token "   " (Parser.Problem "Expecting 3 spaces"))
                , symbol (Advanced.Token "  " (Parser.Problem "Expecting 2 spaces"))
                , symbol (Advanced.Token " " (Parser.Problem "Expecting space"))
                , succeed ()
                ]
            )
        |. oneOf
            [ symbol (Advanced.Token "---" (Parser.Expecting "---"))
                |. chompWhile
                    (\c ->
                        case c of
                            '-' ->
                                True

                            _ ->
                                False
                    )
            , symbol (Advanced.Token "***" (Parser.Expecting "***"))
                |. chompWhile
                    (\c ->
                        case c of
                            '*' ->
                                True

                            _ ->
                                False
                    )
            , symbol (Advanced.Token "___" (Parser.Expecting "___"))
                |. chompWhile
                    (\c ->
                        case c of
                            '_' ->
                                True

                            _ ->
                                False
                    )
            ]
        |. zeroOrMore Helpers.isSpaceOrTab
        |. oneOf
            [ Advanced.end (Parser.Problem "Expecting end")
            , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
            ]


heading : Parser RawBlock
heading =
    succeed Heading
        |. symbol (Advanced.Token "#" (Parser.Expecting "#"))
        |= (getChompedString
                (succeed ()
                    |. chompWhile
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
        |= (getChompedString
                (succeed ()
                    |. Advanced.chompUntilEndOr "\n"
                )
                |> Advanced.andThen
                    (\headingText ->
                        headingText
                            |> dropTrailingHashes
                            |> UnparsedInlines
                            |> succeed
                    )
           )


dropTrailingHashes headingString =
    if headingString |> String.endsWith "#" then
        String.dropRight 1 headingString
            |> String.trimRight
            |> dropTrailingHashes

    else
        headingString


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
    Advanced.run
        multiParser2
        input
