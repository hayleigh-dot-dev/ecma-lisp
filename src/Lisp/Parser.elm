module Lisp.Parser exposing
  ( run
  , program
  , expression
  , value
  , keyword
  )

{-| Imports -------------------------------------------------------------------}
import Lisp.Expression exposing 
  ( Expression (..)
  , Value (..)
  , Keyword (..)
  , ImportExpression (..)
  )
import Parser exposing (Parser, (|=), (|.))

run : String -> Result (List Parser.DeadEnd) (List Expression)
run input =
  Parser.run program input

{-| Program -------------------------------------------------------------------}
program : Parser (List Expression)
program =
  Parser.loop [] (\exprs ->
    Parser.oneOf
      [ Parser.succeed (\expr -> Parser.Loop (expr :: exprs))
          |. Parser.spaces
          |= expression
          |. Parser.spaces
      , Parser.succeed (\_ -> Parser.Done (List.reverse exprs))
          |. Parser.spaces
          |= Parser.end
      ]
  )

{-| Expression ----------------------------------------------------------------}
expression : Parser Expression
expression =
  Parser.oneOf
    [ Parser.backtrackable <| Parser.lazy (\_ -> function)
    , Parser.backtrackable <| Parser.lazy (\_ -> list)
    , Parser.backtrackable <| Parser.lazy (\_ -> symbol)
    , Parser.backtrackable <| Parser.lazy (\_ -> literal)
    , Parser.backtrackable <| reference
    ]

literal : Parser Expression
literal =
  Parser.succeed Literal
    |= value

reference : Parser Expression
reference =
  let 
    validCharacters = 
      ['>', '<', '@', '#', '-', '+', '=', '*', '_', '|', '\\', '/', '.']
  in
    Parser.map Reference
      <| Parser.getChompedString
      <| Parser.succeed ()
      |. Parser.chompIf (\c -> Char.isAlphaNum c|| List.member c validCharacters)
      |. Parser.chompWhile (\c -> Char.isAlphaNum c || List.member c validCharacters)

symbol : Parser Expression
symbol =
  Parser.succeed Symbol
    |= keyword

list : Parser Expression
list =
  Parser.succeed List |= (Parser.backtrackable <| Parser.sequence
    { start = "("
    , separator = ""
    , end = ")"
    , spaces = Parser.succeed ()
    , item = Parser.succeed identity
        |= expression 
        |. Parser.spaces
    , trailing = Parser.Optional
    })

function : Parser Expression
function =
  Parser.succeed Function
    |. Parser.token "("
    |. Parser.token "=>"
    |. Parser.spaces
    |= (Parser.backtrackable <| Parser.sequence
      { start = "("
      , separator = ""
      , end = ")"
      , spaces = Parser.succeed ()
      , item =
          Parser.succeed identity
            |= Parser.getChompedString (Parser.succeed ()
            |. Parser.chompIf Char.isAlpha
            |. Parser.chompWhile Char.isAlphaNum)
            |. Parser.spaces
      , trailing = Parser.Optional
      })
    |. Parser.spaces
    |= expression
    |. Parser.spaces
    |. Parser.token ")"

{-| Value ---------------------------------------------------------------------}
value : Parser Value
value =
  Parser.oneOf
    [ Parser.backtrackable <| number
    , Parser.backtrackable <| boolean
    , Parser.backtrackable <| string
    , Parser.backtrackable <| undefined
    , Parser.backtrackable <| Parser.lazy (\_ -> object)
    , Parser.backtrackable <| Parser.lazy (\_ -> array)
    , Parser.backtrackable <| null 
    ]

number : Parser Value
number =
  Parser.oneOf
    [ Parser.succeed Number 
        |= Parser.float
    , Parser.succeed (Number << Basics.toFloat) 
        |= Parser.int
    ]

boolean : Parser Value
boolean =
  Parser.oneOf
    [ Parser.succeed (Boolean << always True) 
        |= Parser.keyword "true"
    , Parser.succeed (Boolean << always False) 
        |= Parser.keyword "false"
    ]

string : Parser Value
string =
  Parser.succeed String |. Parser.token "'" |= Parser.loop [] (\segments ->
    Parser.oneOf
      [ Parser.succeed (\s -> Parser.Loop (s :: segments))
          |. Parser.token "\\"
          |= Parser.oneOf
              [ Parser.map (\_ -> "\n") (Parser.token "n")
              , Parser.map (\_ -> "\t") (Parser.token "t")
              , Parser.map (\_ -> "\r") (Parser.token "r")
              ]
      , Parser.token "'"
          |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse segments)))
      , Parser.chompWhile (\c ->  c /= '\\' && c /= '\'')
          |> Parser.getChompedString
          |> Parser.map (\s -> Parser.Loop (s :: segments))
      ]
  )

undefined : Parser Value
undefined =
  Parser.succeed (\_ -> Undefined)
    |= Parser.keyword "undefined"

object : Parser Value
object =
  Parser.succeed Object |= Parser.sequence
    { start = "{"
    , separator = " "
    , end = "}"
    , spaces = Parser.spaces
    , item =
        Parser.succeed Tuple.pair
          |= value
          |. Parser.spaces
          |= expression
    , trailing = Parser.Optional
    }

array : Parser Value
array =
  Parser.succeed Array |= Parser.sequence
    { start = "["
    , separator = ""
    , end = "]"
    , spaces = Parser.succeed ()
    , item = Parser.succeed identity
        |= expression
        |. Parser.spaces
    , trailing = Parser.Optional
    }

null : Parser Value
null =
  Parser.succeed (\_ -> Null)
    |= Parser.keyword "null"

{-| Keyword -------------------------------------------------------------------}
keyword : Parser Keyword
keyword =
  Parser.oneOf
    [ Parser.backtrackable <| Parser.lazy (\_ -> return)
    , Parser.backtrackable <| Parser.lazy (\_ -> block)
    , Parser.backtrackable <| Parser.lazy (\_ -> let_)
    , Parser.backtrackable import_
    , Parser.backtrackable <| Parser.lazy (\_ -> export)
    ]

return : Parser Keyword
return =
  Parser.succeed Return
    |. Parser.keyword "return"
    |. Parser.spaces
    |= Parser.lazy (\_ -> expression)

block : Parser Keyword
block =
  Parser.succeed Block
    |. Parser.keyword "block"
    |. Parser.spaces
    |= Parser.sequence
      { start = "("
      , separator = ""
      , end = ")"
      , spaces = Parser.succeed ()
      , item = Parser.succeed identity
          |= expression
          |. Parser.spaces
      , trailing = Parser.Optional
      }

let_ : Parser Keyword
let_ =
  Parser.succeed Let
    |. Parser.keyword "let"
    |. Parser.spaces
    |= Parser.getChompedString (Parser.succeed ()
    |. Parser.chompIf Char.isAlpha
    |. Parser.chompWhile Char.isAlphaNum)
    |. Parser.spaces
    |= expression

import_ : Parser Keyword
import_ =
  Parser.succeed Import |= Parser.oneOf
    [ Parser.backtrackable qualified
    , Parser.backtrackable destructured
    ]

export : Parser Keyword
export =
  Parser.succeed Export
    |. Parser.keyword "export"
    |. Parser.spaces
    |= expression

{-| Import expression ---------------------------------------------------------}
qualified : Parser ImportExpression
qualified =
  Parser.succeed Qualified
    |. Parser.keyword "import"
    |. Parser.spaces
    |= Parser.getChompedString ((Parser.succeed ()
    |. Parser.chompIf Char.isAlpha
    |. Parser.chompWhile Char.isAlphaNum
    ))
    |. Parser.spaces
    |. Parser.token "'"
    |= Parser.loop [] (\segments ->
        Parser.oneOf
          [ Parser.token "'"
              |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse segments)))
          , Parser.chompWhile (\c ->  c /= '\\' && c /= '\'')
              |> Parser.getChompedString
              |> Parser.map (\s -> Parser.Loop (s :: segments))
          ]
      )  

destructured : Parser ImportExpression
destructured =
  Parser.succeed Destructured
    |. Parser.keyword "import"
    |. Parser.spaces
    |= Parser.sequence
      { start = "("
      , separator = " "
      , end = ")"
      , spaces = Parser.succeed ()
      , item = Parser.getChompedString ((Parser.succeed ()
          |. Parser.chompIf Char.isAlpha
          |. Parser.chompWhile Char.isAlphaNum
        ))
      , trailing = Parser.Optional
      }
    |. Parser.spaces
    |. Parser.token "'"
    |= Parser.loop [] (\segments ->
        Parser.oneOf
          [ Parser.token "'"
              |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse segments)))
          , Parser.chompWhile (\c ->  c /= '\\' && c /= '\'')
              |> Parser.getChompedString
              |> Parser.map (\s -> Parser.Loop (s :: segments))
          ]
      )  