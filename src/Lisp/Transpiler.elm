module Lisp.Transpiler exposing
  ( run
  , program
  , expression
  )

{-| Imports -------------------------------------------------------------------}
import Lisp.Expression exposing 
  ( Expression (..)
  , Value (..)
  , Keyword (..)
  , ImportExpression (..)
  )

run : List Expression -> String
run prgm =
  case prgm of
    expr :: [] ->
      expression expr

    expr :: exprs ->
      program (expr :: exprs)

    [] ->
      ""

{-| Program -------------------------------------------------------------------}
program : List Expression -> String
program prgm =
  List.map expression prgm
    |> String.join "\n"

{-| Expression ----------------------------------------------------------------}
expression : Expression -> String
expression expr =
  case expr of
    Literal value ->
      literal value

    Reference ref ->
      reference ref

    Symbol keyword ->
      symbol keyword

    List (Symbol keyword :: []) ->
      symbol keyword

    List (fn :: []) ->
      "(" ++ expression fn ++ ")"

    List (fn :: args) ->
      expression fn ++ "(" ++ (List.map expression args |> String.join ", ") ++ ")"

    List [] ->
      "(void 0)"

    Function args expr_ ->
      function args expr_

literal : Value -> String
literal value =
  case value of
    Number n ->
      number n

    Boolean b ->
      boolean b

    String s ->
      string s

    Undefined ->
      undefined

    Object keyValuePairs ->
      object keyValuePairs

    Array items ->
      array items

    Null ->
      null

reference : String -> String
reference ref =
  String.replace "-" "_" ref

symbol : Keyword -> String
symbol keyword =
  case keyword of
    Return expr ->
      return expr

    Block exprs ->
      block exprs

    Let var expr ->
      let_ var expr

    Import importExpr ->
      import_ importExpr

    Export expr ->
      export expr

{-| Value ---------------------------------------------------------------------}
function : List String -> Expression -> String
function args expr =
  "(" ++ String.join ", " args ++ ") => " ++ expression expr

number : Float -> String
number n =
  String.fromFloat n

boolean : Bool -> String
boolean b =
  if b then "true" else "false"

string : String -> String
string s =
  "'" ++ s ++ "'"

undefined : String
undefined =
  "undefined"

object : List (Value, Expression) -> String
object keyValuePairs =
  "{ " 
    ++ (List.map (\(k, v) -> "[" ++ literal k ++ "]: " ++ expression v) keyValuePairs 
    |> String.join ", ")
    ++ " }"

array : List Expression -> String
array items =
  "[ "
    ++ (List.map expression items 
    |> String.join ", ")
    ++ " ]"

null : String
null =
  "null"

{-| Keyword -------------------------------------------------------------------}
return : Expression -> String
return expr =
  "return " ++ expression expr

block : List Expression -> String
block exprs =
  "{ " ++ (List.map expression exprs |> String.join "; ") ++ " }"

let_ : String -> Expression -> String
let_ var expr =
  "let " ++ var ++ " = " ++ expression expr

import_ : ImportExpression -> String
import_ importExpression =
  case importExpression of
    Qualified alias_ path ->
      qualified alias_ path

    Destructured refs path ->
      destructured refs path

export : Expression -> String
export expr =
  "export " ++ expression expr

{-| Import expression ---------------------------------------------------------}
qualified : String -> String -> String
qualified alias_ path =
  "import * as " ++ alias_ ++ " from " ++ path

destructured : List String -> String -> String
destructured refs path =
  "import { " ++ String.join ", " refs ++ " } from '" ++ path ++ "'"