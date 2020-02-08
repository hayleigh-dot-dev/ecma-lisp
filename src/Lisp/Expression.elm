module Lisp.Expression exposing
  ( Expression (..)
  , Value (..)
  , Keyword (..)
  , ImportExpression (..)
  )

{-| Imports -------------------------------------------------------------------}


{-| Expression ----------------------------------------------------------------}
type Expression
  = Literal Value
  | Reference String
  | Symbol Keyword
  | List (List Expression)
  | Function (List String) Expression

{-| Value ---------------------------------------------------------------------}
type Value
  -- primitives
  = Number Float
  | Boolean Bool
  | String String
  | Undefined
  -- objects
  | Object (List (Value, Expression))
  | Array (List Expression)
  | Null

{-| Keyword -------------------------------------------------------------------}
type Keyword
  = Return Expression
  | Block (List Expression)
  | Let String Expression
  | Import ImportExpression
  | Export Expression

{-| Import expression ---------------------------------------------------------}
type ImportExpression
  = Qualified String String
  | Destructured (List String) String