module Types
  ( Parser,
    ParserError,
    Primitive(..),
    Operator(..),
    Expression(..),
    Formula(..),
  )
where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void
import Data.Text (Text)


type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

data Primitive
  = DText Text
  | DBool Bool
  | DNumber Int
  deriving (Show, Eq)

data Operator = Add | Subtract | Multiply | Divide | And | Or
  deriving (Show, Eq)

data Expression = Expression Primitive Operator Primitive
  deriving (Show, Eq)

data Formula = DExpression Expression | DPrimitive Primitive
  deriving (Show)
