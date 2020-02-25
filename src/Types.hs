module Types
  ( Parser,
    ParserError,
    Primitive (..),
    Operator (..),
    Expression (..),
    ExpressionPart (..),
    FunctionName (..),
    FunctionArg (..),
    Function (..),
    Formula (..),
  )
where

import Data.Either
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

data Primitive
  = DText Text
  | DBool Bool
  | DNumber Double
  deriving (Show, Eq)

data Operator = Add | Subtract | Multiply | Divide | And | Or
  deriving (Show, Eq)

data ExpressionPart = DExPrimitive Primitive | DExOperator Operator
  deriving (Show, Eq)

newtype Expression = Expression [ExpressionPart]
  deriving (Show, Eq)

newtype FunctionName = DFunctionName Text
  deriving (Show, Eq)

data FunctionArg = DFnPrimitive Primitive | DFnExpression Expression
  deriving (Show, Eq)

data Function = Function FunctionName [FunctionArg]
  deriving (Show, Eq)

data Formula = DFunction Function | DExpression Expression | DPrimitive Primitive
  deriving (Show)
