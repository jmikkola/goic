module Main where

import Control.Monad.State (StateT, get, put, lift, evalStateT, runStateT)

-- import Control.Applicative ((<|>))

main :: IO ()
main = putStrLn "Hello, Haskell!"

--
-- Define the types for the language's AST
--

data Program = Program [Module]
    deriving (Show)

data Module = Module String [Function]
    deriving (Show)

data Function = Function String FnType Statement
    deriving (Show)

data Statement
    = EStatement Expression
    | Return Expression
    | BareReturn
    | NewVar String Expression
    | If Expression Statement Statement
    | While Expression Statement
    | Block [Statement]
    deriving (Show)

data Expression
    = Literal Value
    | BinaryOp Op Expression Expression
    | Call Expression [Expression]
    deriving (Show)

data Value
    = VInt Int
    | VString String
    deriving (Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    | And
    | Or
    deriving (Show)

data FnType = FnType [Type] Type
    deriving (Show)

data Type
    = Func FnType
    | Void
    | Int
    | String
    | Char
    deriving (Show)

--
-- Parser for the basic language
--


type Err = String
type ParseM = StateT String (Either Err)
type Parser a = ParseM a

parse :: Parser a -> String -> Either Err a
parse p text = evalStateT p text

_parsed :: a -> String -> Parser a
_parsed p current = do
  put current
  return p

_err :: String -> Parser a
_err = lift . Left


char :: Char -> Parser Char
char c = do
  text <- get
  case text of
    (c2:rest) | c == c2 -> _parsed c rest
    _                   -> _err $ "Cannot match char " ++ [c]


oneOf :: [Char] -> Parser Char
oneOf options = matchOneOf options
  where matchOneOf []     = _err $ "Cannot match any of " ++ options
        matchOneOf (c:cs) = do
          text <- get
          case text of
           (c':rest) | c' == c -> _parsed c rest
           _                   -> matchOneOf cs

optional :: Parser a -> Parser (Maybe a)
optional p = do
  text <- get
  let result = runStateT p text
  case result of
    Left  _         -> return Nothing
    Right (r, rest) -> _parsed (Just r) rest

many :: Parser a -> Parser [a]
many p = do
  parsed <- optional p
  case parsed of
    Nothing -> return []
    Just x -> do
      more <- many p
      return (x : more)

many1 :: Parser a -> Parser [a]
many1 p = do
  parsed <- p
  more <- many p
  return (parsed : more)

digit :: Parser Char
digit = oneOf ['0' .. '9']

digits :: Parser String
digits = many1 digit
