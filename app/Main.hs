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
    | Variable String
    | BinaryOp Op Expression Expression
    | Paren Expression
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


string :: String -> Parser String
string s = do
  _string s
  return s

_string :: String -> Parser ()
_string [] = return ()
_string (c:cs) = do
  char c
  _string cs

oneOf :: [Char] -> Parser Char
oneOf options = do
  text <- get
  case text of
    (c:rest) | elem c options -> _parsed c rest
    _                         -> _err $ "Cannot match any of " ++ options


noneOf :: [Char] -> Parser Char
noneOf options = do
  text <- get
  case text of
    (c:rest) | not (elem c options) -> _parsed c rest
    _                               -> _err $ "Character was one of " ++ options


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

options :: [Parser a] -> Parser a
options parsers = tryOption parsers
  where tryOption []     = _err "no parsers given to options"
        tryOption [p]    = p
        tryOption (p:ps) = do
          -- save the current value of text
          text <- get
          let result = runStateT p text
          case result of
            Left _  -> do
              -- that parser failed, so restore the value of text before trying the next one
              put text
              tryOption ps
            Right (r, _) ->
              return r

-- this succeeds if the given parser fails
non :: Parser a -> Parser ()
non p = do
  text <- get
  let result = runStateT p text
  case result of
    Left _ -> do
      put text
      return ()
    Right _ ->
      _err "inner parser succeeded"

digit :: Parser Char
digit = oneOf ['0' .. '9']

digits :: Parser String
digits = many1 digit

letter :: Parser Char
letter = oneOf (['a'..'z'] ++ ['A'..'Z'])

letters :: Parser String
letters = many1 letter

----

intValue :: Parser Value
intValue = do
  parsed <- digits
  return $ VInt (read parsed)

stringValue :: Parser Value
stringValue = do
  char '"'
  -- todo: handle escaped strings
  text <- many $ noneOf ['"']
  char '"'
  return $ VString text

value :: Parser Value
value = options [intValue, stringValue]


op :: Parser Op
op = options $ zipWith opOption ["+", "-", "*", "/", "and", "or"] [Plus, Minus, Times, Divide, And, Or]
  where opOption s o = do
          string s
          return o

