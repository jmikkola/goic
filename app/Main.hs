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
    deriving (Eq, Show)

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

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p sep = do
  parsed <- optional p
  case parsed of
    Nothing -> return []
    Just x  -> do
      s <- optional sep
      case s of
        Nothing -> return [x]
        Just _  -> do
          more <- manySepBy p sep
          return (x : more)

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
            Right (r, text') -> do
              put text'
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

-- zero or more whitespace characters, not including newlines
anyLinearWhitespace :: Parser ()
anyLinearWhitespace = do
  many $ oneOf " \t"
  return ()

-- zero or more whitespace characters, including newlines
anyWhitespace :: Parser ()
anyWhitespace = do
  many $ oneOf " \t\n"
  return ()

----

expression :: Parser Expression
expression = binaryExpression

binaryExpression :: Parser Expression
binaryExpression = do
  (expressions, ops) <- readBinExprParts
  return $ unfoldParts expressions ops

readBinExprParts :: Parser ([Expression], [Op])
readBinExprParts = do
  left <- unaryExpression
  anyLinearWhitespace
  parts <- many $ do
    op <- opParser
    anyWhitespace
    right <- unaryExpression
    anyLinearWhitespace
    return (op, right)
  let (ops, rights) = unzip parts
  return (left : rights, ops)

unfoldParts :: [Expression] -> [Op] -> Expression
unfoldParts exprs ops =
  let ([result], []) = foldl unfoldOps (exprs, ops) precOrder
  in result

unfoldOps :: ([Expression], [Op]) -> [Op] -> ([Expression], [Op])
unfoldOps ([e],    [])   _     = ([e], [])
unfoldOps ([],     [])   _     = ([], [])
unfoldOps (l:r:es, o:os) opset =
  if elem o opset
  then unfoldOps (BinaryOp o l r : es, os) opset
  else let (restE, restO) = unfoldOps (r:es, os) opset
       in (l:restE, o:restO)
unfoldOps _ _ = error "invalid call"

precOrder :: [[Op]]
precOrder =
  [ [Times, Divide]
  , [Plus, Minus]
  , [Or]
  , [And]
  ]

unaryExpression :: Parser Expression
unaryExpression = options [paren, call, literal, variable]

paren :: Parser Expression
paren = do
  char '('
  anyWhitespace
  inner <- expression
  anyWhitespace
  char ')'
  return $ Paren inner

call :: Parser Expression
call = do
  -- Right now, expressions that evaluate to a function aren't supported
  fnName <- letters
  char '('
  anyWhitespace
  arguments <- manySepBy expression argSeparator
  anyWhitespace
  char ')'
  return $ Call (Variable fnName) arguments

argSeparator :: Parser ()
argSeparator = do
  char ','
  anyWhitespace
  return ()

literal :: Parser Expression
literal = do
  val <- value
  return $ Literal val

variable :: Parser Expression
variable = do
  name <- letters
  return $ Variable name


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

opParser :: Parser Op
opParser = options $ zipWith opOption names values
  where names = ["+", "-", "*", "/", "and", "or"]
        values = [Plus, Minus, Times, Divide, And, Or]
        opOption s o = do
          string s
          return o
