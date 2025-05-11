module Main where

import Control.Monad.State (StateT, modify, get, put, lift, evalStateT)

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


-- TODO: This is the wrong monad, the text needs to be passed as state
type Parser a = String -> Either String (a, String)


char :: Char -> Parser Char
char c s = case s of
  (c2:s') | c2 == c -> Right (c, s')
  _                 -> Left $ "Cannot match char " ++ [c]


oneOf :: [Char] -> Parser Char
oneOf options s = matchOneOf options
  where err = "Cannot match any of " ++ options
        matchOneOf []     = Left err
        matchOneOf (c:cs) = case s of
           (c':rest) | c' == c -> Right (c, rest)
           _                   -> matchOneOf cs


optional :: Parser a -> Parser (Maybe a)
optional p s = case p s of
  Left  _       -> Right (Nothing, s)
  Right (r, s') -> Right (Just r,  s')

many :: Parser a -> Parser [a]
many p s = Right $ parseMany s
  where parseMany str = case p str of
            Left  _         -> ([], str)
            Right (r, str') ->
               let (rs, rest) = parseMany str'
               in (r : rs, rest)

many1 :: Parser a -> Parser [a]
many1 p s = do
  (a1, s') <- p s
  (as, s'') <- many p s'
  return $ (a1 : as, s'')

digit :: Parser Char
digit = oneOf ['0' .. '9']

digits :: Parser String
digits = many1 digit


