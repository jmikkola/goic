module Main where

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


type Parser a = String -> Either String (a, String)


char :: Char -> Parser Char
char c s = case s of
  (c2:s') | c2 == c -> Right (c, s')
  _                 -> Left $ "Cannot match char " ++ [c]


string :: String -> Parser String
string x s =
  let len = length x
      matching = take len s
      rest = drop len s
  in if matching == x
     then Right (x, rest)
     else Left $ "Cannot match string " ++ x


oneOf :: [Parser a] -> Parser a
oneOf []     _ = Left "oneOf requires at least one parser"
oneOf [p]    s = p s
oneOf (p:ps) s =  p s <> oneOf ps s











