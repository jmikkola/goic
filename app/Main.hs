module Main where

import Control.Monad.State (StateT, get, put, lift, evalStateT, runStateT)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

{-
next:
- start a compiler
-}

main :: IO ()
main = getArgs >>= parseArgs >>= readFile >>= parseFile >>= checkFile >>= (putStrLn . show)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

parseArgs :: [String] -> IO String
parseArgs [a] = return a
parseArgs _   = do
  putErrLn "Usage: goic [filename.gc]"
  exitFailure

parseFile :: String -> IO Module
parseFile content =
  case parse moduleParser content of
    Left err -> do
      putErrLn "Error parsing file:"
      putErrLn err
      exitFailure
    Right result -> do
      return result

checkFile :: Module -> IO Module
checkFile m =
  case checkModule m of
    Left err -> do
      putErrLn err
      exitFailure
    Right _ ->
      return m

data Source = Builtin | FnDecl | Argument | Local
  deriving (Eq, Show)

type Names = [(String, (Type, Source))]

builtins :: Names
builtins = [("println", (Func $ FnType [String] Void, Builtin))]

checkModule :: Module -> Either Err ()
checkModule (Module name functions) = do
  let funcNames = map fnName functions
  let duplicates = findDuplicates (map fst funcNames)
  errIf (not $ null duplicates) ("Duplicate function names: " ++ show duplicates)
  let names = funcNames ++ builtins
  mapM_ (checkFunction names) functions

checkFunction :: Names -> Function -> Either Err ()
checkFunction names (Function _ fnt argNames body) = do
  let duplicates = findDuplicates argNames
  errIf (not $ null duplicates) ("Duplicate argument names: " ++ show duplicates)
  let (FnType argTypes retType) = fnt
  let theArgs = zip argNames $ zip argTypes $ repeat Argument
  _ <- checkStatement (theArgs ++ names) retType body
  return ()

checkStatement :: Names -> Type -> Statement -> Either Err Names
checkStatement names returnType stmt = case stmt of
  EStatement expr -> do
    _ <- typecheck names expr
    return names
  Return expr -> do
    t <- typecheck names expr
    errIf (t /= returnType) ("Return type is " ++ show returnType ++ " but return expression is " ++ show t)
    return names
  BareReturn ->
    if returnType /= Void
      then Left $ "Return type is " ++ show returnType ++ " but return has no expression"
      else return names
  NewVar name t expr -> do
    errIf (hasName name names) ("Duplicate definition of variable " ++ name)
    exprT <- typecheck names expr
    errIf (exprT /= t) ("Type mismatch for declaration of variable " ++ name ++ ", type is " ++ show t ++
      " but expression is " ++ show exprT)
    let names' = (name, (t, Local)) : names
    return names'
  Assign name expression -> do
    (t, source) <- case lookup name names of
      Nothing -> Left ("Assigning to an undefined variable " ++ name)
      Just ts -> return ts
    errIf (elem source [FnDecl, Builtin]) ("Cannot assign a value to a function: " ++ name)
    exprT <- typecheck names expression
    errIf (exprT /= t) ("Type mismatch for assignment to variable " ++ name ++ ", type is " ++ show t ++
                       " but expression is " ++ show exprT)
    return names
  If test body0 body1 -> do
    testT <- typecheck names test
    errIf (testT /= Int) ("If statement test value must be an Int, got " ++ show testT)
    checkStatement names returnType body0
    checkStatement names returnType body1
    return names
  While test body -> do
    testT <- typecheck names test
    errIf (testT /= Int) ("While statement test value must be an Int, got " ++ show testT)
    checkStatement names returnType body
    return names
  Block stmts -> do
    checkStatements names returnType stmts
    -- return the original names unchanged, because variables declared in the block only exist
    -- inside that block
    return names

checkStatements :: Names -> Type -> [Statement] -> Either Err ()
checkStatements _     _ []        = return ()
checkStatements names t (s:stmts) = do
  -- check that there aren't statements after an unconditional return
  if not $ null stmts
    then case s of
           Return _   -> Left "Statements after a return statement"
           BareReturn -> Left "Statements after a return statement"
    else return ()

  names' <- checkStatement names t s
  -- let names declared in the block be used for subsequent statements inside that block
  checkStatements names' t stmts

hasName :: String -> Names -> Bool
hasName name names = case lookup name names of
  Nothing -> False
  Just _  -> True

getType :: String -> Names -> Maybe Type
getType name names = fst <$> lookup name names

errIf :: Bool -> String -> Either Err ()
errIf False _   = return ()
errIf True  err = Left err


typecheck :: Names -> Expression -> Either Err Type
typecheck names (Literal val)   = Right $ case val of
  VInt    _ -> Int
  VString _ -> String
typecheck names (Variable name) = case getType name names of
  Just t -> Right t
  Nothing -> Left $ "Variable not defined: " ++ name
typecheck names (BinaryOp o l r) = do
  lType <- typecheck names l
  rType <- typecheck names r
  if lType /= rType
    then Left $ "Type mismatch for " ++ show o ++ ": " ++ show lType ++ " and " ++ show rType
    else if lType == Int then Right Int
         else if lType == String && o == Plus
              then Right String
              else Left $ "Invalid type for " ++ show o ++ ": " ++ show lType
typecheck names (Paren e) =
  typecheck names e
typecheck names (Call e args) = do
  fnType <- typecheck names e
  argTypes <- mapM (typecheck names) args
  case fnType of
    (Func (FnType args ret)) | args == argTypes -> Right ret
    _                                           ->
      Left $ "Cannot call function with type " ++ show fnType ++ " with args of types " ++ show argTypes


findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates items = findDup [] [] items
  where findDup _    duplicates []     = duplicates
        findDup seen duplicates (i:is) =
          if elem i seen
          then if elem i duplicates
               then findDup seen duplicates is
               else findDup seen (i : duplicates) is
          else findDup (i : seen) duplicates is
--
-- Define the types for the language's AST
--

data Program = Program [Module]
    deriving (Show)

data Module = Module String [Function]
    deriving (Show)

data Function = Function String FnType [String] Statement
    deriving (Show)

data Statement
    = EStatement Expression
    | Return Expression
    | BareReturn
    | NewVar String Type Expression
    | Assign String Expression
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
    deriving (Eq, Show)

data Type
    = Func FnType
    | Void
    | Int
    | String
    | Char
    deriving (Eq, Show)

fnName :: Function -> (String, (Type, Source))
fnName (Function name fnT _ _) = (name, (Func fnT, FnDecl))

--
-- Parser for the basic language
--


type Err = String
type ParseM = StateT String (Either Err)
type Parser a = ParseM a

parse :: Parser a -> String -> Either Err a
parse p text = evalStateT p text

eof :: Parser ()
eof = do
  text <- get
  if text == ""
    then return ()
    else _err $ "Expected eof, got " ++ (take 100 text)

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
      more <- many $ do
        sep
        p
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

-- one or more whitespace characters, not including newlines
any1LinearWhitespace :: Parser ()
any1LinearWhitespace = do
  many1 $ oneOf " \t"
  return ()

-- zero or more whitespace characters, including newlines
anyWhitespace :: Parser ()
anyWhitespace = do
  many $ oneOf " \t\n"
  return ()

----

moduleParser :: Parser Module
moduleParser = do
  anyWhitespace
  string "module"
  any1LinearWhitespace
  name <- letters
  char '\n'
  anyWhitespace

  functions <- manySepBy function functionSep

  anyWhitespace
  eof

  return $ Module name functions

functionSep :: Parser ()
functionSep = do
  anyLinearWhitespace
  char '\n'
  anyWhitespace
  return ()

function :: Parser Function
function = do
  string "func"
  any1LinearWhitespace

  name <- letters
  char '('
  anyWhitespace
  typedArgs <- manySepBy typedArg argSeparator
  anyWhitespace
  char ')'

  any1LinearWhitespace
  returnType <- typeParser
  any1LinearWhitespace

  body <- block

  let (argNames, argTypes) = unzip typedArgs
  let t = FnType argTypes returnType
  return $ Function name t argNames body

typedArg :: Parser (String, Type)
typedArg = do
  name <- letters
  any1LinearWhitespace
  t <- typeParser
  return (name, t)

statement :: Parser Statement
statement = do
  anyLinearWhitespace
  options [block, returnParser, bareReturn, while, ifParser, newVar, assign, exprStatement]

block :: Parser Statement
block = do
  char '{'
  char '\n'
  statements <- manySepBy statement (char '\n')
  anyLinearWhitespace
  char '\n'
  anyWhitespace
  char '}'
  return $ Block statements

returnParser :: Parser Statement
returnParser = do
  string "return"
  anyLinearWhitespace
  e <- expression
  return $ Return e

bareReturn :: Parser Statement
bareReturn = do
  string "return"
  return $ BareReturn

while :: Parser Statement
while = do
  string "while"
  any1LinearWhitespace
  condition <- expression
  anyLinearWhitespace
  body <- block
  return $ While condition body

ifParser :: Parser Statement
ifParser = do
  string "if"
  any1LinearWhitespace
  condition <- expression
  anyLinearWhitespace
  body <- block
  any1LinearWhitespace
  string "else"
  any1LinearWhitespace
  elseBody <- block
  return $ If condition body elseBody

newVar :: Parser Statement
newVar = do
  string "var"
  any1LinearWhitespace
  varName <- letters
  any1LinearWhitespace
  varType <- typeParser
  any1LinearWhitespace
  char '='
  any1LinearWhitespace
  value <- expression
  return $ NewVar varName varType value

assign :: Parser Statement
assign = do
  varName <- letters
  any1LinearWhitespace
  char '='
  any1LinearWhitespace
  value <- expression
  return $ Assign varName value

exprStatement :: Parser Statement
exprStatement = do
  expr <- expression
  return $ EStatement expr

typeParser :: Parser Type
typeParser = options [voidType, intType, stringType, charType, funcType]

voidType :: Parser Type
voidType = do
  string "Void"
  return Void

intType :: Parser Type
intType = do
  string "Int"
  return Int

stringType :: Parser Type
stringType = do
  string "String"
  return String

charType :: Parser Type
charType = do
  string "Char"
  return Char

funcType :: Parser Type
funcType = do
  string "Fn("
  args <- manySepBy typeParser argSeparator
  char ')'
  any1LinearWhitespace
  retType <- typeParser
  return $ Func $ FnType args retType

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
