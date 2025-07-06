module Main where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (StateT, State, get, put, lift, evalStateT, runStateT, runState)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Process (createProcess, proc, waitForProcess)
import System.Exit (ExitCode(..))

--
-- Top-level, IO functions
--

main :: IO ()
main = getArgs >>= parseArgs >>= runForName

runForName :: String -> IO ()
runForName filename = do
  parsedModule <- readFile filename >>= parseFile >>= checkFile
  let compiled = compile parsedModule filename
  putStrLn $ render compiled

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


--
-- Process utility
--

runSubprocess :: FilePath -> [String] -> IO ()
runSubprocess cmd args = do
    (_, _, _, ph) <- createProcess (proc cmd args)

    -- Wait for it to finish
    exitCode <- waitForProcess ph

    -- Check the result
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure c -> error $ "Process failed with exit code: " ++ show c

--
-- Compiler backend (emits assembly)
--

data ASM
  = Instruction Instr
  | Label String
  | Directive String [String]
  deriving (Show)

data Instr
  = Push Arg
  | Pop Arg
  | Mov Arg Arg
  | Add Arg Arg
  | Sub Arg Arg
  | CallI Arg
  | Ret
  deriving (Show)

data Arg
  = Immediate Int
  | Address String
  | Register8 Reg8
  | R8Offset Int Reg8
  | R8Address Reg8
  | R8Index Reg8 Reg8
  | R8Scaled Reg8 Reg8 Int
  | R8ScaledOffset Int Reg8 Reg8 Int
  -- | R4 Reg4
  -- | R4Offset Int Reg4
  -- | R4Address Reg4
  -- | R4Index Reg4 Reg4
  -- | R4Scaled Reg4 Reg4 Int
  -- | R4Offset Int Reg4 Reg4 Int
  deriving (Show)

data Reg8 = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show)

data Reg4 = EAX | EBX | ECX | EDX | ESI | EDI | ESP | EBP
  deriving (Show)


class Render a where
  render :: a -> String

instance Render Reg8 where
  render reg = map toLower (show reg)

instance Render Arg where
  render arg = case arg of
    Immediate n -> show n
    Address s   -> s
    Register8 r -> render r
    R8Offset       offset r ->
      "[" ++ render r ++ "+" ++ show offset ++ "]"
    R8Address             r ->
      "[" ++ render r ++ "]"
    R8Index               r index ->
      "[" ++ render r ++ "+" ++ render index ++ "]"
    R8Scaled              r index scale ->
      "[" ++ render r ++ "+" ++ render index ++ "*" ++ show scale ++ "]"
    R8ScaledOffset offset r index scale ->
      "[" ++ render r ++ "+" ++ render index ++ "*" ++ show scale ++ "+" ++ show offset ++ "]"

instance Render Instr where
  render instr = case instr of
    Ret       -> "ret"
    Pop  arg  -> "pop\t" ++ render arg
    Push arg  -> "push\t" ++ render arg
    Mov a b   -> "mov\t" ++ render a ++ ", " ++ render b
    Add a b   -> "add\t" ++ render a ++ ", " ++ render b
    Sub a b   -> "sub\t" ++ render a ++ ", " ++ render b
    CallI arg -> "call\t" ++ render arg

instance Render ASM where
  render (Instruction instr)   = "\t" ++ render instr
  render (Label label)         = "." ++ label ++ ":"
  render (Directive name [])   = "\t" ++ name
  render (Directive name args) = "\t" ++ name ++ "\t" ++ join ", " args

instance Render [ASM] where
  render instructions = join "\n" $ map render instructions

data CompileState =
  CompileState
  { strings :: Map String String -- name to value
  , argNames :: [String]
  , localVars :: Map String Int -- local var offsets
  }
  deriving (Show)

type Compiler a = State CompileState a

emptyCompileState :: CompileState
emptyCompileState =
  CompileState
  { strings = Map.empty
  , argNames = []
  , localVars = Map.empty
  }

compile :: Module -> String -> [ASM]
compile (Module _ functions) filename =
  let preamble = modulePreamble filename
      (fnTexts, state) = runState (mapM compileFunction functions) emptyCompileState
      stringDecls = compileStringDecls (strings state)
  in preamble ++ stringDecls ++ concat fnTexts

-- TODO
compileStringDecls _ = []

modulePreamble :: String -> [ASM]
modulePreamble name =
  [ Directive "file" [name]
  , Directive "section" [".text"]
  ]

compileFunction :: Function -> Compiler [ASM]
compileFunction (Function name t argNames body) = do
  -- Setup the state for the fuction body
  state <- get
  put (state { argNames = argNames, localVars = Map.empty })
  let preamble = functionPreamble name
  asm <- compileBody t argNames body
  -- Clear the function-specific parts of the state
  state' <- get
  put (state' { argNames = [], localVars = Map.empty })
  return (preamble ++ asm)

functionPreamble :: String -> [ASM]
functionPreamble name =
  [ Directive "global" [name]
  , Label name
  ]

compileBody :: FnType -> [String] -> Statement -> Compiler [ASM]
compileBody t argNames body = do
  compiledBody <- compileStatement body
  let instructions = functionPrologue ++ compiledBody ++ functionEpilogue
  return $ map Instruction instructions

-- TODO: support optionally saving some callee-saved registers
functionPrologue :: [Instr]
functionPrologue =
  [ Push $ Register8 $ RBP
  , Mov (Register8 $ RBP) (Register8 $ RSP)
  ]

-- TODO: support optionally restoring some callee-saved registers
functionEpilogue :: [Instr]
functionEpilogue =
  [ Mov (Register8 $ RSP) (Register8 $ RBP)
  , Pop $ Register8 $ RBP
  ]

compileStatement :: Statement -> Compiler [Instr]
compileStatement statement = case statement of
  EStatement expr -> compileExpression expr
  Return     expr -> do
    exprInstrs <- compileExpression expr
    return $ exprInstrs ++ [Ret]
  BareReturn      -> return [Ret]
  NewVar name t expr ->
    -- TODO: determine where on the stack `name` lives
    -- also reserve space on the stack for it
    -- then compile the expression and mov the result into that stack location
    undefined
  Assign name expr ->
    -- TODO: lookup where on the stack `name` lives
    -- then compile the expression and mov the result into it
    undefined
  If expr tcase ecase ->
    -- TODO: add code to handle ifs
    undefined
  While test body ->
    -- TODO: add code to handle while
    undefined
  Block statements ->
    concatMapM compileStatement statements

-- returns the variable name that holds the string
addString :: String -> Compiler String
addString string = do
  state <- get
  let strs = strings state
  let name = "str" ++ show (length strs)
  let strs' = Map.insert name string strs
  put (state { strings = strs' })
  return name

compileExpression :: Expression -> Compiler [Instr]
compileExpression expression = case expression of
  Literal value -> case value of
    VInt i    -> return [Mov (Register8 RAX) (Immediate i)]
    VString s -> do
      varname <- addString s
      return [Mov (Register8 RAX) (Address varname)]
  Variable name ->
    -- TODO: look up where this variable or argument is stored
    undefined
  BinaryOp op left right -> do
    leftInstrs <- compileExpression left
    rightInstrs <- compileExpression right
    return $ concat [ leftInstrs
                    , [Push $ Register8 RAX]
                    , rightInstrs
                    , [ Mov (Register8 RBX) (Register8 RAX)
                      , Pop $ Register8 RAX
                      ]
                    , compileOp op
                    ]
  Paren inner -> compileExpression inner
  Call (Variable fnName) args -> do
    compileCall fnName args
  _ -> undefined

argRegisters :: [Reg8]
argRegisters = [RDI, RSI, RDX, RCX, R8, R9]

compileCall :: String -> [Expression] -> Compiler [Instr]
compileCall fnName []   =
  return [CallI (Address fnName)]
compileCall fnName args = do
  let nArgs = length args
  -- Push all N arguments on to the stack, left to right
  argInstrs <- concatMapM compileArg args

  -- TODO: repush the 7th and following arguments
  if nArgs > 6
    then error "can't handle more than 6 args yet"
    else return ()

  -- Fill the argument-passing registers with the values from the stack
  let regFill =
        [ Mov (Register8 r) (R8Offset (i * 8) RSP)
        | (r, i) <- zip argRegisters (reverse [0..nArgs - 1])
        ]

  -- TODO: Save caller saved registers
  let call = [CallI (Address fnName)]
  -- TODO: Restore caller saved registers

  -- TODO: Also pop 7th and following args
  let cleanup = [Sub (Register8 RSP) (Immediate $ nArgs * 8)]

  return $ argInstrs ++ regFill ++ call ++ cleanup

compileArg :: Expression ->  Compiler [Instr]
compileArg argExpr = do
  compiled <- compileExpression argExpr
  return $ compiled ++ [Push (Register8 RAX)]


compileOp :: Op -> [Instr]
compileOp Plus = [Add (Register8 RAX) (Register8 RBX)]
compileOp Minus = [Sub (Register8 RAX) (Register8 RBX)]
compileOp _ = undefined -- TODO


join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join j (x:ys) = x ++ j ++ (join j ys)


--
-- Type and semantic checks
--

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
builtins = [("puts", (Func $ FnType [String] Void, Builtin))]

checkModule :: Module -> Either Err ()
checkModule (Module _ functions) = do
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
  Assign name expr -> do
    (t, source) <- case lookup name names of
      Nothing -> Left ("Assigning to an undefined variable " ++ name)
      Just ts -> return ts
    errIf (elem source [FnDecl, Builtin]) ("Cannot assign a value to a function: " ++ name)
    exprT <- typecheck names expr
    errIf (exprT /= t) ("Type mismatch for assignment to variable " ++ name ++ ", type is " ++ show t ++
                       " but expression is " ++ show exprT)
    return names
  If test body0 body1 -> do
    testT <- typecheck names test
    errIf (testT /= Int) ("If statement test value must be an Int, got " ++ show testT)
    _ <- checkStatement names returnType body0
    _ <- checkStatement names returnType body1
    return names
  While test body -> do
    testT <- typecheck names test
    errIf (testT /= Int) ("While statement test value must be an Int, got " ++ show testT)
    _ <- checkStatement names returnType body
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
           _          -> return ()
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
typecheck _     (Literal val)   = Right $ case val of
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
    (Func (FnType fargs ret)) | fargs == argTypes -> Right ret
    _                                             ->
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

instance Render Value where
  render (VInt i) = show i
  render (VString s) = show s

instance Render Op where
  render op = case op of
    Plus   -> "+"
    Minus  -> "-"
    Times  -> "*"
    Divide -> "/"
    And    -> "and"
    Or     -> "or"


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


char :: Char -> Parser ()
char c = do
  text <- get
  discard $ case text of
    (c2:rest) | c == c2 -> _parsed c rest
    _                   -> _err $ "Cannot match char " ++ [c]


-- string :: String -> Parser String
-- string s = do
--   _string s
--   return s

string :: String -> Parser ()
string [] = return ()
string (c:cs) = do
  char c
  string cs

oneOf :: [Char] -> Parser Char
oneOf opts = do
  text <- get
  case text of
    (c:rest) | elem c opts -> _parsed c rest
    _                      -> _err $ "Cannot match any of " ++ opts


noneOf :: [Char] -> Parser Char
noneOf opts = do
  text <- get
  case text of
    (c:rest) | not (elem c opts) -> _parsed c rest
    _                            -> _err $ "Character was one of " ++ opts


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
        discard sep
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

alphaNum :: Parser Char
alphaNum = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0' .. '9'] ++ ['_'])

-- zero or more whitespace characters, not including newlines
anyLinearWhitespace :: Parser ()
anyLinearWhitespace = discard $ many $ oneOf " \t"

-- one or more whitespace characters, not including newlines
any1LinearWhitespace :: Parser ()
any1LinearWhitespace = discard $ many1 $ oneOf " \t"

-- zero or more whitespace characters, including newlines
anyWhitespace :: Parser ()
anyWhitespace = discard $ many $ oneOf " \t\n"

-- Ignore the results of a parser in a way that doesn't trigger a compiler warning
discard :: Parser a -> Parser ()
discard p = do
  _ <- p
  return ()

----

nameParser :: Parser String
nameParser = do
  first <- letter
  rest <- many alphaNum
  return $ first : rest

moduleParser :: Parser Module
moduleParser = do
  anyWhitespace
  string "module"
  any1LinearWhitespace
  name <- nameParser
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

  name <- nameParser
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
  name <- nameParser
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
  if null statements
    then anyWhitespace
    else do
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
  varName <- nameParser
  any1LinearWhitespace
  varType <- typeParser
  any1LinearWhitespace
  char '='
  any1LinearWhitespace
  value <- expression
  return $ NewVar varName varType value

assign :: Parser Statement
assign = do
  varName <- nameParser
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
  case foldl unfoldOps (exprs, ops) precOrder of
    ([result], []) -> result
    _              -> undefined

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
  funcName <- nameParser
  char '('
  anyWhitespace
  arguments <- manySepBy expression argSeparator
  anyWhitespace
  char ')'
  return $ Call (Variable funcName) arguments

argSeparator :: Parser ()
argSeparator = do
  char ','
  anyWhitespace
  return ()

literal :: Parser Expression
literal = do
  val <- valueParser
  return $ Literal val

variable :: Parser Expression
variable = do
  name <- nameParser
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

valueParser :: Parser Value
valueParser = options [intValue, stringValue]

opParser :: Parser Op
opParser = options $ zipWith opOption names values
  where names = ["+", "-", "*", "/", "and", "or"]
        values = [Plus, Minus, Times, Divide, And, Or]
        opOption s o = do
          string s
          return o
