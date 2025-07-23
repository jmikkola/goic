module Main where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State, get, put, runState)
import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Process (createProcess, proc, waitForProcess)
import System.Exit (ExitCode(..))

import Text.Parsec ( many
                   , try
                   , eof
                   , choice
                   , oneOf
                   , option
                   , sepEndBy
                   , char
                   , optional
                   , letter
                   , alphaNum
                   , parse
                   , (<|>)
                   , SourceName
                   )
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

--
-- Top-level, IO functions
--

main :: IO ()
main = getArgs >>= parseArgs >>= runForName

runForName :: String -> IO ()
runForName filename = do
  let name = trimExtension filename
  parsedModule <- readFile filename >>= parseFile filename >>= checkFile
  let compiled = compile parsedModule filename
  writeFile (name ++ ".asm") (render compiled)
  assemble name
  link name
  -- putStrLn $ render compiled

trimExtension :: String -> String
trimExtension = takeWhile (/= '.')

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

parseArgs :: [String] -> IO String
parseArgs [a] = return a
parseArgs _   = do
  putErrLn "Usage: goic [filename.gc]"
  exitFailure

parseFile :: SourceName -> String -> IO Module
parseFile filename content =
  case parse moduleParser filename content of
    Left err -> do
      putErrLn "Error parsing file:"
      putErrLn (show err)
      exitFailure
    Right result -> do
      return result

assemble :: String -> IO ()
assemble name =
  let args = [ "-Worphan-labels"
             , "-g"
             , "dwarf2"
             , "-f"
             , "elf64"
             , name ++ ".asm"
             , "-o"
             , name ++ ".o"
             ]
  in runSubprocess "yasm" args

link :: String -> IO ()
link name =
  let args = [ "-g"
             , "-o"
             , name
             , name ++ ".o"
             , "-lc"
             , "--dynamic-linker"
             , "/lib64/ld-linux-x86-64.so.2"
             , "-e"
             , "_start"
             ]
  in runSubprocess "ld" args

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
  -- name, type, value
  | Constant String String String
  deriving (Eq, Show)

data Instr
  = Push Arg
  | Pop Arg
  | Mov Arg Arg
  | Movzx Arg Arg
  | Add Arg Arg
  | Sub Arg Arg
  | Mul Arg
  | IDiv Arg
  | Cqo
  | CallI Arg
  | Cmp Arg Arg
  | Setl Arg
  | Setg Arg
  | Jmp String
  | Je String
  | Ret
  | Syscall
  deriving (Eq, Show)

data Arg
  = Immediate Int
  | Address String
  | Register8 Reg8
  | Register4 Reg4
  | Register1 Reg1
  | R8Offset Int Reg8
  | R8Address Reg8
  | R8Index Reg8 Reg8
  | R8Scaled Reg8 Reg8 Int
  | R8ScaledOffset Int Reg8 Reg8 Int
  deriving (Eq, Show)

data Reg8 = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Show)

data Reg4 = EAX | EBX | ECX | EDX | ESI | EDI | ESP | EBP
  deriving (Eq, Show)

-- this is not a complete list
data Reg1 = AL | BL | CL | DL
  deriving (Eq, Show)

class Render a where
  render :: a -> String

instance Render Reg8 where
  render reg = map toLower (show reg)

instance Render Reg4 where
  render reg = map toLower (show reg)

instance Render Reg1 where
  render reg = map toLower (show reg)

instance Render Arg where
  render arg = case arg of
    Immediate n -> show n
    Address s   -> s
    Register8 r -> render r
    Register4 r -> render r
    Register1 r -> render r
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
    Movzx a b -> "movzx\t" ++ render a ++ ", " ++ render b
    Add a b   -> "add\t" ++ render a ++ ", " ++ render b
    Sub a b   -> "sub\t" ++ render a ++ ", " ++ render b
    Mul a     -> "mul\t" ++ render a
    IDiv a    -> "idiv\t" ++ render a
    Cqo       -> "cqo"
    CallI arg -> "call\t" ++ render arg
    Cmp a b   -> "cmp\t" ++ render a ++ ", " ++ render b
    Setl arg  -> "setl\t" ++ render arg
    Setg arg  -> "setg\t" ++ render arg
    Jmp l     -> "jmp\t" ++ l
    Je l      -> "je\t" ++ l
    Syscall   -> "syscall"

instance Render ASM where
  render (Instruction instr)     = "\t" ++ render instr
  render (Label label)           = label ++ ":"
  render (Directive name [])     = name
  render (Directive name args)   = name ++ "\t" ++ join ", " args
  render (Constant name t value) = "\t" ++ name ++ " " ++ t ++ " " ++ value

instance Render [ASM] where
  render instructions = join "\n" $ map render instructions

data CompileState =
  CompileState
  { strings            :: Map String String -- name to value
  , labelsUsed         :: Int
  , argNames           :: [String]
  , localVars          :: Map String Int -- local var offsets
  , nextLocalVarOffset :: Int
  , stackDepth         :: Int
  }
  deriving (Show)

type Compiler a = State CompileState a


emptyCompileState :: CompileState
emptyCompileState =
  CompileState
  { strings            = Map.empty
  , labelsUsed         = 0
  , argNames           = []
  , localVars          = Map.empty
  , nextLocalVarOffset = 0
  , stackDepth         = 0
  }

newLabel :: Compiler String
newLabel = do
  state <- get
  let nUsed = labelsUsed state
  put $ state { labelsUsed = nUsed + 1 }
  return $ "L" ++ show nUsed

compile :: Module -> String -> [ASM]
compile (Module _ functions) filename =
  let (fnTexts, state) = runState (mapM compileFunction functions) emptyCompileState
      dataSection = [Directive "section" [".data"]]
      stringDecls = compileStringDecls (strings state)
      textSection = [Directive "section" [".text"]]
      externs = [ Directive "extern" [name]
                | name <- ["puts", "putchar"] ]
  in externs ++ dataSection ++ constants ++ stringDecls ++ textSection ++ defineStart ++ concat fnTexts

constants :: [ASM]
constants =
  [ Constant "SYS_exit" "equ" "60"
  , Constant "EXIT_SUCCESS" "equ" "0" ]

defineStart :: [ASM]
defineStart =
  let callMain = [Instruction $ CallI $ Address "main"]
      exitSuccess =
        [ Instruction $ Mov (Register8 RAX) (Address "SYS_exit")
        , Instruction $ Mov (Register8 RDI) (Address "EXIT_SUCCESS")
        , Instruction $ Syscall
        ]
  in functionPreamble "_start" ++ callMain ++ exitSuccess

compileStringDecls :: Map String String -> [ASM]
compileStringDecls strs =
  [ Constant name "db" (show value)
  | (name, value) <- Map.toList strs ]

compileFunction :: Function -> Compiler [ASM]
compileFunction (Function name t argNames body) = do
  -- Setup the state for the fuction body
  state <- get
  let localVarOffset = 1 + min (length argRegisters) (length argNames)
  put $ state { argNames = argNames
              , localVars = Map.empty
              , nextLocalVarOffset = localVarOffset
              , stackDepth = 0
              }
  let preamble = functionPreamble name
  -- TODO: Save caller-saved registers (e.g. stack variables) if those registers
  -- will get used during the function body
  asm <- compileBody t argNames body
  -- Clear the function-specific parts of the state
  state' <- get
  put $ state' { argNames = []
               , localVars = Map.empty
               , nextLocalVarOffset = 0
               , stackDepth = 0
               }
  return (preamble ++ asm)

pushStack :: Compiler ()
pushStack = changeStackDepth 1

popStack :: Compiler ()
popStack = changeStackDepth (-1)

changeStackDepth :: Int -> Compiler ()
changeStackDepth offset = do
  depth <- getStackDepth
  putStackDepth (depth + offset)

getStackDepth :: Compiler Int
getStackDepth = do
  state <- get
  return (stackDepth state)

putStackDepth :: Int -> Compiler ()
putStackDepth depth = do
  state <- get
  put $ state { stackDepth = depth }

functionPreamble :: String -> [ASM]
functionPreamble name =
  [ Directive "global" [name]
  , Label name
  ]

-- TODO: support optionally saving and restoring the callee-saved registers
--       that aren't used for arguments
compileBody :: FnType -> [String] -> Statement -> Compiler [ASM]
compileBody t argNames body = do
  let prologue = map Instruction functionPrologue
  -- the prologue contains one push
  pushStack

  let saveArgs = map Instruction $ saveArguments argNames
  -- saveArgs can contain multiple pushes
  changeStackDepth (length saveArgs)

  let epilogue = map Instruction functionEpilogue
  -- the epilog resets RSP back to RBP, so it wipes out most of the current
  -- stack frame.

  compiledBody <- compileStatement body
  let end = if (take 1 $ reverse compiledBody) == [Instruction Ret] then [] else [Instruction Ret]

  state <- get
  let nLocalVars = Map.size $ localVars state
  let reserveLocalSpace =
        if nLocalVars == 0
        then []
        else [ Instruction $ Sub (Register8 RSP) (Immediate (8 * nLocalVars)) ]

  return $ prologue ++ saveArgs ++ reserveLocalSpace ++ compiledBody ++ epilogue ++ end

saveArguments :: [String] -> [Instr]
saveArguments argNames =
  [ Push $ Register8 reg
  | (_, reg) <- zip argNames argRegisters ]

functionPrologue :: [Instr]
functionPrologue =
  [ Push $ Register8 $ RBP
  , Mov (Register8 $ RBP) (Register8 $ RSP)
  ]

functionEpilogue :: [Instr]
functionEpilogue =
  [ Mov (Register8 $ RSP) (Register8 $ RBP)
  , Pop $ Register8 $ RBP
  ]

compileStatement :: Statement -> Compiler [ASM]
compileStatement statement = case statement of
  EStatement expr -> do
    instructions <- compileExpression expr
    return $ map Instruction instructions
  Return     expr -> do
    exprInstrs <- compileExpression expr
    let instructions = exprInstrs ++ functionEpilogue ++ [Ret]
    return $ map Instruction instructions
  BareReturn      -> do
    let instructions = functionEpilogue ++ [Ret]
    return $ map Instruction instructions
  NewVar name t expr ->
    compileNewVar name expr
  Assign name expr ->
    compileAssign name expr
  If expr tcase ecase ->
    compileIf expr tcase ecase
  While test body ->
    compileWhile test body
  Block statements ->
    concatMapM compileStatement statements

compileNewVar :: String -> Expression -> Compiler [ASM]
compileNewVar name expr = do
  index <- createLocalVar name
  exprAsm <- compileExpression expr
  let offset = index * (-8)
  let writeVar = [ Mov (R8Offset offset RBP) (Register8 RAX) ]
  let instructions = exprAsm ++ writeVar
  return $ map Instruction instructions

createLocalVar :: String -> Compiler Int
createLocalVar name = do
  state <- get
  let offset = nextLocalVarOffset state
  let locals = localVars state
  let updatedLocals = Map.insert name offset locals
  put $ state { nextLocalVarOffset = (offset + 1), localVars = updatedLocals }
  return offset

compileAssign :: String -> Expression -> Compiler [ASM]
compileAssign name expr = do
  offset <- lookupLocalVar name
  exprAsm <- compileExpression expr
  let rbpOffset = offset * (-8)
  let writeVar = [ Mov (R8Offset rbpOffset RBP) (Register8 RAX) ]
  let instructions = exprAsm ++ writeVar
  return $ map Instruction instructions

lookupLocalVar :: String -> Compiler Int
lookupLocalVar name = do
  state <- get
  return $ case Map.lookup name (localVars state) of
    Nothing -> error ("name should have been defined: " ++ name)
    Just o  -> o

compileWhile :: Expression -> Statement -> Compiler [ASM]
compileWhile test body = do
    startLabel <- newLabel
    endLabel   <- newLabel
    testAsm    <- compileExpression test
    bodyAsm    <- compileStatement body

    let results =
            [ [ Label startLabel ]
            -- Evaluate the test condition
            , map Instruction testAsm
            -- Check if the result is 0 (false) and exit loop if so
            , [ Instruction $ Cmp (Register8 RAX) (Immediate 0)
              , Instruction $ Je endLabel
              ]
            -- Execute the loop body
            , bodyAsm
            -- Jump back to the start of the loop to re-evaluate
            , [ Instruction $ Jmp startLabel ]
            -- The end label to exit the loop
            , [ Label endLabel ]
            ]
    return $ concat results

-- TODO: Add a special case for an empty 'else' statement
compileIf :: Expression -> Statement -> Statement -> Compiler [ASM]
compileIf expr tcase ecase = do
  elseLabel <- newLabel
  endLabel  <- newLabel
  testAsm  <- compileExpression expr
  thenCase <- compileStatement tcase
  elseCase <- compileStatement ecase

  let results =
        [ map Instruction testAsm
        , [ Instruction $ Cmp (Register8 RAX) (Immediate 0)
          , Instruction $ Je elseLabel
          ]
        , thenCase
        , [ Instruction $ Jmp endLabel
          , Label elseLabel
          ]
        , elseCase
        , [ Label endLabel ]
        ]
  return $ concat results

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
    compileVariable name
  BinaryOp op left right -> do
    leftInstrs <- compileExpression left
    pushStack
    rightInstrs <- compileExpression right
    popStack
    return $ concat [ leftInstrs
                    , [Push $ Register8 RAX]
                    , rightInstrs
                    , [ Mov (Register8 RBX) (Register8 RAX)
                      , Pop $ Register8 RAX
                      ]
                    , compileOp op
                    ]
  Paren inner -> compileExpression inner
  Call function args -> case function of
    Variable fnName -> compileCall fnName args
    _               -> error "cannot handle function pointers yet"

compileVariable :: String -> Compiler [Instr]
compileVariable name = do
  state <- get
  case elemIndex name (argNames state) of
    Just idx ->
      if idx < 6
      then return [Mov (Register8 RAX) (R8Offset ((idx + 1) * (-8)) RBP)]
      else error "todo: handle more than 6 args"
    Nothing  -> do
      offset <- lookupLocalVar name
      return [Mov (Register8 RAX) (R8Offset (offset * (-8)) RBP)]

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

  stackDepth <- getStackDepth
  -- The stack needs to be left in a 16 byte alignment after the call
  -- instruction, which means it needs to be 8 bytes away from a 16 byte
  -- alignment right before the call instruction
  let needsAlignment = stackDepth `mod` 2 == 0
  let alignment =
        if needsAlignment
        then [Sub (Register8 RSP) (Immediate 8)]
        else []

  -- TODO: Save caller saved registers
  let call = [CallI (Address fnName)]
  -- TODO: Restore caller saved registers

  -- TODO: Also pop 7th and following args
  let popSize =
        if needsAlignment
        then (nArgs + 1) * 8
        else nArgs * 8
  let cleanup = [Add (Register8 RSP) (Immediate popSize)]
  changeStackDepth (-popSize)

  return $ argInstrs ++ regFill ++ alignment ++ call ++ cleanup

compileArg :: Expression ->  Compiler [Instr]
compileArg argExpr = do
  compiled <- compileExpression argExpr
  pushStack
  return $ compiled ++ [Push (Register8 RAX)]


compileOp :: Op -> [Instr]
compileOp Plus  = [Add (Register8 RAX) (Register8 RBX)]
compileOp Minus = [Sub (Register8 RAX) (Register8 RBX)]
compileOp Times = [Mul (Register8 RBX)]
compileOp Divide =
  [ Cqo
  , IDiv (Register8 RBX) ]
compileOp Mod =
  [ Cqo
  , IDiv (Register8 RBX), Mov (Register8 RAX) (Register8 RDX) ]
compileOp Less =
  [ Cmp (Register8 RAX) (Register8 RBX)
  , Setl (Register1 AL)
  , Movzx (Register4 EAX) (Register1 AL) ]
compileOp Greater =
  [ Cmp (Register8 RAX) (Register8 RBX)
  , Setg (Register1 AL)
  , Movzx (Register4 EAX) (Register1 AL) ]
compileOp op    = error $ "todo: handle op " ++ show op


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

type Err = String

builtins :: Names
builtins =
  [ ("puts", (Func $ FnType [String] Void, Builtin))
  , ("putchar", (Func $ FnType [Int] Void, Builtin))
  ]

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
    | Mod
    | And
    | Or
    | Greater
    | Less
    | Equal
    | GEqual
    | LEqual
    | NotEqual
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
    Plus     -> "+"
    Minus    -> "-"
    Times    -> "*"
    Divide   -> "/"
    Mod      -> "%"
    And      -> "and"
    Or       -> "or"
    Greater  -> ">"
    Less     -> "<"
    Equal    -> "=="
    GEqual   -> ">="
    LEqual   -> "<="
    NotEqual -> "!="


--
-- Parser for the basic language
--

langDef :: Token.LanguageDef ()
langDef = emptyDef
    { Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> char '_'
    , Token.opStart         = oneOf "+-*/%<>=!&|"
    , Token.opLetter        = oneOf "+-*/%<>=!&|"
    , Token.reservedNames   = ["module", "func", "return", "while", "if", "else", "var", "Void", "Int", "String", "Char", "Fn"]
    , Token.reservedOpNames = ["+", "-", "*", "/", "%", "and", "or", ">", "<", "==", ">=", "<=", "!="]
    , Token.commentLine     = "//"
    , Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

-- Lexer helper functions
identifier    = Token.identifier lexer
reserved      = Token.reserved lexer
reservedOp    = Token.reservedOp lexer
parens        = Token.parens lexer
braces        = Token.braces lexer
commaSep      = Token.commaSep lexer
whiteSpace    = Token.whiteSpace lexer
integer       = Token.integer lexer
stringLiteral = Token.stringLiteral lexer

-- Module Parser
moduleParser :: Parser Module
moduleParser = do
    whiteSpace
    reserved "module"
    name <- identifier
    functions <- many (try function)
    eof
    return $ Module name functions

-- Function Parser
function :: Parser Function
function = do
    reserved "func"
    name <- identifier
    typedArgs <- parens (commaSep typedArg)
    returnType <- typeParser
    body <- block
    let (argNames, argTypes) = unzip typedArgs
    let t = FnType argTypes returnType
    return $ Function name t argNames body

typedArg :: Parser (String, Type)
typedArg = do
    name <- identifier
    t <- typeParser
    return (name, t)

-- Statement Parsers
statement :: Parser Statement
statement = choice
    [ try block
    , try returnParser
    , try bareReturn
    , try while
    , try ifParser
    , try newVar
    , try assign
    , EStatement <$> expression
    ]

block :: Parser Statement
block = Block <$> braces (sepEndBy statement (optional $ char '\n'))

returnParser :: Parser Statement
returnParser = do
    reserved "return"
    Return <$> expression

bareReturn :: Parser Statement
bareReturn = reserved "return" >> return BareReturn

while :: Parser Statement
while = do
    reserved "while"
    condition <- expression
    body <- block
    return $ While condition body

ifParser :: Parser Statement
ifParser = do
    reserved "if"
    condition <- expression
    body <- block
    else_ <- option (Block []) $ do
        reserved "else"
        block <|> ifParser -- allows for "else if"
    return $ If condition body else_

newVar :: Parser Statement
newVar = do
    reserved "var"
    varName <- identifier
    varType <- typeParser
    reservedOp "="
    value <- expression
    return $ NewVar varName varType value

assign :: Parser Statement
assign = do
    varName <- identifier
    reservedOp "="
    value <- expression
    return $ Assign varName value

-- Type Parsers
typeParser :: Parser Type
typeParser = choice
    [ reserved "Void"   >> return Void
    , reserved "Int"    >> return Int
    , reserved "String" >> return String
    , reserved "Char"   >> return Char
    , funcType
    ]

funcType :: Parser Type
funcType = do
    reserved "Fn"
    args <- parens (commaSep typeParser)
    retType <- typeParser
    return $ Func $ FnType args retType

-- Expression Parsers
expression :: Parser Expression
expression = buildExpressionParser operatorTable term

operatorTable =
    [ [binary "*" Times AssocLeft, binary "/" Divide AssocLeft, binary "%" Mod AssocLeft]
    , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
    , [binary ">" Greater AssocNone, binary "<" Less AssocNone, binary ">=" GEqual AssocNone, binary "<=" LEqual AssocNone]
    , [binary "==" Equal AssocNone, binary "!=" NotEqual AssocNone]
    , [binary "and" And AssocLeft]
    , [binary "or" Or AssocLeft]
    ]
    where
      binary name op = Infix (do{ reservedOp name; return (BinaryOp op) })

term :: Parser Expression
term = choice
    [ try (Paren <$> parens expression)
    , try call
    , Literal <$> valueParser
    , Variable <$> identifier
    ]

call :: Parser Expression
call = do
    name <- identifier
    args <- parens (commaSep expression)
    return $ Call (Variable name) args

-- Value and Literal Parsers
valueParser :: Parser Value
valueParser = choice
    [ VInt . fromInteger <$> integer
    , VString <$> stringLiteral
    ]
