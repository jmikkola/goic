module Main where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State, get, put, runState)
import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
  | Comment String
  deriving (Eq, Show)

data Instr
  = Push Arg
  | Pop Arg
  | Mov Arg Arg
  | Movzx Arg Arg
  | Lea Arg Arg
  | Add Arg Arg
  | Sub Arg Arg
  | Mul Arg
  | IDiv Arg
  | AndI Arg Arg
  | XorI Arg Arg
  | OrI Arg Arg
  | NotI Arg
  | Neg Arg
  | Sal Arg Arg
  | Sar Arg Arg
  | Cqo
  | CallI Arg
  | Cmp Arg Arg
  | Setl Arg
  | Setg Arg
  | Sete Arg
  | Setge Arg
  | Setle Arg
  | Setne Arg
  | Jmp String
  | Je String
  | Jne String
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
      "qword [" ++ render r ++ "+" ++ show offset ++ "]"
    R8Address             r ->
      "qword [" ++ render r ++ "]"
    R8Index               r index ->
      "qword [" ++ render r ++ "+" ++ render index ++ "]"
    R8Scaled              r index scale ->
      "qword [" ++ render r ++ "+" ++ render index ++ "*" ++ show scale ++ "]"
    R8ScaledOffset offset r index scale ->
      "qword [" ++ render r ++ "+" ++ render index ++ "*" ++ show scale ++ "+" ++ show offset ++ "]"

instance Render Instr where
  render instr = case instr of
    Ret       -> "ret"
    Pop  arg  -> "pop\t" ++ render arg
    Push arg  -> "push\t" ++ render arg
    Mov a b   -> "mov\t" ++ render a ++ ", " ++ render b
    Movzx a b -> "movzx\t" ++ render a ++ ", " ++ render b
    Lea a b   -> "lea\t" ++ render a ++ ", " ++ render b
    Add a b   -> "add\t" ++ render a ++ ", " ++ render b
    Sub a b   -> "sub\t" ++ render a ++ ", " ++ render b
    Mul a     -> "mul\t" ++ render a
    IDiv a    -> "idiv\t" ++ render a
    AndI a b  -> "and\t" ++ render a ++ ", " ++ render b
    XorI a b  -> "xor\t" ++ render a ++ ", " ++ render b
    OrI a b   -> "or\t" ++ render a ++ ", " ++ render b
    NotI a    -> "not\t" ++ render a
    Neg a     -> "neg\t" ++ render a
    Sal a b   -> "sal\t" ++ render a ++ ", " ++ render b
    Sar a b   -> "sar\t" ++ render a ++ ", " ++ render b
    Cqo       -> "cqo"
    CallI arg -> "call\t" ++ render arg
    Cmp a b   -> "cmp\t" ++ render a ++ ", " ++ render b
    Setl arg  -> "setl\t" ++ render arg
    Setg arg  -> "setg\t" ++ render arg
    Sete arg  -> "sete\t" ++ render arg
    Setge arg -> "setge\t" ++ render arg
    Setle arg -> "setle\t" ++ render arg
    Setne arg -> "setne\t" ++ render arg
    Jmp l     -> "jmp\t" ++ l
    Je l      -> "je\t" ++ l
    Jne l     -> "jne\t" ++ l
    Syscall   -> "syscall"

instance Render ASM where
  render (Instruction instr)     = "\t" ++ render instr
  render (Label label)           = label ++ ":"
  render (Directive name [])     = name
  render (Directive name args)   = name ++ "\t" ++ join ", " args
  render (Constant name t value) = "\t" ++ name ++ " " ++ t ++ " " ++ value
  render (Comment comment)       = "\t;; " ++ comment

instance Render [ASM] where
  render instructions = join "\n" $ map render instructions

data CompileState =
  CompileState
  { strings            :: Map String String -- name to value
  , labelsUsed         :: Int
  , argNames           :: [String]
  , localVars          :: Map String Int -- local var offsets
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
  -- the ", 0" at the end adds a null termination
  [ Constant name "db" (show value ++ ", 0")
  | (name, value) <- Map.toList strs ]

compileFunction :: Function -> Compiler [ASM]
compileFunction (Function name t argNames body) = do
  -- Setup the state for the fuction body
  state <- get
  put $ state { argNames = argNames
              , localVars = Map.empty
              -- it starts off not aligned to 16 bytes because the call instruction pushes RIP
              , stackDepth = 1
              }
  let preamble = functionPreamble name
  -- TODO: Save caller-saved registers (e.g. stack variables) if those registers
  -- will get used during the function body
  asm <- compileBody t argNames body
  -- Clear the function-specific parts of the state
  state' <- get
  put $ state' { argNames = []
               , localVars = Map.empty
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
  let localVarNames = Set.toList $ findLocalVars body
  let localVarOffset = 1 + min (length argRegisters) (length argNames)
  let localVarOffsets = Map.fromList $ zip localVarNames [localVarOffset..]
  let nLocalVars = Map.size localVarOffsets

  state <- get
  put $ state { localVars = localVarOffsets }

  let prologue = map Instruction functionPrologue
  -- the prologue contains one push
  pushStack

  let saveArgs = map Instruction $ saveArguments argNames
  -- saveArgs can contain multiple pushes
  changeStackDepth (length saveArgs)

  let reserveLocalSpace =
        if nLocalVars == 0
        then []
        else [ Instruction $ Sub (Register8 RSP) (Immediate (8 * nLocalVars)) ]
  changeStackDepth nLocalVars

  let epilogue = map Instruction functionEpilogue
  -- the epilog resets RSP back to RBP, so it wipes out most of the current
  -- stack frame.

  compiledBody <- compileStatement body
  let end = if (take 1 $ reverse compiledBody) == [Instruction Ret] then [] else [Instruction Ret]

  return $ prologue ++ saveArgs ++ reserveLocalSpace ++ compiledBody ++ epilogue ++ end

findLocalVars :: Statement -> Set String
findLocalVars stmt = findLV stmt Set.empty
  where findLV st names = case st of
          NewVar name _ _ -> Set.insert name names
          If _ s1 s2      -> findLVList [s1, s2] names
          While _ s1      -> findLV s1 names
          Block sts       -> findLVList sts names
          _               -> names
        findLVList sts names =
          foldr findLV names sts

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
  EStatement expr -> compileExpression expr
  Return     expr -> do
    exprInstrs <- compileExpression expr
    let instructions = exprInstrs ++ (map Instruction functionEpilogue) ++ [Instruction Ret]
    return instructions
  BareReturn      -> do
    let instructions = functionEpilogue ++ [Ret]
    return $ map Instruction instructions
  NewVar name t expr ->
    compileNewVar name expr
  Assign name expr ->
    compileAssign name expr
  AssignPtr name expr ->
    compileAssignPointer name expr
  If expr tcase ecase ->
    compileIf expr tcase ecase
  While test body ->
    compileWhile test body
  Block statements ->
    concatMapM compileStatement statements

compileNewVar :: String -> Expression -> Compiler [ASM]
compileNewVar name expr = do
  index <- lookupLocalVar name
  exprAsm <- compileExpression expr
  let offset = index * (-8)
  let writeVar = [ Instruction $ Mov (R8Offset offset RBP) (Register8 RAX) ]
  let instructions = exprAsm ++ writeVar
  return instructions

compileAssign :: String -> Expression -> Compiler [ASM]
compileAssign name expr = do
  offset <- lookupVariableOffset name
  exprAsm <- compileExpression expr
  let writeVar = [ Instruction $ Mov (R8Offset offset RBP) (Register8 RAX) ]
  let instructions = exprAsm ++ writeVar
  return instructions

compileAssignPointer :: String -> Expression -> Compiler [ASM]
compileAssignPointer name expr = do
  offset <- lookupVariableOffset name
  exprAsm <- compileExpression expr
  let writeVar = toASM [ Mov (Register8 RBX) (R8Offset offset RBP)
                       , Mov (R8Address RBX) (Register8 RAX) ]
  let instructions = exprAsm ++ writeVar
  return instructions

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
            , testAsm
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

compileIf :: Expression -> Statement -> Statement -> Compiler [ASM]
-- A special case for an empty else block
compileIf expr tcase (Block []) = do
  endLabel <- newLabel
  testAsm  <- compileExpression expr
  thenCase <- compileStatement tcase

  let results =
        [ testAsm
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Je endLabel ]
        , thenCase
        , [ Label endLabel ]
        ]
  return $ concat results
compileIf expr tcase ecase = do
  elseLabel <- newLabel
  endLabel  <- newLabel
  testAsm  <- compileExpression expr
  thenCase <- compileStatement tcase
  elseCase <- compileStatement ecase

  let results =
        [ testAsm
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

compileExpression :: Expression -> Compiler [ASM]
compileExpression expression = case expression of
  Literal value          -> compileLiteral value
  Variable name          -> compileVariable name
  BinaryOp op left right -> compileBinaryOp op left right
  UnaryOp op inner       -> compileUnaryOp op inner
  Paren inner            -> compileExpression inner
  Call function args     -> case function of
    -- TODO: Ensure that `fnName` refers to a function not a local or argument
    Variable fnName -> compileCall fnName args
    _               -> error "cannot handle function pointers yet"

compileLiteral :: Value -> Compiler [ASM]
compileLiteral value = case value of
  VInt i    ->
    return [Instruction $ Mov (Register8 RAX) (Immediate i)]
  VString s -> do
    varname <- addString s
    return [Instruction $ Mov (Register8 RAX) (Address varname)]

compileUnaryOp :: Uop -> Expression -> Compiler [ASM]
compileUnaryOp op inner = case op of
  Negate -> do
    innerAsm <- compileExpression inner
    let operation = [ Neg (Register8 RAX) ]
    return $ innerAsm ++ toASM operation
  Not -> do
    innerAsm <- compileExpression inner
    let operation = [ Cmp (Register8 RAX) (Immediate 0)
                    , Sete (Register1 AL)
                    , Movzx (Register8 RAX) (Register1 AL) ]
    return $ innerAsm ++ toASM operation
  Dereference -> do
    innerAsm <- compileExpression inner
    let operation = [ Mov (Register8 RAX) (R8Address RAX) ]
    return $ innerAsm ++ toASM operation
  TakeReference -> case inner of
    Variable name -> do
      offset <- lookupVariableOffset name
      let operation = [ Lea (Register8 RAX) (R8Offset offset RBP) ]
      return $ toASM operation
    _ -> error "taking a reference to non-variables is not supported yet"
  BitNot -> do
    innerAsm <- compileExpression inner
    let operation = [NotI (Register8 RAX)]
    return $ innerAsm ++ toASM operation

compileBinaryOp :: Op -> Expression -> Expression -> Compiler [ASM]
compileBinaryOp op left right = case op of
  Plus       -> compileBinMath left right [Add (Register8 RAX) (Register8 RBX)]
  Minus      -> compileBinMath left right [Sub (Register8 RAX) (Register8 RBX)]
  Times      -> compileBinMath left right [Mul (Register8 RBX)]
  Divide     -> compileBinMath left right [Cqo, IDiv (Register8 RBX)]
  -- the shift amount can only be in CL, not BL
  ShiftLeft  -> compileBinMath left right [ Mov (Register8 RCX) (Register8 RBX)
                                          , Sal (Register8 RAX) (Register1 CL) ]
  ShiftRight -> compileBinMath left right [ Mov (Register8 RCX) (Register8 RBX)
                                          , Sar (Register8 RAX) (Register1 CL)]
  Mod        -> compileBinMath left right [ Cqo
                                          , IDiv (Register8 RBX)
                                          , Mov (Register8 RAX) (Register8 RDX) ]
  And        -> compileAnd left right
  Or         -> compileOr  left right
  Greater    -> compileBinComp left right Setg
  Less       -> compileBinComp left right Setl
  Equal      -> compileBinComp left right Sete
  GEqual     -> compileBinComp left right Setge
  LEqual     -> compileBinComp left right Setle
  NotEqual   -> compileBinComp left right Setne
  BitAnd     -> compileBinMath left right [AndI (Register8 RAX) (Register8 RBX)]
  BitXor     -> compileBinMath left right [XorI (Register8 RAX) (Register8 RBX)]
  BitOr      -> compileBinMath left right [OrI (Register8 RAX) (Register8 RBX)]

-- This is a convenience function for compiling binary values then
-- running some math instruction[s] on them
compileBinMath :: Expression -> Expression -> [Instr] -> Compiler [ASM]
compileBinMath left right instrs = do
  setup <- compileBinaryValues left right
  return $ setup ++ (toASM instrs)

compileBinComp :: Expression -> Expression -> (Arg -> Instr) -> Compiler [ASM]
compileBinComp left right setInstr = do
  setup <- compileBinaryValues left right
  let instrs = [ Cmp (Register8 RAX) (Register8 RBX)
               , setInstr (Register1 AL)
               , Movzx (Register4 EAX) (Register1 AL) ]
  return $ setup ++ (toASM instrs)

-- creates code that will evaluate both `left` and `right`, and
-- leave the results in RAX and RBX, respectively
compileBinaryValues :: Expression -> Expression -> Compiler [ASM]
compileBinaryValues left right = do
  -- compile the left hand side and push the result on the stack
  leftInstrs <- compileExpression left
  let pushInstrs = [Instruction $ Push $ Register8 RAX]
  pushStack
  -- compile the right hand side
  rightInstrs <- compileExpression right
  -- move the right hand side to RBX, then pop the left hand side into RAX
  let swapAndPopInstrs =
        [ Instruction $ Mov (Register8 RBX) (Register8 RAX)
        , Instruction $ Pop $ Register8 RAX
        ]
  popStack
  return $ concat [leftInstrs, pushInstrs, rightInstrs, swapAndPopInstrs]

compileAnd :: Expression -> Expression -> Compiler [ASM]
compileAnd left right = do
  labelFalse <- newLabel
  labelEnd   <- newLabel

  leftInstrs  <- compileExpression left
  rightInstrs <- compileExpression right

  let instructions =
        [ leftInstrs
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Je labelFalse ]
        , rightInstrs
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Je labelFalse ]
        , toASM [ Mov (Register8 RAX) (Immediate 1)
                , Jmp labelEnd ]
        , [Label labelFalse]
        , toASM [ Mov (Register8 RAX) (Immediate 0) ]
        , [Label labelEnd]
        , toASM [ Movzx (Register8 RAX) (Register1 AL) ]
        ]
  return $ concat instructions

compileOr :: Expression -> Expression -> Compiler [ASM]
compileOr left right = do
  labelTrue  <- newLabel
  labelFalse <- newLabel
  labelEnd   <- newLabel

  leftInstrs  <- compileExpression left
  rightInstrs <- compileExpression right

  let instructions =
        [ leftInstrs
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Jne labelTrue ]
        , rightInstrs
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Je labelFalse ]
        , [Label labelTrue]
        , toASM [ Mov (Register8 RAX) (Immediate 1)
                , Jmp labelEnd ]
        , [Label labelFalse]
        , toASM [ Mov (Register8 RAX) (Immediate 0) ]
        , [Label labelEnd]
        , toASM [ Movzx (Register8 RAX) (Register1 AL) ]
        ]
  return $ concat instructions

compileVariable :: String -> Compiler [ASM]
compileVariable name = do
  offset <- lookupVariableOffset name
  return $ toASM [Mov (Register8 RAX) (R8Offset offset RBP)]

lookupVariableOffset :: String -> Compiler Int
lookupVariableOffset name = do
  state <- get
  case elemIndex name (argNames state) of
    Just idx ->
      if idx < 6
         -- read args inside this stack frame where they were saved
         -- from the registers by this function
         then return $ (idx + 1) * (-8)
         -- read args from the previous stack frame where they were
         -- saved by the caller
         -- (-6: ignore the first 6 indexes, +2: skip over where RBP
         -- and RIP are stored on the stack)
         else return $ (idx - 6 + 2) * 8
    Nothing -> do
      offset <- lookupLocalVar name
      return $ offset * (-8)

argRegisters :: [Reg8]
argRegisters = [RDI, RSI, RDX, RCX, R8, R9]

compileCall :: String -> [Expression] -> Compiler [ASM]
compileCall fnName []   =
  return [Instruction $ CallI (Address fnName)]
compileCall fnName args = do
  let nArgs = length args
  -- Push all N arguments on to the stack, left to right
  argInstrs <- concatMapM compileArg args

  -- Figure out how many arguments will be passed on the stack (the first 6 are
  -- passed in registers)
  let nStackArgs = if nArgs > 6 then nArgs - 6 else 0

  -- Fill the argument-passing registers with the values from the stack
  let regFill =
        [ Instruction $ Mov (Register8 r) (R8Offset (i * 8) RSP)
        | (r, i) <- zip argRegisters (reverse [0..nArgs - 1])
        ]

  stackDepth <- getStackDepth
  -- The stack needs to be left in a 16 byte alignment before the call.
  -- This determines what the stack depth will end up being after the arguments
  -- that are passed on the stack are pushed.
  -- The stack alignment happens *before* those arguments are pushed.
  let needsAlignment = (stackDepth + nStackArgs) `mod` 2 == 1
  (alignmentOffset, alignment) <- if needsAlignment
    then do
        pushStack
        return (1, toASM [Sub (Register8 RSP) (Immediate 8)])
    else
        return (0, [])

  -- Push the 7th and following args.
  -- The 7th arg ends up being the top of the stack, the 8th arg after that, etc.
  -- Explanation of the formula:
  --  - `8 *`: Words are 8 bytes
  --  - `alignmentOffset`: account for the `sub RSP 8` and read further back
  --  - `+ 2 * n`: As this pushes args, RSP changes _and_ this needs to read
  --    further back
  let pushArgs =
        toASM [ Push $ R8Offset (8 * (alignmentOffset + 2 * n)) RSP
              | n <- [0..nStackArgs-1] ]

  -- TODO: Save caller saved registers
  let call = [Instruction $ CallI (Address fnName)]
  -- TODO: Restore caller saved registers

  let popSize = nArgs + nStackArgs + alignmentOffset
  let cleanup = [Instruction $ Add (Register8 RSP) (Immediate (popSize * 8))]
  changeStackDepth (-popSize)

  return $ argInstrs ++ regFill ++ alignment ++ pushArgs ++ call ++ cleanup

compileArg :: Expression ->  Compiler [ASM]
compileArg argExpr = do
  compiled <- compileExpression argExpr
  pushStack
  return $ compiled ++ [Instruction $ Push (Register8 RAX)]

toASM :: [Instr] -> [ASM]
toASM = map Instruction

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
  AssignPtr name expr -> do
    (t, source) <- case lookup name names of
      Nothing -> Left ("Assigning to an undefined variable *" ++ name)
      Just ts -> return ts
    errIf (elem source [FnDecl, Builtin]) ("Cannot assign a value to a function: " ++ name)
    errIf (not $ isPointer t) ("Cannot pointer-assign to a variable that is not a pointer: " ++ name)
    exprT <- typecheck names expr
    errIf ((Pointer exprT) /= t) ("Type mismatch for assignment to variable *" ++ name ++ ", type is " ++ show t ++
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
typecheck names (UnaryOp o inner) = do
  iType <- typecheck names inner
  let mustInt =
        if iType == Int
          then return Int
          else Left $ "Invalid type for unary " ++ show o ++ ": " ++ show iType
  case o of
    Not           -> mustInt
    Negate        -> mustInt
    BitNot        -> mustInt
    TakeReference -> do
      ensureReferenceable names inner
      return $ Pointer iType
    Dereference   ->
      case iType of
        (Pointer pointed) ->
          return pointed
        _                 ->
         Left $ "Cannot dereference non-pointer type " ++ show iType ++ " in " ++ show o
typecheck names (Paren e) =
  typecheck names e
typecheck names (Call e args) = do
  fnType <- typecheck names e
  argTypes <- mapM (typecheck names) args
  case fnType of
    (Func (FnType fargs ret)) | fargs == argTypes -> Right ret
    _                                             ->
      Left $ "Cannot call function with type " ++ show fnType ++ " with args of types " ++ show argTypes

ensureReferenceable :: Names -> Expression -> Either Err ()
ensureReferenceable names expr = case expr of
  Variable name -> do
    (_t, source) <- case lookup name names of
      Nothing -> Left "unexpected undefined variable in ensureReferenceable"
      Just ts -> return ts
    errIf (not $ source `elem` [Local, Argument]) ("Taking pointers to non-local is not yet supported: " ++ name)
    return ()
  Literal _     ->
    Left "taking pointers to literals is not supported yet"
  _             ->
    Left "taking pointers to the results of expressions is not supported yet"

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
    | AssignPtr String Expression
    | If Expression Statement Statement
    | While Expression Statement
    | Block [Statement]
    deriving (Show)

data Expression
    = Literal Value
    | Variable String
    | BinaryOp Op Expression Expression
    | UnaryOp Uop Expression
    | Paren Expression
    | Call Expression [Expression]
    deriving (Show)

data Value
    = VInt Int
    | VFloat Double
    | VString String
    deriving (Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    | Mod
    | ShiftLeft
    | ShiftRight
    | And
    | Or
    | Greater
    | Less
    | Equal
    | GEqual
    | LEqual
    | NotEqual
    | BitAnd
    | BitXor
    | BitOr
    deriving (Eq, Show)

data Uop
  = Not
  | Negate
  | TakeReference
  | Dereference
  | BitNot
  deriving (Eq, Show)

data FnType = FnType [Type] Type
    deriving (Eq, Show)

data Type
    = Func FnType
    | Void
    | Int
    | Float
    | String
    | Char
    | Pointer Type
    deriving (Eq, Show)

isPointer :: Type -> Bool
isPointer (Pointer _) = True
isPointer _           = False

fnName :: Function -> (String, (Type, Source))
fnName (Function name fnT _ _) = (name, (Func fnT, FnDecl))

instance Render Value where
  render (VInt i) = show i
  render (VString s) = show s

instance Render Op where
  render op = case op of
    Plus       -> "+"
    Minus      -> "-"
    Times      -> "*"
    Divide     -> "/"
    ShiftLeft  -> "<<"
    ShiftRight -> ">>"
    Mod        -> "%"
    And        -> "and"
    Or         -> "or"
    Greater    -> ">"
    Less       -> "<"
    Equal      -> "=="
    GEqual     -> ">="
    LEqual     -> "<="
    NotEqual   -> "!="
    BitAnd     -> "&"
    BitXor     -> "^"
    BitOr      -> "|"


--
-- Parser for the basic language
--

langDef :: Token.LanguageDef ()
langDef = emptyDef
    { Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> char '_'
    , Token.opStart         = oneOf "+-*/%<>=!&|"
    , Token.opLetter        = oneOf "+-*/%<>=!&|"
    , Token.reservedNames   = ["module", "func", "return", "while", "if", "else", "var", "Void", "Int", "String", "Char", "Fn", "Float"]
    , Token.reservedOpNames = ["+", "-", "*", "/", "%", "and", "or", "not", ">", "<", "==", ">=", "<=", "!="]
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
float         = Token.float lexer
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
    , try assignPointer
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

assignPointer :: Parser Statement
assignPointer = do
    reservedOp "*"
    varName <- identifier
    reservedOp "="
    value <- expression
    return $ AssignPtr varName value

-- Type Parsers
typeParser :: Parser Type
typeParser = choice
    [ reserved "Void"   >> return Void
    , reserved "Int"    >> return Int
    , reserved "String" >> return String
    , reserved "Char"   >> return Char
    , reserved "Float"  >> return Float
    , funcType
    , pointerType
    ]

funcType :: Parser Type
funcType = do
    reserved "Fn"
    args <- parens (commaSep typeParser)
    retType <- typeParser
    return $ Func $ FnType args retType

pointerType :: Parser Type
pointerType = do
  reservedOp "*"
  innerType <- typeParser
  return $ Pointer innerType

-- Expression Parsers
expression :: Parser Expression
expression = buildExpressionParser operatorTable term

operatorTable =
    [ [prefix "*" Dereference, prefix "&" TakeReference]
    , [prefix "-" Negate]
    , [prefix "not" Not, prefix "~" BitNot]
    , [binary "*" Times AssocLeft, binary "/" Divide AssocLeft, binary "%" Mod AssocLeft]
    , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
    , [binary ">>" ShiftRight AssocLeft, binary "<<" ShiftLeft AssocLeft]
    , [binary ">" Greater AssocNone, binary "<" Less AssocNone, binary ">=" GEqual AssocNone, binary "<=" LEqual AssocNone]
    , [binary "==" Equal AssocNone, binary "!=" NotEqual AssocNone]
    , [binary "&" BitAnd AssocLeft]
    , [binary "^" BitXor AssocLeft]
    , [binary "|" BitOr AssocLeft]
    , [binary "and" And AssocLeft]
    , [binary "or" Or AssocLeft]
    ]
    where
      binary name op = Infix (do{ reservedOp name; return (BinaryOp op) })
      prefix name unop = Prefix (do{ reservedOp name; return (UnaryOp unop) })

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
    [ VFloat <$> float
    , VInt . fromInteger <$> integer
    , VString <$> stringLiteral
    ]
