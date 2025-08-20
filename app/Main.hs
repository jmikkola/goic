module Main where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State, get, put, runState)
import Data.Bits.Floating (coerceToWord)
import Data.Char (toLower, ord)
import Data.List (elemIndex, findIndex)
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

parseFile :: SourceName -> String -> IO (Module ())
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
link name = runSubprocess "gcc" args
  -- Let `gcc` pick the flags to pass to `ld`
  where args = ["-g", "-o", name, name ++ ".o"]

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
  | Movsd Arg Arg
  | Movq Arg Arg
  | Lea Arg Arg
  | Add Arg Arg
  | Sub Arg Arg
  | Mul Arg
  | IDiv Arg
  | Addsd Arg Arg
  | Subsd Arg Arg
  | Mulsd Arg Arg
  | Divsd Arg Arg
  | AndI Arg Arg
  | XorI Arg Arg
  | OrI Arg Arg
  | NotI Arg
  | Neg Arg
  | Sal Arg Arg
  | Sar Arg Arg
  | Cqo
  | CallI Arg
  | CallRel String
  | Cmp Arg Arg
  | Ucomisd Arg Arg
  | Setl Arg
  | Setb Arg
  | Setg Arg
  | Seta Arg
  | Sete Arg
  | Setge Arg
  | Setae Arg
  | Setle Arg
  | Setbe Arg
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
  | AddressVal String
  | Register8 Reg8
  | Register4 Reg4
  | Register1 Reg1
  | R8Offset Int Reg8
  | R8Address Reg8
  | R8Index Reg8 Reg8
  | R8Scaled Reg8 Reg8 Int
  | R8ScaledOffset Int Reg8 Reg8 Int
  | XMM Int
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
    AddressVal s -> "[rel " ++ s ++ "]"
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
    XMM n -> "xmm" ++ show n

instance Render Instr where
  render instr = case instr of
    Ret       -> "ret"
    Pop  arg  -> "pop\t" ++ render arg
    Push arg  -> "push\t" ++ render arg
    Mov a b   -> "mov\t" ++ render a ++ ", " ++ render b
    Movzx a b -> "movzx\t" ++ render a ++ ", " ++ render b
    Movsd a b -> "movsd\t" ++ render a ++ ", " ++ render b
    Movq a b  -> "movq\t" ++ render a ++ ", " ++ render b
    Lea a b   -> "lea\t" ++ render a ++ ", " ++ render b
    Add a b   -> "add\t" ++ render a ++ ", " ++ render b
    Sub a b   -> "sub\t" ++ render a ++ ", " ++ render b
    Mul a     -> "mul\t" ++ render a
    IDiv a    -> "idiv\t" ++ render a
    Addsd a b -> "addsd\t" ++ render a ++ ", " ++ render b
    Subsd a b -> "subsd\t" ++ render a ++ ", " ++ render b
    Mulsd a b -> "mulsd\t" ++ render a ++ ", " ++ render b
    Divsd a b -> "divsd\t" ++ render a ++ ", " ++ render b
    AndI a b  -> "and\t" ++ render a ++ ", " ++ render b
    XorI a b  -> "xor\t" ++ render a ++ ", " ++ render b
    OrI a b   -> "or\t" ++ render a ++ ", " ++ render b
    NotI a    -> "not\t" ++ render a
    Neg a     -> "neg\t" ++ render a
    Sal a b   -> "sal\t" ++ render a ++ ", " ++ render b
    Sar a b   -> "sar\t" ++ render a ++ ", " ++ render b
    Cqo       -> "cqo"
    CallI arg -> "call\t" ++ render arg
    CallRel a -> "call\t" ++ a ++ " wrt ..plt"
    Cmp a b   -> "cmp\t" ++ render a ++ ", " ++ render b
    Ucomisd a b -> "ucomisd\t" ++ render a ++ ", " ++ render b
    Setl arg  -> "setl\t" ++ render arg
    Setb arg  -> "setb\t" ++ render arg
    Setg arg  -> "setg\t" ++ render arg
    Seta arg  -> "seta\t" ++ render arg
    Sete arg  -> "sete\t" ++ render arg
    Setge arg -> "setge\t" ++ render arg
    Setae arg -> "setae\t" ++ render arg
    Setle arg -> "setle\t" ++ render arg
    Setbe arg -> "setbe\t" ++ render arg
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
  , functionTypes      :: Map String FnType
  , argLocations       :: Map String Int -- argument offsets
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
  , functionTypes      = Map.empty
  , argLocations       = Map.empty
  }

newLabel :: Compiler String
newLabel = do
  state <- get
  let nUsed = labelsUsed state
  put $ state { labelsUsed = nUsed + 1 }
  return $ "L" ++ show nUsed

compile :: Module Type -> String -> [ASM]
compile (Module _ functions) filename =
  let funcTypes = [(name, t) | (Function name t _ _) <- functions]
      allFuncTypes = funcTypes ++ builtinFunctions
      startingState = emptyCompileState { functionTypes = Map.fromList allFuncTypes }
      (fnTexts, state) = runState (mapM compileFunction functions) startingState
      dataSection = [Directive "section" [".rodata"]]
      stringDecls = compileStringDecls (strings state)
      textSection = [Directive "section" [".text"]]
      externs = [ Directive "extern" [name] | name <- builtinNames ]
      footer = [Directive "section" [".note.GNU-stack noalloc noexec nowrite progbits"]]
  in externs ++ dataSection ++ stringDecls ++ textSection ++ concat fnTexts ++ footer

compileStringDecls :: Map String String -> [ASM]
compileStringDecls strs =
  -- the ", 0" at the end adds a null termination
  [ Constant name "db" (stringLit value ++ ", 0")
  | (name, value) <- Map.toList strs ]

stringLit :: String -> String
stringLit []       = ""
stringLit s@(c:cs) =
  if printable c
  then let (stringPart, rest) = span printable s
       in show stringPart ++ stringLitRest rest
  else show (ord c) ++ stringLitRest cs

stringLitRest [] = ""
stringLitRest s  = ", " ++ stringLit s

printable :: Char -> Bool
printable c =
  ord c >= ord ' '

compileFunction :: Function Type -> Compiler [ASM]
compileFunction (Function name t argNames body) = do
  -- Setup the state for the fuction body
  state <- get
  put $ state { argNames = argNames
              , localVars = Map.empty
              -- it starts off not aligned to 16 bytes because the call instruction pushes RIP
              , stackDepth = 1
              , argLocations = Map.empty
              }
  let preamble = functionPreamble name
  asm <- compileBody t argNames body
  -- Clear the function-specific parts of the state
  state' <- get
  put $ state' { argNames = []
               , localVars = Map.empty
               , stackDepth = 0
               , argLocations = Map.empty
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
compileBody :: FnType -> [String] -> Statement Type -> Compiler [ASM]
compileBody t argNames body = do
  let localVarNames = Set.toList $ findLocalVars body
  let localVarOffset = 1 + min (length argRegisters) (length argNames)
  let localVarOffsets = Map.fromList $ zip localVarNames [localVarOffset..]
  let nLocalVars = Map.size localVarOffsets

  let (FnType argTypes _) = t
  let argPassingPlan = argPassing argTypes

  state <- get
  put $ state { localVars = localVarOffsets
              , argLocations = toArgLocations argNames argPassingPlan }

  let prologue = toASM functionPrologue
  -- the prologue contains one push
  pushStack

  let saveArgs = toASM $ saveArguments argPassingPlan
  -- saveArgs can contain multiple pushes
  changeStackDepth (length saveArgs)

  let reserveLocalSpace =
        if nLocalVars == 0
        then []
        else toASM [ Sub (Register8 RSP) (Immediate (8 * nLocalVars)) ]
  changeStackDepth nLocalVars

  compiledBody <- compileStatement body
  let endInstrs = if compiledBody `endsWith` (Instruction Ret)
                  then []
                  else functionEpilogue ++ [Ret]
  let end = toASM endInstrs

  return $ prologue ++ saveArgs ++ reserveLocalSpace ++ compiledBody ++ end

endsWith :: (Eq a) => [a] -> a -> Bool
endsWith []     _ = False
endsWith [x]    y = x == y
endsWith (x:xs) y = endsWith xs y

findLocalVars :: Statement a -> Set String
findLocalVars stmt = findLV stmt Set.empty
  where findLV st names = case st of
          NewVar name _ _ -> Set.insert name names
          If _ s1 s2      -> findLVList [s1, s2] names
          While _ s1      -> findLV s1 names
          Block sts       -> findLVList sts names
          _               -> names
        findLVList sts names =
          foldr findLV names sts

saveArguments :: ArgPassing -> [Instr]
saveArguments argPassingPlan =
  let saveIntArgs =
        [ Push $ Register8 reg
        | (_, reg) <- registerArgs argPassingPlan ]
      saveFloatArgsList =
        [ [ Sub (Register8 RSP) (Immediate 8)
          , Movsd (R8Address RSP) (XMM xmm) ]
        | (_, xmm) <- floatingArgs argPassingPlan ]
      saveFloatArgs = concat saveFloatArgsList
  in saveFloatArgs ++ saveIntArgs

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

compileStatement :: Statement Type -> Compiler [ASM]
compileStatement statement = case statement of
  EStatement expr -> compileExpression expr
  Return     expr -> do
    exprInstrs <- compileExpression expr
    let instructions = exprInstrs ++ (toASM functionEpilogue) ++ [Instruction Ret]
    return instructions
  BareReturn      -> do
    let instructions = functionEpilogue ++ [Ret]
    return $ toASM instructions
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

compileNewVar :: String -> Expression Type -> Compiler [ASM]
compileNewVar name expr = do
  index <- lookupLocalVar name
  exprAsm <- compileExpression expr
  let offset = index * (-8)
  let writeVar =
        if exprType expr == Float
        then [Movsd (R8Offset offset RBP) (XMM 0)]
        else [Mov (R8Offset offset RBP) (Register8 RAX)]
  let instructions = exprAsm ++ (toASM writeVar)
  return instructions

compileAssign :: String -> Expression Type -> Compiler [ASM]
compileAssign name expr = do
  offset <- lookupVariableOffset name
  exprAsm <- compileExpression expr
  let writeVar =
        if exprType expr == Float
        then [Movsd (R8Offset offset RBP) (XMM 0)]
        else [Mov (R8Offset offset RBP) (Register8 RAX)]
  let instructions = exprAsm ++ (toASM writeVar)
  return instructions

compileAssignPointer :: String -> Expression Type -> Compiler [ASM]
compileAssignPointer name expr = do
  offset <- lookupVariableOffset name
  exprAsm <- compileExpression expr
  let getAddress = toASM [Mov (Register8 RBX) (R8Offset offset RBP)]
  let writeVar =
        if exprType expr == Float
        then [Movsd (R8Address RBX) (XMM 0)]
        else [Mov (R8Address RBX) (Register8 RAX)]
  let instructions = exprAsm ++ getAddress ++ (toASM writeVar)
  return instructions

lookupLocalVar :: String -> Compiler Int
lookupLocalVar name = do
  state <- get
  return $ case Map.lookup name (localVars state) of
    Nothing -> error ("name should have been defined: " ++ name)
    Just o  -> o

compileWhile :: Expression Type -> Statement Type -> Compiler [ASM]
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
            , toASM [ Cmp (Register8 RAX) (Immediate 0)
                    , Je endLabel ]
            -- Execute the loop body
            , bodyAsm
            -- Jump back to the start of the loop to re-evaluate
            , toASM [ Jmp startLabel ]
            -- The end label to exit the loop
            , [ Label endLabel ]
            ]
    return $ concat results

compileIf :: Expression Type -> Statement Type -> Statement Type -> Compiler [ASM]
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
        , toASM [ Cmp (Register8 RAX) (Immediate 0)
                , Je elseLabel ]
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

compileExpression :: Expression Type -> Compiler [ASM]
compileExpression expression = case expression of
  Literal value            -> compileLiteral value
  Variable t name          -> compileVariable t name
  BinaryOp _ op left right -> compileBinaryOp op left right
  UnaryOp  _ op inner      -> compileUnaryOp op inner
  Paren inner              -> compileExpression inner
  Call _ function args     -> case function of
    -- TODO: Ensure that `fnName` refers to a function not a local or argument
    Variable _ fnName -> compileCall fnName args
    _                 -> error "cannot handle function pointers yet"

compileLiteral :: Value -> Compiler [ASM]
compileLiteral value = case value of
  VInt i    ->
    return [Instruction $ Mov (Register8 RAX) (Immediate i)]
  VString s -> do
    varname <- addString s
    return [Instruction $ Lea (Register8 RAX) (AddressVal varname)]
  VFloat f -> do
    return $ toASM [ Mov (Register8 RAX) (Immediate $ fromIntegral $ coerceToWord f)
                   , Movq (XMM 0) (Register8 RAX) ]

compileUnaryOp :: Uop -> Expression Type -> Compiler [ASM]
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
    Variable _ name -> do
      offset <- lookupVariableOffset name
      let operation = [ Lea (Register8 RAX) (R8Offset offset RBP) ]
      return $ toASM operation
    _ -> error "taking a reference to non-variables is not supported yet"
  BitNot -> do
    innerAsm <- compileExpression inner
    let operation = [NotI (Register8 RAX)]
    return $ innerAsm ++ toASM operation

compileBinaryOp :: Op -> Expression Type -> Expression Type -> Compiler [ASM]
compileBinaryOp op left right = case op of
  Plus       -> case exprType left of
    Float -> compileFloatMath left right [Addsd (XMM 0) (XMM 1)]
    _     -> compileBinMath   left right [Add (Register8 RAX) (Register8 RBX)]
  Minus      -> case exprType left of
    Float -> compileFloatMath left right [Subsd (XMM 0) (XMM 1)]
    _     -> compileBinMath left right [Sub (Register8 RAX) (Register8 RBX)]
  Times      -> case exprType left of
    Float -> compileFloatMath left right [Mulsd (XMM 0) (XMM 1)]
    _     -> compileBinMath left right [Mul (Register8 RBX)]
  Divide     -> case exprType left of
    Float -> compileFloatMath left right [Divsd (XMM 0) (XMM 1)]
    _     -> compileBinMath left right [Cqo, IDiv (Register8 RBX)]
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
  Greater    -> case exprType left of
    Float -> compileFloatComp left right Seta
    _     -> compileBinComp left right Setg
  Less       -> case exprType left of
    Float -> compileFloatComp left right Setb
    _     -> compileBinComp left right Setl
  Equal      -> case exprType left of
    Float -> compileFloatComp left right Sete
    _     -> compileBinComp left right Sete
  GEqual     -> case exprType left of
    Float -> compileFloatComp left right Setae
    _     -> compileBinComp left right Setge
  LEqual     -> case exprType left of
    Float -> compileFloatComp left right Setbe
    _     -> compileBinComp left right Setle
  NotEqual   -> case exprType left of
    Float -> compileFloatComp left right Setne
    _     -> compileBinComp left right Setne
  BitAnd     -> compileBinMath left right [AndI (Register8 RAX) (Register8 RBX)]
  BitXor     -> compileBinMath left right [XorI (Register8 RAX) (Register8 RBX)]
  BitOr      -> compileBinMath left right [OrI (Register8 RAX) (Register8 RBX)]

-- This is a convenience function for compiling binary values then
-- running some math instruction[s] on them
compileBinMath :: Expression Type -> Expression Type -> [Instr] -> Compiler [ASM]
compileBinMath left right instrs = do
  setup <- compileBinaryValues left right
  return $ setup ++ (toASM instrs)

compileFloatMath :: Expression Type -> Expression Type -> [Instr] -> Compiler [ASM]
compileFloatMath left right instrs = do
  setup <- compileFloatValues left right
  return $ setup ++ (toASM instrs)

compileBinComp :: Expression Type -> Expression Type -> (Arg -> Instr) -> Compiler [ASM]
compileBinComp left right setInstr = do
  setup <- compileBinaryValues left right
  let instrs = [ Cmp (Register8 RAX) (Register8 RBX)
               , setInstr (Register1 AL)
               , Movzx (Register4 EAX) (Register1 AL) ]
  return $ setup ++ (toASM instrs)

compileFloatComp :: Expression Type -> Expression Type -> (Arg -> Instr) -> Compiler [ASM]
compileFloatComp left right setInstr = do
  setup <- compileFloatValues left right
  let instrs = [ Ucomisd (XMM 0) (XMM 1)
               , setInstr (Register1 AL)
               , Movzx (Register4 EAX) (Register1 AL) ]
  return $ setup ++ (toASM instrs)

-- creates code that will evaluate both `left` and `right`, and
-- leave the results in RAX and RBX, respectively
compileBinaryValues :: Expression Type -> Expression Type -> Compiler [ASM]
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

-- creates code that will evaluate both `left` and `right`, and
-- leave the results in XMM0 and XMM1, respectively
compileFloatValues :: Expression Type -> Expression Type -> Compiler [ASM]
compileFloatValues left right = do
  leftInstrs <- compileExpression left
  let pushInstrs = toASM [ Sub (Register8 RSP) (Immediate 8)
                         , Movsd (R8Address RSP) (XMM 0) ]

  pushStack
  rightInstrs <- compileExpression right
  popStack

  let swapAndPopInstrs = toASM [ Movsd (XMM 1) (XMM 0)
                               , Movsd (XMM 0) (R8Address RSP)
                               , Add (Register8 RSP) (Immediate 8) ]
  return $ concat [leftInstrs, pushInstrs, rightInstrs, swapAndPopInstrs]


compileAnd :: Expression Type -> Expression Type -> Compiler [ASM]
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

compileOr :: Expression Type -> Expression Type -> Compiler [ASM]
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

compileVariable :: Type -> String -> Compiler [ASM]
compileVariable t name = do
  offset <- lookupVariableOffset name
  case t of
    Float ->
      return $ toASM [Movsd (XMM 0) (R8Offset offset RBP)]
    _     ->
      return $ toASM [Mov (Register8 RAX) (R8Offset offset RBP)]

lookupVariableOffset :: String -> Compiler Int
lookupVariableOffset name = do
  state <- get
  case Map.lookup name (argLocations state) of
    Just offset ->
      return offset
    Nothing -> do
      offset <- lookupLocalVar name
      return $ offset * (-8)

argRegisters :: [Reg8]
argRegisters = [RDI, RSI, RDX, RCX, R8, R9]

isBuiltin :: String -> Bool
isBuiltin fnName = elem fnName builtinNames

callInstructionFor :: String -> [ASM]
callInstructionFor fnName =
  if isBuiltin fnName
  then toASM [CallRel fnName]
  else toASM [CallI (Address fnName)]

compileCall :: String -> [Expression Type] -> Compiler [ASM]
compileCall fnName []   =
  return $ callInstructionFor fnName
compileCall fnName args = do
  state <- get
  let argTypes = case Map.lookup fnName (functionTypes state) of
        Nothing -> error ("function " ++ fnName ++ " should have been defined")
        Just (FnType ats _) -> ats

  let isVarArgs = argTypes `endsWith` VarArg
  let nArgs = length args
  let argPassingPlan = argPassing (map exprType args)

  -- Push all N arguments on to the stack, left to right
  argInstrs <- concatMapM compileArg args

  -- Figure out how many arguments will be passed on the stack (the first 6 are
  -- passed in registers)
  let nStackArgs = if nArgs > 6 then nArgs - 6 else 0

  -- Fill the argument-passing registers with the values from the stack
  let xmmFill = floatingFill nArgs argPassingPlan
  let regFill = registerFill nArgs argPassingPlan

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

  let pushArgs = pushStackArgs alignmentOffset nArgs argPassingPlan

  let varArgs =
        if isVarArgs
        then
          let nVarArgs = nArgs - (length argTypes) + 1
          in toASM [Mov (Register8 RAX) (Immediate nVarArgs)]
        else []

  -- TODO: Save caller saved registers
  let call = callInstructionFor fnName
  -- TODO: Restore caller saved registers

  let popSize = nArgs + nStackArgs + alignmentOffset
  let cleanup = toASM [Add (Register8 RSP) (Immediate (popSize * 8))]
  changeStackDepth (-popSize)

  return $ argInstrs ++ xmmFill ++ regFill ++ alignment ++ pushArgs ++ varArgs ++ call ++ cleanup

data ArgPassing
  = ArgPassing { registerArgs :: [(Int, Reg8)]
               , floatingArgs :: [(Int, Int)]
               , stackArgs    :: [Int]
               }
  deriving (Show)

argPassing :: [Type] -> ArgPassing
argPassing types = ArgPassing { registerArgs = regArgs, floatingArgs = flArgs, stackArgs = other }
  where indexedArgs = zip [0..] types
        floating = [i | (i,t) <- indexedArgs, t == Float]
        integer  = [i | (i,t) <- indexedArgs, t /= Float]

        xmm  = take 8 floating
        regs = take 6 integer

        regArgs = zip regs argRegisters
        flArgs  = zip xmm  [0..]
        other   = [i | (i,_) <- indexedArgs, not (i `elem` xmm || i `elem` regs)]

toArgLocations :: [String] -> ArgPassing -> Map String Int
toArgLocations names argPassing =
  Map.fromList [(name, findArgLocation i argPassing) | (name, i) <- zip names [0..]]

-- This relies on the fact that `saveArguments` saves args in XMM registers to
-- the stack first before args in the integer registers.
findArgLocation :: Int -> ArgPassing -> Int
findArgLocation argI argPassing =
  let argsInXMM   = floatingArgs argPassing
      argsInReg   = registerArgs argPassing
      argsInStack = stackArgs    argPassing

      matchReg (idx, _reg) = idx == argI

      numFloating = length argsInXMM

      idxToFloatLoc idx = (idx + 1) * (-8)
      idxToIntLoc   idx = (idx + 1 + numFloating) * (-8)
      idxToStackLoc idx = (idx + 2) * 8 -- +2 to skip over where RBP and RIP were pushed

      floatLocation = fmap idxToFloatLoc $ findIndex matchReg argsInXMM
      intLocation   = fmap idxToIntLoc   $ findIndex matchReg argsInReg
      stackLocation = fmap idxToStackLoc $ elemIndex argI     argsInStack

      err = error "argument not found in arg passing plan"
  in floatLocation `orElse` (intLocation `orElse` (stackLocation `orElse` err))

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing  y = y

{-
When preparing to call a fucntion foo(a, b, c), the arguments are evaluated left to
right and pushed on the stack as they are evaluated. The result is the stack
looks like:
- RSP+16: a's value
- RSP+8:  b's value
- RSP+0:  c's value

These values all need to be passed in registers. The argument passing plan for
them will look like:
[ (0, RDI) -- a
, (1, RSI) -- b
, (2, RDX) -- c
]

To get the value of `a` (argument 0), the math is
  (nArgs - i - 1) * 8
= (3 - 0 - 1) * 8
= 2 * 8
= 16
-}
registerFill :: Int -> ArgPassing -> [ASM]
registerFill nArgs argPassingPlan =
  let toOffset i = 8 * (nArgs - i - 1)
      instructions =
        [ Mov (Register8 r) (R8Offset (toOffset i) RSP)
        | (i, r) <- registerArgs argPassingPlan ]
  in toASM instructions

-- floatingFill is very similar to registerFill except that it populates XMM
-- registers
floatingFill :: Int -> ArgPassing -> [ASM]
floatingFill nArgs argPassingPlan =
  let toOffset i = 8 * (nArgs - i - 1)
      instructions =
        [ Movsd (XMM r) (R8Offset (toOffset i) RSP)
        | (i, r) <- floatingArgs argPassingPlan ]
  in toASM instructions

{-
Push arguments that didn't get packed into the integer or floating registers.
For integer arguments, this is the 7th and following argument.

The stack will contain the values of the arguments already, but in the wrong order:
- ...values that were pushed...
- arg7
- arg8
- arg9
Before calling the function, the stack-passed args need to be re-pushed the
other way around:
- arg9
- arg8
- arg7

A complicating factor is that, because floating and integer args can be mixed,
the list of args that need to be re-pushed might not be consecutive:
- arg7
- arg8 -> saved in XMM0
- arg9
so the stack before the call would have non-consecutive args:
- arg9
- arg7

Also, as more arguments are re-pushed, the existing ones get further away!

Finally, this may need to account for extra space pushed after the args were
evaluated for the sake of alignment.

Before starting to re-push any arguments, argument argI will be found at
`RSP + 8 * (alignmentOffset + nArgs - argI - 1)`
(Assuming alignmentOffset is zero, this means the last arg is found at RSP+0)

The offset relative to the current value of RSP will then be that value plus 8 *
nArgsPushed.
-}
pushStackArgs :: Int -> Int -> ArgPassing -> [ASM]
pushStackArgs alignmentOffset nArgs argPassingPlan =
  let toOffset argI nArgsPushed = 8 * (alignmentOffset + nArgs - argI - 1 + nArgsPushed)
      args = reverse $ stackArgs argPassingPlan
      instructions = [ Push (R8Offset (toOffset argI n) RSP)
                     | (argI, n) <- zip args [0..] ]
  in toASM instructions

compileArg :: Expression Type ->  Compiler [ASM]
compileArg argExpr = do
  compiled <- compileExpression argExpr
  pushStack

  let instr = if exprType argExpr == Float
        then [ Sub (Register8 RSP) (Immediate 8)
             , Movq (R8Address RSP) (XMM 0) ]
        else [ Push (Register8 RAX) ]
  return $ compiled ++ toASM instr

toASM :: [Instr] -> [ASM]
toASM = map Instruction

join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join j (x:ys) = x ++ j ++ (join j ys)


--
-- Type and semantic checks
--

checkFile :: Module () -> IO (Module Type)
checkFile m =
  case checkModule m of
    Left err -> do
      putErrLn err
      exitFailure
    Right typed ->
      return typed

data Source = Builtin | FnDecl | Argument | Local
  deriving (Eq, Show)

type Names = [(String, (Type, Source))]

type Err = String

builtins :: Names
builtins =
  [ (name, (Func fnType, Builtin))
  | (name, fnType) <- builtinFunctions ]

builtinNames :: [String]
builtinNames = map fst builtinFunctions

builtinFunctions :: [(String, FnType)]
builtinFunctions =
  [ ("puts", FnType [String] Void)
  , ("putchar", FnType [Int] Void)
  , ("printf", FnType [String, VarArg] Void)
  ]

checkModule :: Module a -> Either Err (Module Type)
checkModule (Module name functions) = do
  let funcNames = map fnName functions
  let duplicates = findDuplicates (map fst funcNames)
  errIf (not $ null duplicates) ("Duplicate function names: " ++ show duplicates)
  let names = funcNames ++ builtins
  typedFunctions <- mapM (checkFunction names) functions
  return $ Module name typedFunctions

checkFunction :: Names -> Function a -> Either Err (Function Type)
checkFunction names (Function name fnt argNames body) = do
  let duplicates = findDuplicates argNames
  errIf (not $ null duplicates) ("Duplicate argument names: " ++ show duplicates)
  let (FnType argTypes retType) = fnt
  let theArgs = zip argNames $ zip argTypes $ repeat Argument
  (_names, typedBody) <- checkStatement (theArgs ++ names) retType body
  return $ Function name fnt argNames typedBody

checkStatement :: Names -> Type -> Statement a -> Either Err (Names, Statement Type)
checkStatement names returnType stmt = case stmt of
  EStatement expr -> do
    (_t, typedExpr) <- typecheck names expr
    return (names, EStatement typedExpr)
  Return expr -> do
    (t, typedExpr) <- typecheck names expr
    errIf (t /= returnType) ("Return type is " ++ show returnType ++ " but return expression is " ++ show t)
    return (names, Return typedExpr)
  BareReturn ->
    if returnType /= Void
      then Left $ "Return type is " ++ show returnType ++ " but return has no expression"
      else return (names, BareReturn)
  NewVar name t expr -> do
    errIf (hasName name names) ("Duplicate definition of variable " ++ name)
    (exprT, typedExpr) <- typecheck names expr
    errIf (exprT /= t) ("Type mismatch for declaration of variable " ++ name ++ ", type is " ++ show t ++
      " but expression is " ++ show exprT)
    let names' = (name, (t, Local)) : names
    return (names', NewVar name t typedExpr)
  Assign name expr -> do
    (t, source) <- case lookup name names of
      Nothing -> Left ("Assigning to an undefined variable " ++ name)
      Just ts -> return ts
    errIf (elem source [FnDecl, Builtin]) ("Cannot assign a value to a function: " ++ name)
    (exprT, typedExpr) <- typecheck names expr
    errIf (exprT /= t) ("Type mismatch for assignment to variable " ++ name ++ ", type is " ++ show t ++
                       " but expression is " ++ show exprT)
    return (names, Assign name typedExpr)
  AssignPtr name expr -> do
    (t, source) <- case lookup name names of
      Nothing -> Left ("Assigning to an undefined variable *" ++ name)
      Just ts -> return ts
    errIf (elem source [FnDecl, Builtin]) ("Cannot assign a value to a function: " ++ name)
    errIf (not $ isPointer t) ("Cannot pointer-assign to a variable that is not a pointer: " ++ name)
    (exprT, typedExpr) <- typecheck names expr
    errIf ((Pointer exprT) /= t) ("Type mismatch for assignment to variable *" ++ name ++ ", type is " ++ show t ++
                       " but expression is " ++ show exprT)
    return (names, AssignPtr name typedExpr)
  If test body0 body1 -> do
    (testT, testTyped) <- typecheck names test
    errIf (testT /= Int) ("If statement test value must be an Int, got " ++ show testT)
    (_names0, body0Typed) <- checkStatement names returnType body0
    (_names1, body1Typed) <- checkStatement names returnType body1
    return (names, If testTyped body0Typed body1Typed)
  While test body -> do
    (testT, testTyped) <- typecheck names test
    errIf (testT /= Int) ("While statement test value must be an Int, got " ++ show testT)
    (_names, bodyTyped) <- checkStatement names returnType body
    return (names, While testTyped bodyTyped)
  Block stmts -> do
    typedStatements <- checkStatements names returnType stmts
    -- return the original names unchanged, because variables declared in the block only exist
    -- inside that block
    return (names, Block typedStatements)

checkStatements :: Names -> Type -> [Statement a] -> Either Err [Statement Type]
checkStatements _     _ []        = return []
checkStatements names t (s:stmts) = do
  -- check that there aren't statements after an unconditional return
  if not $ null stmts
    then case s of
           Return _   -> Left "Statements after a return statement"
           BareReturn -> Left "Statements after a return statement"
           _          -> return ()
    else return ()

  (names', sTyped) <- checkStatement names t s
  -- let names declared in the block be used for subsequent statements inside that block
  stmtsTyped <- checkStatements names' t stmts
  return (sTyped : stmtsTyped)

hasName :: String -> Names -> Bool
hasName name names = case lookup name names of
  Nothing -> False
  Just _  -> True

getType :: String -> Names -> Maybe Type
getType name names = fst <$> lookup name names

errIf :: Bool -> String -> Either Err ()
errIf False _   = return ()
errIf True  err = Left err


typecheck :: Names -> Expression a -> Either Err (Type, Expression Type)
typecheck _     (Literal val)     =
  return (valType val, Literal val)
typecheck names (Variable _ name) = case getType name names of
  Just t  -> return (t, Variable t name)
  Nothing -> Left $ "Variable not defined: " ++ name
typecheck names (BinaryOp _ o l r) = do
  (lType, lTyped) <- typecheck names l
  (rType, rTyped) <- typecheck names r
  errIf (lType /= rType)
    ("Type mismatch for " ++ show o ++ ": " ++ show lType ++ " and " ++ show rType)
  errIf (not $ binOpSupportsType o lType)
    ("Invalid type for " ++ show o ++ ": " ++ show lType)
  let resultType = binOpResultType o lType
  return (resultType, BinaryOp resultType o lTyped rTyped)
typecheck names (UnaryOp _ o inner) = do
  (iType, innerTyped) <- typecheck names inner
  let mustInt =
        if iType == Int
          then return (Int, UnaryOp Int o innerTyped)
          else Left $ "Invalid type for unary " ++ show o ++ ": " ++ show iType
  case o of
    Not           -> mustInt
    Negate        -> mustInt
    BitNot        -> mustInt
    TakeReference -> do
      ensureReferenceable names inner
      let resultType = Pointer iType
      return (resultType, UnaryOp resultType o innerTyped)
    Dereference   ->
      case iType of
        (Pointer pointed) ->
          return (pointed, UnaryOp pointed o innerTyped)
        _                 ->
         Left $ "Cannot dereference non-pointer type " ++ show iType ++ " in " ++ show o
typecheck names (Paren e) = do
  (t, eTyped) <- typecheck names e
  return (t, Paren eTyped)
typecheck names (Call _ e args) = do
  (fnType, fnTyped) <- typecheck names e
  argsAndTypes <- mapM (typecheck names) args
  let (argTypes, argsTyped) = unzip argsAndTypes
  case fnType of
    (Func (FnType fargs ret)) | argTypesMatch fargs argTypes ->
      Right (ret, Call ret fnTyped argsTyped)
    _                                             ->
      Left $ "Cannot call function with type " ++ show fnType ++ " with args of types " ++ show argTypes

argTypesMatch :: [Type] -> [Type] -> Bool
argTypesMatch []       []       = True
argTypesMatch [VarArg] _        = True
argTypesMatch (tf:tfs) (ta:tas) = tf == ta && argTypesMatch tfs tas
argTypesMatch _        _        = False


ensureReferenceable :: Names -> Expression a -> Either Err ()
ensureReferenceable names expr = case expr of
  Variable _ name -> do
    (_t, source) <- case lookup name names of
      Nothing -> Left "unexpected undefined variable in ensureReferenceable"
      Just ts -> return ts
    errIf (not $ source `elem` [Local, Argument]) ("Taking pointers to non-local is not yet supported: " ++ name)
    return ()
  Literal _       ->
    Left "taking pointers to literals is not supported yet"
  _               ->
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

data Module a = Module String [Function a]
    deriving (Show)

data Function a = Function String FnType [String] (Statement a)
    deriving (Show)

data Statement a
    = EStatement (Expression a)
    | Return (Expression a)
    | BareReturn
    | NewVar String Type (Expression a)
    | Assign String (Expression a)
    | AssignPtr String (Expression a)
    | If (Expression a) (Statement a) (Statement a)
    | While (Expression a) (Statement a)
    | Block [Statement a]
    deriving (Show)

data Expression a
    = Literal Value
    | Variable a String
    | BinaryOp a Op (Expression a) (Expression a)
    | UnaryOp  a Uop (Expression a)
    | Paren (Expression a)
    | Call a (Expression a) [Expression a]
    deriving (Show)

exprType :: Expression Type -> Type
exprType expr = case expr of
  Literal val      -> valType val
  Variable t _     -> t
  BinaryOp t _ _ _ -> t
  UnaryOp  t _ _   -> t
  Paren e          -> exprType e
  Call     t _ _   -> t

data Value
    = VInt Int
    | VFloat Double
    | VString String
    deriving (Show)

valType :: Value -> Type
valType val = case val of
  VInt    _ -> Int
  VFloat  _ -> Float
  VString _ -> String

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

binOpSupportsType :: Op -> Type -> Bool
binOpSupportsType op Int =
  elem op [ Plus, Minus, Times, Divide, Mod, ShiftLeft, ShiftRight
          , And, Or, Greater, Less, Equal, GEqual, LEqual, NotEqual
          , BitAnd, BitXor, BitOr ]
binOpSupportsType op Float =
  elem op [ Plus, Minus, Times, Divide
          , Greater, Less, Equal, GEqual, LEqual, NotEqual ]
binOpSupportsType op _ =
  elem op [ Equal, NotEqual ]

binOpResultType :: Op -> Type -> Type
binOpResultType op inputType =
  let isComparison = elem op [ Greater, Less, Equal, GEqual, LEqual, NotEqual ]
  in if isComparison then Int else inputType

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
    | VarArg
    deriving (Eq, Show)

isPointer :: Type -> Bool
isPointer (Pointer _) = True
isPointer _           = False

fnName :: Function a -> (String, (Type, Source))
fnName (Function name fnT _ _) = (name, (Func fnT, FnDecl))

instance Render Value where
  render (VInt i)    = show i
  render (VString s) = show s
  render (VFloat f)  = show f

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
moduleParser :: Parser (Module ())
moduleParser = do
    whiteSpace
    reserved "module"
    name <- identifier
    functions <- many (try function)
    eof
    return $ Module name functions

-- Function Parser
function :: Parser (Function ())
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
statement :: Parser (Statement ())
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

block :: Parser (Statement ())
block = Block <$> braces (sepEndBy statement (optional $ char '\n'))

returnParser :: Parser (Statement ())
returnParser = do
    reserved "return"
    Return <$> expression

bareReturn :: Parser (Statement ())
bareReturn = reserved "return" >> return BareReturn

while :: Parser (Statement ())
while = do
    reserved "while"
    condition <- expression
    body <- block
    return $ While condition body

ifParser :: Parser (Statement ())
ifParser = do
    reserved "if"
    condition <- expression
    body <- block
    else_ <- option (Block []) $ do
        reserved "else"
        block <|> ifParser -- allows for "else if"
    return $ If condition body else_

newVar :: Parser (Statement ())
newVar = do
    reserved "var"
    varName <- identifier
    varType <- typeParser
    reservedOp "="
    value <- expression
    return $ NewVar varName varType value

assign :: Parser (Statement ())
assign = do
    varName <- identifier
    reservedOp "="
    value <- expression
    return $ Assign varName value

assignPointer :: Parser (Statement ())
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
expression :: Parser (Expression ())
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
      binary name op = Infix (do{ reservedOp name; return (BinaryOp () op) })
      prefix name unop = Prefix (do{ reservedOp name; return (UnaryOp () unop) })

term :: Parser (Expression ())
term = choice
    [ try (Paren <$> parens expression)
    , try call
    , Literal <$> valueParser
    , Variable () <$> identifier
    ]

call :: Parser (Expression ())
call = do
    name <- identifier
    args <- parens (commaSep expression)
    return $ Call () (Variable () name) args

-- Value and Literal Parsers
valueParser :: Parser Value
valueParser = choice
    [ try (VFloat <$> float)
    , VInt . fromInteger <$> integer
    , VString . unescape <$> stringLiteral
    ]

unescape :: String -> String
unescape s = read $ "\"" ++ s ++ "\""
