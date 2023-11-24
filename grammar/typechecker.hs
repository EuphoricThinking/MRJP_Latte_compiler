{-# LANGUAGE FlexibleContexts #-}

import Latte.Abs
import Latte.Lex
import Latte.Par

import System.IO
import System.Environment

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

mainName = "main"


main :: IO () 
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= parseFile >>= printReturnCode
        [filename] -> readFile filename >>= parseFile >>= printReturnCode

getFileStrinContent :: FilePath -> IO (Either String Value)
getFileStrinContent path = readFile path >>= parseFile

parseFile :: String -> IO (Either String Value)
parseFile fileContent =
    let
      tokens = myLexer fileContent
      parsed = pProgram tokens
    in executeProgram parsed

printReturnCode :: Either String Value -> IO ()
printReturnCode res = do
    case res of
        Left mes -> hPutStrLn stderr mes
        Right cont -> print $ "Exit code: " ++ show cont 

display_tokens :: [Token] -> IO()
display_tokens tokens =  do 
  let 
    parsed = pProgram tokens in
      print parsed

-- data TypeV = IntT | StringT | BoolT | VoidT | FunT deriving (Show, Eq)

data Value = IntV Int | Success | StringV String | BoolV Bool 
            --  | FnDefV [Arg] [Stmt] Env | FnDefRetV [Arg] [Stmt] Env 
             | DeclGlobV | DeclFInvV | NonInitV | BreakV | ContinueV | VoidV 
             | FnDecl Type [Arg] | IntT | StringT | BoolT | VoidT | FunT Value --TypeV Type

instance Show Value where
    show (IntV v) = show v
    show (StringV v) = show v
    show (BoolV v) = show v
    show Success = "Success"
    -- show (FnDefV a b c) = "FnDefV " ++ show a ++ " " ++ show b ++ " " ++ show c
    -- show (FnDefRetV a b c) = "FnDefRetV " ++ show a ++ " " ++ show b ++ " " ++ show c
    show DeclGlobV = "DeclGlobV"
    show DeclFInvV = "DeclFInvV"
    show NonInitV = "NonInitV"
    show BreakV = "BreakV"
    show ContinueV = "ContinueV"
    show VoidV = "VoidV"

-- Store przechowuje wszystkie zmienne przez cały czas
-- Env wskazuje na lokacje aktualnie widocznych zmiennych
-- Env -> Store -> Either String (a, Store)

type Loc = Int
type Env = Map.Map String Loc
data Store = Store {
    store :: Map.Map Loc Value,
    lastLoc :: Loc
} deriving (Show)

type InterpreterMonad a = ReaderT Env (StateT Store (ExceptT String IO)) a 

-- Allocate new location in store and return new location
alloc :: InterpreterMonad Loc
alloc = do
    cur_state <- get
    put cur_state {lastLoc = lastLoc cur_state + 1}
    return (lastLoc cur_state + 1) -- refers to the variable lastLoc

-- Assign the value to the given location
insertToStore val newloc = do
    cur_state <- get
    put cur_state {store = Map.insert newloc val (store cur_state)}

evalMaybe :: String -> Maybe a -> InterpreterMonad a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a

getEitherMessage (Left mes) = mes
getEitherMessage (Right mes) = mes

printMes mes = lift $ lift $ lift $ putStrLn mes

executeProgram :: Either String Program -> IO (Either String Value)
executeProgram program = 
    case program of
        Left mes -> runExceptT $ throwError mes
        Right p -> runExceptT $ evalStateT (runReaderT (executeRightProgram p) Map.empty) (Store {store = Map.empty, lastLoc = 0})

-- executeRightProgram :: Program -> InterpreterMonad Value
-- executeRightProgram (Program pos topDefs) = do
--     return VoidV
printSth mes = lift $ lift $ lift $ print mes

executeRightProgram :: Program -> InterpreterMonad Value  -- ? prog?
executeRightProgram (Prog pos topDefs) = 
    do
        envWithFuncDecl <- findFuncDecl topDefs

        -- mainFound <- Map.lookup "main" envWithFuncDecl

        -- printSth envWithFuncDecl
        case Map.lookup "main" envWithFuncDecl of
            Nothing -> throwError $ "No main method defined"
            _ ->  local (const envWithFuncDecl) (checkFunction topDefs)--checkFunction envWithFuncDecl topDefs --return (StringV "OK")
    
    -- do
        -- lift $ lift $ lift $ print topDefs
        -- return VoidV

findFuncDecl [] = do
    curEnv <- ask
    return curEnv

findFuncDecl ((FnDef pos rettype (Ident ident) args stmts) : rest) = do
    printSth ident
    
    funDecLoc <- alloc
    let funDeclData = (FnDecl rettype args)
    insertToStore funDeclData funDecLoc

    -- findFuncDecl rest
    local (Map.insert ident funDecLoc) (findFuncDecl rest)

-- checkFunction :: 
--checkFunction envFuncs ((FnDef pos (Latte.Abs.Type' posT) (Ident ident) args stmts) : rest) = do
isInt (Int a) = True
isInt _ = False

getPos (Just pos) = pos

getTypeOriginal :: Type -> Value
getTypeOriginal (Int _)  = IntT
getTypeOriginal (Bool _) = BoolT
getTypeOriginal (Str _)  = StringT
getTypeOriginal (Void _) = VoidT
getTypeOriginal (Fun _ t _) = FunT (getTypeOriginal t)

-- checkArgs []
-- checkArgs envLoc (arg: args)
checkArgs [] = do
    curEnv <- ask
    return curEnv

checkArgs ((Ar pos argType (Ident argName)): args) = do
    locFound <- asks (Map.lookup argName)
    case locFound of
        Just _ -> throwError $ "Multiple entity declaration in arguments (row, col): " ++ show (getPos pos)
        Nothing -> do
            typeLoc <- alloc
            -- insertToStore (TypeV argType) typeLoc
            insertToStore (getTypeOriginal argType) typeLoc
            local (Map.insert argName typeLoc) (checkArgs args)

checkFunction [] = printSth "here" >> return (StringV "OK")

checkFunction ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do

        -- check args
        printSth rettype
        --print pos
        if (ident == mainName)
        then do
            if not (isInt rettype)
            then
                throwError $ "Main method must return int (row, col): " ++ (show (getPos pos))
            else if (args /= [])
            then
                throwError $ "Too many arguments in main method; row, col " ++ show (getPos pos)
            else
                do
                curEnv <- ask
                local (const curEnv) (checkBody stmts)
                -- checkBody stmts what are the consequences?

        else
            do
            envWithParams <- checkArgs args
            printSth envWithParams
            --return VoidV
            local (const envWithParams) (checkBody stmts)

-- get Expr Type

-- modify store, in function: ret lists [retIf, ertElse, ret]
-- in if add field list of if, in else - else; nested if - add a list; remove after leaving if or else
    -- in the end of functo [] check return number and if void
        -- check return type na on the go
        -- stack
        --- check func arguments, check if function is declared in expressions

-- get type of a variable (x, a, b)
getExprType (EVar pos (Ident name)) = do
    typeLoc <- asks (Map.lookup name)
    case typeLoc of
        Nothing -> throwError $ "Unknown variable " ++ show name ++ " (row, col): " ++ show (getPos pos)
        Just loc -> do
            val <- gets (Map.lookup loc . store)
            return val

getExprType (ELitInt pos intVal) = return (Just IntT) --return IntT

checkDecl _ [] = do
    curEnv <- ask
    return curEnv

checkDecl vartype ((NoInit posIn (Ident ident)) : rest) = do
    foundVar <- asks (Map.lookup ident)
    case foundVar of
        Just _ -> throwError $ "Multiple variable declaration (row, col): " ++ show (getPos posIn)
        Nothing -> do
            decVarLoc <- alloc
            -- insertToStore (TypeV vartype) decVarLoc
            insertToStore (getTypeOriginal vartype) decVarLoc
            local (Map.insert ident decVarLoc) (checkDecl vartype rest)

checkDecl vartype ((Init posIn (Ident ident) expr) : rest) = do
    foundVar <- asks (Map.lookup ident)
    case foundVar of
        Just _ -> throwError $ "Multiple variable declaration (row, col): " ++ show (getPos posIn)
        Nothing -> do
            decVarLoc <- alloc
            -- insertToStore (TypeV vartype) decVarLoc
            insertToStore (getTypeOriginal vartype) decVarLoc
            -- check if expression type is correct
            exprType <- getExprType expr

            local (Map.insert ident decVarLoc) (checkDecl vartype rest)

checkBody [] = return (StringV "OK")

checkBody ((Empty pos) : rest) = checkBody rest

checkBody ((Decl pos vartype items) : rest) = do
    updatedEnv <- checkDecl vartype items
    local (const updatedEnv) (checkBody rest)
    -- foundDecl <- asks (Map.lookup ident)
    -- case foundDecl of
    --     Just _ -> throwError $ "Duplicate variable at (row, col): " ++ show (getPos pos)
    --     Nothing -> do
    --         declLoc <- alloc
    --         insertToStore (TypeV vartype) declLoc
    --         local (Map.insert ident declLoc) (checkBody rest)


checkBody _ = printSth "there" >>  return VoidV


-- findFuncDecl ( _ : rest) = do
--     findFuncDecl rest
    