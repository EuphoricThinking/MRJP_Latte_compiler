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


data Value = IntV Int | Success | StringV String | BoolV Bool 
            --  | FnDefV [Arg] [Stmt] Env | FnDefRetV [Arg] [Stmt] Env 
             | DeclGlobV | DeclFInvV | NonInitV | BreakV | ContinueV | VoidV 
             | FnDecl Type [Arg]

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

    
    -- do
        -- lift $ lift $ lift $ print topDefs
        return VoidV

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

-- findFuncDecl ( _ : rest) = do
--     findFuncDecl rest
    