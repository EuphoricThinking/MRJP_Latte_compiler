{-# LANGUAGE FlexibleContexts #-}
module Quad where

import Typechecker (Loc, Env)

import Latte.Abs
import Latte.Lex
import Latte.Par

import System.IO
import System.Environment

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import System.Exit


--type Loc = Int
--type Env = Map.Map String Loc -- ident, location
data QStore = QStore {
    storeQ :: Map.Map Loc (Val, Int), -- Int is blockDepth (probably)
    lastLocQ :: Loc,
    curFuncName :: String,
    specialFunc :: [String],
    defFunc :: Map.Map String FuncData
} deriving (Show)

data Val = FnDecl Type [Arg] BNFC'Position | IntQ | StringQ | BoolQ | VoidQ | FunQ Val | SuccessQ | FunRetTypeQ | IntQVal Int
             deriving (Eq, Show)

type LocNum = Int
type RetType = Val
type Body = QuadCode
data FuncData = FuncData String RetType [Arg] LocNum Body deriving (Show)

data Quad = QLabel String --FuncData
    -- add special funcs
    | QRet Val
    | QFunc FuncData
    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncName = "", specialFunc = [], defFunc = Map.empty})

-- let 
    -- p = runQuadGen program
    -- s = QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)}
    -- in
            -- runwriterv $ runexcept $ evalstate (runreader p mapempty) s

runQuadGen :: Program -> QuadMonad (Val, QStore)
runQuadGen (Prog pos topDefs) = do
    cur_state <- insOneByOne topDefs --get
    -- cur_state <- get
    return (IntQ, cur_state)

getRettypeDecl (Int _) = IntQ

insertToStoreNewFunc name funcInfo = do
    cur_state <- get
    put cur_state {defFunc = Map.insert name funcInfo (defFunc cur_state)}

getFuncRet (FuncData _ rett _ _ _) = rett
getFuncArgs (FuncData _ _ args _ _) = args
getFuncNumLoc (FuncData _ _ _ numloc _) = numloc
updateCurFuncName name = do
    curState <- get
    put curState {curFuncName = name}

updateCurFuncBody body = do
    curState <- get
    -- curFName <- curFuncName curState
    -- throwError $ (show curFName)
    curFName <- gets curFuncName
    --throwError $ [curFName]
    curBody <- gets (Map.lookup curFName . defFunc)
    -- let curBody = Map.lookup curFName (defFunc curState)
    case curBody of
        Nothing -> throwError $ curFName ++ " cur not found"
        Just curFuncBody ->
            let 
                newBody = FuncData curFName (getFuncRet curFuncBody) (getFuncArgs curFuncBody) (getFuncNumLoc curFuncBody) body
            in
                put curState {defFunc = Map.insert curFName newBody (defFunc curState)} >> return newBody

insOneByOne [] = do
    cur_state <- get
    return cur_state

insOneByOne ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
    curState <- get
    let newFuncData = FuncData ident (getRettypeDecl rettype) args 0 []
    insertToStoreNewFunc ident newFuncData
    updateCurFuncName ident

    -- curState <- get
    -- curFName <- gets curFuncName
    --print (show curFName)

    funcBody <- genQStmt stmts []
    newFullFunc <- updateCurFuncBody funcBody

    tell $ [QFunc newFullFunc]

    insOneByOne rest

-- genQIns [] = return [[]] -- [] should be

-- genQIns ((BStmt pos (Blk posB stmts)) : rest) = (genQIns stmts) : (genQIns rest)
    -- curEnv <- ask
    -- return ((local (const curEnv) (genQIns stmts)) : genQIns

genQStmt [] qcode = return qcode

genQStmt ((BStmt pos (Blk posB stmts)) : rest) qcode = do
    blockCode <- genQStmt stmts []
    genQStmt rest (qcode ++ blockCode)
    -- funcBody <- genQCode stms
    -- update body as funcBody, qcode saved also in writer

genQStmt ((Ret pos expr) : rest) qcode = do
    -- add inf if constant to avoid mov rax repetition
    (retVal, codeExpr) <- genQExpr expr qcode
    genQStmt rest (codeExpr ++ [QRet retVal]) -- mem addr, const, register

genQExpr (ELitInt pos intVal) qcode = return ((IntQVal (fromInteger intVal)), qcode)





