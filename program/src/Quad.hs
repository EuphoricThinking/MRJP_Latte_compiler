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

data Val = FnDecl Type [Arg] BNFC'Position | IntQ | StringQ | BoolQ | VoidQ | FunQ Val | SuccessQ | FunRetTypeQ
             deriving (Eq, Show)

type LocNum = Int
type RetType = Val
type Body = QuadCode
data FuncData = FuncData String RetType [Arg] LocNum Body deriving (Show)

data Quad = QLabel FuncData
    -- add special funcs
    | QRet Val
    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = "", specialFunc = [], defFunc = Map.empty})

-- let 
    -- p = runQuadGen program
    -- s = QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)}
    -- in
            -- runwriterv $ runexcept $ evalstate (runreader p mapempty) s

runQuadGen :: Program -> QuadMonad (Val, QStore)
runQuadGen p = do
    cur_state <- isOneByOne topDefs --get
    return (IntQ, cur_state)

getRettypeDecl (Int _) = IntQ

insertToStoreNewFunc name funcInfo = do
    cur_state <- get
    put cur_state {defFunc = Map.insert name funcInfo (defFunc cur_state)}

updateCurFuncName name = do
    curState <- get
    put curState {curFunc = name}

isOneByOne [] = do
    cur_state <- get
    return cur_state

isOneByOne ((FnDef pos rettype (Ident ident) args stmts) : rest) = do
    curState <- get
    let newFuncData = FuncData ident (getRettypeDecl rettype) args 0 []
    insertToStoreNewFunc ident newFuncData
    updateCurFuncName ident

    -- funcBody <- genQCode stms
    -- update body as funcBody, qcode saved also in writer






