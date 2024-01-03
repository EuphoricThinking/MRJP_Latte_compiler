{-# LANGUAGE FlexibleContexts #-}
module Quad where

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


type Loc = Int
type Env = Map.Map String Loc -- ident, location
data QStore = QStore {
    storeQ :: Map.Map Loc (Val, Int), -- Int is blockDepth (probably)
    lastLocQ :: Loc,
    curFuncQ :: FuncData
} deriving (Show)

data Val = FnDecl Type [Arg] BNFC'Position | IntT | StringT | BoolT | VoidT | FunT Val | Success | FunRetType
             deriving (Eq, Show)

type LocNum = Int
data FuncData = FuncData String [Arg] LocNum deriving (Show)

data Quad = QLabel FuncData
    -- add special funcs
    | QRet Val
    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)})

-- let 
    -- p = runQuadGen program
    -- s = QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)}
    -- in
            -- runwriterv $ runexcept $ evalstate (runreader p mapempty) s

runQuadGen :: Program -> QuadMonad (Val, QStore)
runQuadGen p = do
    cur_state <- get
    return (IntT, cur_state)



