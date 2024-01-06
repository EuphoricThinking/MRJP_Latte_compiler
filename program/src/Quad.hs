{-# LANGUAGE FlexibleContexts #-}
module Quad where

import Typechecker (Loc, Env, checkIfAnyNameFromList)

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
type LabelCustom = String
data QStore = QStore {
    storeQ :: Map.Map Loc (LabelCustom, Val), --(Val, Int), -- Int is blockDepth (probably)
    lastLocQ :: Loc,
    curFuncName :: String,
    specialFunc :: [String],
    defFunc :: Map.Map String FuncData,
    countLabels :: Map.Map String Int
} deriving (Show)

data Val = FnDecl Type [Arg] BNFC'Position | IntQ | StringQ | BoolQ | VoidQ | FunQ Val | SuccessQ | FunRetTypeQ | IntQVal Int
             deriving (Eq, Show)

type SizeLocals = Int
type RetType = Val
type Body = QuadCode
type NumIntTypes = Int
data FuncData = FuncData String RetType [Arg] SizeLocals Body NumIntTypes deriving (Show)

data QVar = QLoc String Val | QArg String Val deriving (Show)

data Quad = QLabel String --FuncData
    -- add special funcs
    | QRet Val
    | QFunc FuncData
    | QAss QVar Val
    | QParam Val
    | QCall QVar String Int
    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

tmpInfix = "_tmp_"

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncName = "", specialFunc = [], defFunc = Map.empty, countLabels = Map.empty})

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

getOrigQType (Int _) = IntQ

alloc :: QuadMonad Loc
alloc = do
    cur_state <- get
    put cur_state {lastLocQ = lastLocQ cur_state + 1}
    return (lastLocQ cur_state + 1)

insertToStoreNewFunc name funcInfo = do
    cur_state <- get
    put cur_state {defFunc = Map.insert name funcInfo (defFunc cur_state)}

insertToStoreNewIdentVal name val loc = do
    curState <- get
    put curState {storeQ = Map.insert loc (name, val) (storeQ curState)}

insertNewLabelToCounter ident = do
    curState <- get
    put curState {countLabels = Map.insert ident 1 (countLabels curState)}

increaseLabelCounter ident = do
    curState <- get
    currentCount <- gets (Map.lookup ident . countLabels)
    case currentCount of
        Nothing -> throwError $ ident ++ " cannot increase counter"
        Just countIdent -> put curState {countLabels = Map.insert ident (countIdent + 1) (countLabels curState)}

getFuncRet (FuncData _ rett _ _ _ _) = rett
getFuncArgs (FuncData _ _ args _ _ _) = args
getFuncNumLoc (FuncData _ _ _ numloc _ _) = numloc
getFuncBody (FuncData _ _ _ _ body _) = body
getFuncBodyIntsNum (FuncData _ _ _ _ _ numInts) = numInts

-- return Label
-- createQVal (Int _) expr qcode= do
--     (val,  <- genExpr
--     return (IntQVal val)

updateCurFuncName name = do
    curState <- get
    put curState {curFuncName = name}

updateCurFuncBody :: Body -> QuadMonad FuncData
updateCurFuncBody body = do
    curState <- get
    let curFName = curFuncName curState


    -- throwError $ (show curFName)

    -- curFName <- gets curFuncName
    
    --throwError $ [curFName]
    curBody <- gets (Map.lookup curFName . defFunc)
    -- let curBody = Map.lookup curFName (defFunc curState)
    case curBody of
        Nothing -> throwError $ curFName ++ " cur not found"
        Just curFuncBody ->
            let 
                newBody = FuncData curFName (getFuncRet curFuncBody) (getFuncArgs curFuncBody) (getFuncNumLoc curFuncBody) body (getFuncBodyIntsNum curFuncBody)
                -- newBody = createNewBody (getFuncNumLoc curFuncBody) curFName curFuncBody
            in
                put curState {defFunc = Map.insert curFName newBody (defFunc curState)} >> return newBody

insOneByOne :: [TopDef] -> QuadMonad QStore
insOneByOne [] = do
    cur_state <- get
    return cur_state

insOneByOne ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
    curState <- get
    let newFuncData = FuncData ident (getOrigQType rettype) args 0 [] 0
    insertToStoreNewFunc ident newFuncData
    updateCurFuncName ident

    -- curState <- get
    -- curFName <- gets curFuncName
    --print (show curFName)

    funcBody <- genQStmt stmts []
    -- PERFORM in local env (probably)
    newFullFunc <- updateCurFuncBody funcBody

    tell $ [QFunc newFullFunc]

    insOneByOne rest

-- genQIns [] = return [[]] -- [] should be

-- genQIns ((BStmt pos (Blk posB stmts)) : rest) = (genQIns stmts) : (genQIns rest)
    -- curEnv <- ask
    -- return ((local (const curEnv) (genQIns stmts)) : genQIns

-- evalDecl :: Type' -> [Item'] -> QuadMonad Env

createNewBody (Int numLoc) fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) numLoc (getFuncBody fbody) (getFuncBodyIntsNum fbody)

createIncreaseNumInts numInts fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) ((getFuncBodyIntsNum fbody) + numInts)

increaseNumLocTypesCur exprVal = do
    curState <- get
    fname <- gets curFuncName
    fbody <- gets (Map.lookup fname . defFunc)
    case fbody of
        Nothing -> throwError $ fname ++ " curfunc not found"
        Just curBody -> do
            case exprVal of
                (IntQVal _) -> do
                    let updatedNumInts = createIncreaseNumInts 1 fname curBody
                    put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

updateLocalNumCur = do
    --update locals counter
    curFName <- gets curFuncName
    curFuncData <- gets (Map.lookup curFName . defFunc)
    curState <- get
    case curFuncData of
        Nothing -> throwError $ curFName ++ " unsuccessful current function data retrieval"
        Just foundData -> do
            let updatedNumloc = createNewBody (Int ((getFuncNumLoc foundData) + 1)) curFName foundData
            put curState {defFunc = Map.insert curFName updatedNumloc (defFunc curState)}
            
evalDecl :: Type -> [Item] -> QuadCode -> QuadMonad (Env, QuadCode)
evalDecl _ [] qcode = do
    curEnv <- ask
    return (curEnv, qcode)

evalDecl declType ((Init posIn (Ident ident) expr) : rest) qcode = do
    updateLocalNumCur
    (val, updcode, _) <- genQExpr expr --qcode
    increaseNumLocTypesCur val

    countIdent <- gets (Map.lookup ident . countLabels)
    case countIdent of
        Nothing -> do
            insertNewLabelToCounter ident
            newLoc <- alloc
            insertToStoreNewIdentVal ident val newLoc

            let codeWithAsgn = qcode ++ updcode ++ [QAss (QLoc ident (getOrigQType declType)) val]

            local (Map.insert ident newLoc) (evalDecl declType rest codeWithAsgn)

        Just curNumId -> do
            increaseLabelCounter ident
            let newName = ident ++ "_" ++ (show curNumId)
            newLoc <- alloc
            insertToStoreNewIdentVal newName val newLoc

            let codeWithAsgn = qcode ++ updcode ++ [QAss (QLoc newName (getOrigQType declType)) val]

            local (Map.insert ident newLoc) (evalDecl declType rest codeWithAsgn) -- newName changed to ident

evalDecl declType ((NoInit posIn (Ident ident)) : rest) qcode = do
    case declType of
        (Int _) -> evalDecl declType ((Init posIn (Ident ident) (ELitInt posIn 0)) : rest) qcode

specialFuncsList = ["printInt", "printString", "error", "readInt", "readString"]
isSpecialFuncQ fname = checkIfAnyNameFromList specialFuncsList fname

-- generateParams (e:exprs) qcode = do
--     (val, updCode) <- genQExpr val qcode
paramsConcatCode [] qcode = return qcode
paramsConcatCode ((_, paramCode, _) : rest) qcode = paramsConcatCode rest (qcode ++ paramCode)

addParamsFromList [] qcode maxDepth = return (qcode, maxDepth)
addParamsFromList ((paramVal, _, depth) : rest) qcode maxDepth = addParamsFromList rest (qcode ++ [QParam paramVal]) (max maxDepth depth)

genParamCodeForExprList exprList qcode = do
    valsCodes <- mapM genQExpr exprList
    paramGenCode <- paramsConcatCode valsCodes []
    return (addParamsFromList valsCodes paramGenCode)


addToSpecialFuncsIfSpecial fname = do
    if isSpecialFuncQ fname
    then do
        curState <- get
        put curState {specialFunc = (fname : (specialFunc curState))}
    else
        return ()

createTempVarName ident = do
    let candName = ident ++ tmpInfix
    cntLbl <- gets (Map.lookup candName . countLabels)
    case cntLbl of
        Nothing -> do
            insertNewLabelToCounter candName

            return candName

        Just numLabels -> do
            let newName = candName ++ (show numLabels)
            curState <- get
            put curState {countLabels = Map.insert newName (numLabels + 1) (countLabels curState)}

            return newName


genQStmt :: [Stmt] -> QuadCode -> QuadMonad QuadCode
genQStmt [] qcode = return qcode

genQStmt ((BStmt pos (Blk posB stmts)) : rest) qcode = do
    blockCode <- genQStmt stmts []
    genQStmt rest (qcode ++ blockCode)
    -- funcBody <- genQCode stms
    -- update body as funcBody, qcode saved also in writer

genQStmt ((Ret pos expr) : rest) qcode = do
    -- add inf if constant to avoid mov rax repetition
    (retVal, codeExpr, _) <- genQExpr expr --qcode
    genQStmt rest (qcode ++ codeExpr ++ [QRet retVal]) -- mem addr, const, register

genQStmt ((Decl pos vartype items) : rest) qcode = do
    (updatedEnv, updCode) <- evalDecl vartype items qcode
    local (const updatedEnv) (genQStmt rest updCode)

genQStmt ((SExp pos expr) : rest) qcode = do
    (val, updCode, _) <- genQExpr expr --qcode
    genQStmt rest (qcode ++ updCode)

-- fromInteger intVal
genQExpr (ELitInt pos intVal) = return ((IntQVal (fromInteger intVal)), [], 1)

genQExpr (EApp pos (Ident ident) exprList) = do
    addToSpecialFuncsIfSpecial ident
    (updCode, depth) <- genParamCodeForExprList exprList
    appliedFuncData <- gets (Map.lookup ident . defFunc)
    let retType = getFuncRet appliedFuncData
    let newTmpName = createTempVarName ident








