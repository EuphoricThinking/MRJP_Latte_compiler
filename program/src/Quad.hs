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

data ValType = IntQ | StringQ | BoolQ | VoidQ deriving (Eq, Show)

data Val = FnDecl Type [Arg] BNFC'Position | FunQ Val | SuccessQ | FunRetTypeQ | IntQVal Int | ParamQVal String ValType | LocQVal String ValType | VoidQVal | StrQVal String
             deriving (Eq, Show)

type SizeLocals = Int
type RetType = ValType --Val
type Body = QuadCode
type NumIntTypes = Int
type NumStrVars = Int

data ArgData = ArgData String ValType deriving (Show)
type Args = [ArgData] 

data FuncData = FuncData String RetType Args SizeLocals Body NumIntTypes [String] NumStrVars deriving (Show)

data QVar = QLoc String ValType | QArg String ValType | NoMeaning deriving (Show)

data ParamIndicator = JustLocal | Param String deriving (Show)

data Quad = QLabel String --FuncData
    -- add special funcs
    | QRet Val
    | QFunc FuncData
    | QAss QVar Val
    | QParam Val
    | QCall QVar String Int
    | QDecl QVar Val
    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

tmpInfix = "_tmp_"
printInt = "printInt"

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncName = "", specialFunc = [], defFunc = Map.empty, countLabels = Map.empty})

-- let 
    -- p = runQuadGen program
    -- s = QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)}
    -- in
            -- runwriterv $ runexcept $ evalstate (runreader p mapempty) s

runQuadGen :: Program -> QuadMonad (ValType, QStore)
runQuadGen (Prog pos topDefs) = do
    cur_state <- insOneByOne topDefs --get
    -- cur_state <- get
    return (IntQ, cur_state)

getOrigQType (Int _) = IntQ
getOrigQType (Str _) = StringQ
getOrigQType (Void _) = VoidQ

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

getFuncRet (FuncData _ rett _ _ _ _ _ _) = rett
getFuncArgs (FuncData _ _ args _ _ _ _ _) = args
getFuncNumLoc (FuncData _ _ _ numloc _ _ _ _) = numloc
getFuncBody (FuncData _ _ _ _ body _ _ _) = body
getFuncBodyIntsNum (FuncData _ _ _ _ _ numInts _ _) = numInts
getFuncStringList (FuncData _ _ _ _ _ _ strs _) = strs
getFuncNumStrVars (FuncData _ _ _ _ _ _ _ numStrs) = numStrs

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
                newBody = FuncData curFName (getFuncRet curFuncBody) (getFuncArgs curFuncBody) (getFuncNumLoc curFuncBody) body (getFuncBodyIntsNum curFuncBody) (getFuncStringList curFuncBody) (getFuncNumStrVars curFuncBody)
                -- newBody = createNewBody (getFuncNumLoc curFuncBody) curFName curFuncBody
            in
                put curState {defFunc = Map.insert curFName newBody (defFunc curState)} >> return newBody

getArgData (Ar _ (Int _) (Ident ident)) = ArgData ident IntQ
getArgData (Ar _ (Bool _) (Ident ident)) = ArgData ident BoolQ
getArgData (Ar _ (Str _) (Ident ident)) = ArgData ident StringQ

insOneByOne :: [TopDef] -> QuadMonad QStore
insOneByOne [] = do
    cur_state <- get
    return cur_state

insOneByOne ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
    curState <- get
    let newFuncData = FuncData ident (getOrigQType rettype) (map getArgData args) 0 [] 0 [] 0
    insertToStoreNewFunc ident newFuncData
    updateCurFuncName ident

    -- curState <- get
    -- curFName <- gets curFuncName
    --print (show curFName)

    funcBody <- genQStmt stmts []
    -- PERFORM in local env (probably)
    curEnv <- ask
    -- newFullFunc <- updateCurFuncBody funcBody
    newFullFunc <- local (const curEnv) (updateCurFuncBody funcBody)

    tell $ [QFunc newFullFunc]

    insOneByOne rest

-- genQIns [] = return [[]] -- [] should be

-- genQIns ((BStmt pos (Blk posB stmts)) : rest) = (genQIns stmts) : (genQIns rest)
    -- curEnv <- ask
    -- return ((local (const curEnv) (genQIns stmts)) : genQIns

-- evalDecl :: Type' -> [Item'] -> QuadMonad Env

createNewBody (Int numLoc) fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) numLoc (getFuncBody fbody) (getFuncBodyIntsNum fbody) (getFuncStringList fbody) (getFuncNumStrVars fbody)

createIncreaseNumInts numInts fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) ((getFuncBodyIntsNum fbody) + numInts) (getFuncStringList fbody) (getFuncNumStrVars fbody)

addToStringVars strVal fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (strVal : (getFuncStringList fbody)) (getFuncNumStrVars fbody)

updateStringVars strVal fname curBody = do
    curState <- get
    let updatedStringList = addToStringVars strVal fname curBody

    put curState {defFunc = Map.insert fname updatedStringList (defFunc curState)} 

    -- return updatedStringList

createIncreasedStrVarsNum fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (getFuncStringList fbody) ((getFuncNumStrVars fbody) + 1)

updateStringVarsNum fname curBody = do
    curState <- get
    -- curFName <- gets curFuncName
    -- curBody
    let updatedStringVarsNum = createIncreasedStrVarsNum fname curBody

    put curState {defFunc = Map.insert fname updatedStringVarsNum (defFunc curState)} 

    return updatedStringVarsNum

updBothStrNumAndList strVal fname fbody = do
    curState <- get

    let newBody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (strVal : (getFuncStringList fbody)) ((getFuncNumStrVars fbody) + 1)

    put curState {defFunc = Map.insert fname newBody (defFunc curState)}

-- TODO REMEMBER WHEN TO UPDATE!
-- check other possibilities of updating strings
increaseNumLocTypesCur exprVal = do
    curState <- get
    fname <- gets curFuncName
    fbody <- gets (Map.lookup fname . defFunc)
    case fbody of
        Nothing -> throwError $ fname ++ " curfunc not found"
        Just curBody -> do
            case exprVal of
                (IntQVal _) -> do
                    -- updateLocalNumCur

                    let updatedNumInts = createIncreaseNumInts 1 fname curBody
                    put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

                t@(LocQVal tmpName retType) -> do
                    case retType of
                        IntQ -> do 
                            -- updateLocalNumCur

                            let updatedNumInts = createIncreaseNumInts 1 fname curBody
                            put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

                        StringQ -> do
                            updateStringVarsNum fname curBody
                            printMesQ $ "upd " ++ (show t)
                            printMesQ $ (show $ getFuncNumStrVars curBody)
                            --return () --do
                            --printMesQ $ "STR " ++ (show t)



                (ParamQVal tmpName retType) -> do
                    case retType of
                        IntQ -> do 
                            -- updateLocalNumCur

                            let updatedNumInts = createIncreaseNumInts 1 fname curBody
                            put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

                r@(StrQVal strVal) -> do
                    -- printMesQ $ "upd " ++ (show r)
                    -- printMesQ $ (show $ getFuncNumStrVars curBody)
                    -- let updatedStringList = addToStringVars strVal fname curBody
                    -- put curState {defFunc = Map.insert fname updatedStringList (defFunc curState)}
                    -- updateStringVarsNum fname curBody
                    -- updateStringVars strVal fname curBody
                    -- let newBody = (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (strVal : (getFuncStringList fbody)) ((getFuncNumStrVars fbody) + 1)

                    -- put curState {defFunc = Map.insert fname newBody (defFunc curState)}
                    updBothStrNumAndList strVal fname curBody

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
    return (curEnv, qcode)  -- TODO add depth

evalDecl declType ((Init posIn (Ident ident) expr) : rest) qcode = do
    updateLocalNumCur
    (val, updcode, _) <- genQExpr expr JustLocal --qcode --LOOKOUT
    increaseNumLocTypesCur val

    countIdent <- gets (Map.lookup ident . countLabels)
    case countIdent of
        Nothing -> do
            insertNewLabelToCounter ident
            newLoc <- alloc
            insertToStoreNewIdentVal ident val newLoc

            -- CHANGED
            let codeWithAsgn = qcode ++ updcode ++ [QDecl (QLoc ident (getOrigQType declType)) val]

            local (Map.insert ident newLoc) (evalDecl declType rest codeWithAsgn)

        Just curNumId -> do
            increaseLabelCounter ident
            let newName = ident ++ "_" ++ (show curNumId)
            newLoc <- alloc
            insertToStoreNewIdentVal newName val newLoc

            -- CHANGED
            let codeWithAsgn = qcode ++ updcode ++ [QDecl (QLoc newName (getOrigQType declType)) val]

            local (Map.insert ident newLoc) (evalDecl declType rest codeWithAsgn) -- newName changed to ident

evalDecl declType ((NoInit posIn (Ident ident)) : rest) qcode = do
    case declType of
        (Int _) -> evalDecl declType ((Init posIn (Ident ident) (ELitInt posIn 0)) : rest) qcode
        (Str _) -> evalDecl declType ((Init posIn (Ident ident) (EString posIn "")) : rest) qcode

specialFuncsList = ["printInt", "printString", "error", "readInt", "readString"]
isSpecialFuncQ fname = checkIfAnyNameFromList specialFuncsList fname

-- generateParams (e:exprs) qcode = do
--     (val, updCode) <- genQExpr val qcode
paramsConcatCode [] qcode = return qcode
paramsConcatCode ((_, paramCode, _) : rest) qcode = paramsConcatCode rest (qcode ++ paramCode)

-- isStringVarVal (StrQVal _) = True
-- isStringVarVal (StringQ) = True
-- isStringVarVal (LocQVal _ StringQ) = True
-- isStringVarVal (ParamQVal _ StringQ) = True
-- isStringVarVal _ = False -- case of

addParamsFromList [] qcode maxDepth = return (qcode, maxDepth)
addParamsFromList ((paramVal, _, depth) : rest) qcode maxDepth = do
    case paramVal of
        e@(StrQVal s) -> do
            curFName <- gets curFuncName
            cbody <- gets (Map.lookup curFName . defFunc)
            case cbody of
                Nothing -> throwError $ "param creation for " ++ s ++ ": cur func not found"
                Just body -> do
                    -- updateStringVars s curFName body
                    -- updateStringVarsNum curFName body
                    -- printMesQ $ "upd param " ++ (show e)
                    -- printMesQ $ (show $ getFuncNumStrVars body)
                    -- let newBody = 
                    -- let newBody = (getFuncRet body) (getFuncArgs body) (getFuncNumLoc body) (getFuncBody body) (getFuncBodyIntsNum body) (strVal : (getFuncStringList body)) ((getFuncNumStrVars body) + 1)

                    -- put curState {defFunc = Map.insert fname newBody (defFunc curState)}
                    updBothStrNumAndList s curFName body

                    addParamsFromList rest (qcode ++ [QParam paramVal]) (max maxDepth depth)

        _ -> addParamsFromList rest (qcode ++ [QParam paramVal]) (max maxDepth depth)

genParamCodeForExprList exprList isParam = do
    let genExpParams exp = genQExpr exp isParam
    valsCodes <- mapM genExpParams exprList --exprList isParam
    paramGenCode <- paramsConcatCode valsCodes []
    addParamsFromList valsCodes paramGenCode 0

addToSpecialFuncsIfSpecial fname = do
    if isSpecialFuncQ fname
    then do
        curState <- get
        put curState {specialFunc = (fname : (specialFunc curState))}
        return True

        -- case fname of
        --     "printInt" -> do
        --         let printBody = FuncData "printInt" VoidQ [Arg]
    else
        --return ()
        return False

addToSpecialUncond fname = do
    curState <- get
    if checkIfAnyNameFromList (specialFunc curState) fname
    then
        return ()
    else
        put curState {specialFunc = (fname : (specialFunc curState))}

printMesQ mes = lift $ lift $ lift $ lift $ print mes

callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth = do
    case isParam of
                JustLocal -> do
                    let locVal = QLoc newTmpName retType
                    let newCode = updCode ++ [QCall locVal ident (length exprList)]

                    return ((LocQVal newTmpName retType), newCode, depth)

                Param funcName -> do
                    let paramVal = QArg newTmpName retType
                    let newCode = updCode ++ [QCall paramVal ident (length exprList)]

                    return ((ParamQVal newTmpName retType), newCode, depth)

createTempVarName ident = do
    let candName = ident ++ tmpInfix
    cntLbl <- gets (Map.lookup candName . countLabels)
    case cntLbl of
        Nothing -> do
            insertNewLabelToCounter candName

            printMesQ $ "cand " ++ candName

            return candName

        Just numLabels -> do
            let newName = candName ++ (show numLabels)
            curState <- get
            put curState {countLabels = Map.insert candName (numLabels + 1) (countLabels curState)}



            printMesQ $ "newn " ++ newName

            return newName

getSpecialRetType fname =
    case fname of
        "printInt" -> VoidQ
        "readInt" -> IntQ
        "printString" -> VoidQ
        "readString" -> StringQ
        "error" -> VoidQ

getValType val =
    case val of
        (IntQVal _) -> IntQ
        (LocQVal _ vtype) -> vtype
        (ParamQVal _ vtype) -> vtype
        (StrQVal _) -> StringQ

isRawString (StrQVal _) = True
isRawString _ = False

extractString (StrQVal s) = s

genQStmt :: [Stmt] -> QuadCode -> QuadMonad QuadCode
genQStmt [] qcode = return qcode

genQStmt ((BStmt pos (Blk posB stmts)) : rest) qcode = do
    blockCode <- genQStmt stmts []
    genQStmt rest (qcode ++ blockCode)
    -- funcBody <- genQCode stms
    -- update body as funcBody, qcode saved also in writer

genQStmt ((Ret pos expr) : rest) qcode = do
    -- add inf if constant to avoid mov rax repetition
    (retVal, codeExpr, _) <- genQExpr expr JustLocal--qcode
    genQStmt rest (qcode ++ codeExpr ++ [QRet retVal]) -- mem addr, const, register

genQStmt ((Decl pos vartype items) : rest) qcode = do
    (updatedEnv, updCode) <- evalDecl vartype items qcode
    local (const updatedEnv) (genQStmt rest updCode)

genQStmt ((SExp pos expr) : rest) qcode = do
    (val, updCode, _) <- genQExpr expr JustLocal --qcode
    genQStmt rest (qcode ++ updCode)

genQStmt ((Ass pos (Ident ident) expr) : rest) qcode = do
    varVal <- asks (Map.lookup ident)

    case varVal of
        Nothing -> throwError $ ident ++ " unknown variable"
        Just loc -> do
            (val, exprCode, _) <- genQExpr expr JustLocal

            let updCode = qcode ++ exprCode ++ [QAss (QLoc ident (getValType val)) val]
            newLoc <- alloc
            insertToStoreNewIdentVal ident val newLoc

            if isRawString val
            then do
                -- printMesQ $ "RAW " ++ (show val)
                cfname <- gets curFuncName
                cfbody <- gets (Map.lookup cfname . defFunc)
                case cfbody of
                    Nothing -> throwError $ "assignment (re) failed for var: " ++ ident ++ " and val: " ++ (show val)
                    Just body -> do
                        updBothStrNumAndList (extractString val) cfname body

                        local (Map.insert ident newLoc) (genQStmt rest updCode)

            else do
                -- printMesQ $ "NOT RAW: " ++ (show val)
                local (Map.insert ident newLoc) (genQStmt rest updCode)

            


-- fromInteger intVal
genQExpr (ELitInt pos intVal) _ = return ((IntQVal (fromInteger intVal)), [], 1)
            
genQExpr (EString pos strVal) _ = return ((StrQVal strVal), [], 1)

genQExpr (EApp pos (Ident ident) exprList) isParam = do
    let isSpecial = isSpecialFuncQ ident --addToSpecialFuncsIfSpecial ident
    (updCode, depth) <- genParamCodeForExprList exprList isParam

    if isSpecial
    then do
        addToSpecialUncond ident
        newTmpName <- createTempVarName ident
        let retType = getSpecialRetType ident

        callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth
    else do
        fbody <- gets (Map.lookup ident . defFunc)
        -- return ((IntQVal (fromInteger 1)), [], 1)
        case fbody of
            Nothing -> throwError $ ident ++ " function call error: no such function"
            Just appliedFuncData -> do
                let retType = getFuncRet appliedFuncData
                newTmpName <- createTempVarName ident -- move decl depending on param
                callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth

genQExpr (EVar pos (Ident ident)) isParam = do
    curLoc <- asks (Map.lookup ident)
    case curLoc of
        Nothing -> throwError $ ident ++ " var not found"
        Just loc -> do
            curVal <- gets (Map.lookup loc . storeQ)
            case curVal of
                Nothing -> throwError $ "No value in streQ for " ++ ident ++ " at " ++ (show loc)
                Just (curName, val) ->
                    case isParam of
                        JustLocal -> do
                            let locVal = LocQVal curName (getValType val)
                            return (locVal, [], 0)
                        Param fname -> do
                            let paramVal = ParamQVal curName (getValType val)
                            return (paramVal, [], 0)











