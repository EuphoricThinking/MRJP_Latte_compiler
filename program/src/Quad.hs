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

-- POSSIBLE UPGRADE TO SSA (NEW LABELS): countLabels not empited, args are not mapped, but the list is iterated over


--type Loc = Int
--type Env = Map.Map String Loc -- ident, location
type LabelCustom = String
data QStore = QStore {
    storeQ :: Map.Map Loc (LabelCustom, Val), --(Val, Int), -- Int is blockDepth (probably)
    lastLocQ :: Loc,
    curFuncName :: String,
    specialFunc :: [String],
    defFunc :: Map.Map String FuncData,
    countLabels :: Map.Map String Int,
    defClass :: Map.Map String ClassData,
    curClassName :: String
    -- add label map?
} deriving (Show)

data ValType = IntQ | StringQ | BoolQ | VoidQ | ArrayQ ValType | ClassQ String | AttrQ ValType
                deriving (Eq, Show)

data CondType = QEQU | QNE | QGTH | QLTH | QLE | QGE | QAND | QOR deriving (Show)

-- | QGE QVar Val Val -- greate or equal, a >= b
-- | QGTH QVar Val Val -- greater than, a > b
-- | QLTH QVar Val Val -- less than, a < b
-- | QLE QVar Val Val -- less or equal, a <= b

data Val = FnDecl Type [Arg] BNFC'Position | FunQ Val | SuccessQ | FunRetTypeQ | IntQVal Int | ParamQVal String ValType | LocQVal String ValType | VoidQVal | StrQVal String | BoolQVal Bool | Attr ValType | ClassMeth LabelCustom RetType | ClassQObj String | QNull ValType--[Arg]
             deriving (Eq, Show)

type SizeLocals = Int
type RetType = ValType --Val
type Body = QuadCode
type NumIntTypes = Int
type NumStrVars = Int
type NumBools = Int

data ArgData = ArgData String ValType deriving (Show)
type Args = [ArgData] 

data FuncData = FuncData String RetType Args SizeLocals Body NumIntTypes [String] NumStrVars NumBools deriving (Show)

type OffsetClass = Int
type Attributes = Map.Map String (ValType, OffsetClass)
type Methods = Map.Map String (Val, OffsetClass) -- classdata
type OffsetAttr = Int
type OffsetMeth = Int
type AttrList = [(ValType, String)]
type MethList = [String]

data ClassData = ClassData String NumIntTypes [String] NumStrVars NumBools Attributes Methods OffsetAttr OffsetMeth AttrList MethList deriving (Show)

data QVar = QLoc String ValType | QArg String ValType | NoMeaning deriving (Show)

data ParamIndicator = JustLocal | Param String deriving (Show)

data Quad = QLabel String --FuncData
    -- add special funcs
    | QRet Val
    | QFunc String FuncData
    | QAss QVar Val
    | QParam Val
    | QCall QVar String Int
    | QDecl QVar Val
    | QVRet
    | QAdd QVar Val Val
    | QConcat QVar Val Val
    | QSub QVar Val Val
    | QNeg QVar Val
    | QMul QVar Val Val
    | QDiv QVar Val Val
    | QMod QVar Val Val
    | QDec QVar String
    | QInc QVar String
    | QGoTo String -- label
    | QIf Val String -- Val is also a variable
    | QNot QVar Val
    | QCond QVar Val Val CondType
    | JumpCondQ String Val Val CondType
    | QWhile Val String
    | QCondJMPAndOr QVar Val Val CondType
    -- | QTrueJMP String
    | QCmp Val Val
    | QJumpCMP CondType String
    | QArrNew QVar Val -- qvar valType size
    | QAttr QVar Val String -- string -> the attribute name
    | QArrAss Val Val Val -- array elemNum elemVal
    | QArrElem QVar Val Val -- array ident elemNum
    | QClass QVar
    | QCallMethod QVar Val String Int

    deriving (Show)

type QuadCode = [Quad]

type QuadMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT QuadCode IO))) a 

tmpInfix = "_tmp_"
printInt = "printInt"
exprInfix = "_expr_"
concatStr = "___concatenateStrings"
allocArr = "___allocArray"
allocStruct = "___allocStructClass"

arrLenIdent = "length"
selfClassPtr = "self"

defaultPos = (Just (1,1))

-- genQuadcode :: Program -> Quadcode
genQuadcode program = runWriterT $ runExceptT $ evalStateT (runReaderT (runQuadGen program) Map.empty) (QStore {storeQ = Map.empty, lastLocQ = 0, curFuncName = "", specialFunc = [], defFunc = Map.empty, countLabels = Map.empty, defClass = Map.empty, curClassName = ""})

-- let 
    -- p = runQuadGen program
    -- s = QStore {storeQ = Map.empty, lastLocQ = 0, curFuncQ = (FuncData "" [] 0)}
    -- in
            -- runwriterv $ runexcept $ evalstate (runreader p mapempty) s

declareEmptyFuncBodiesWithRets :: [TopDef] -> QuadMonad ()
declareEmptyFuncBodiesWithRets [] = return ()
declareEmptyFuncBodiesWithRets ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
    let emptyBodyFunc = FuncData ident (getOrigQType rettype) [] 0 [] 0 [] 0 0
    insertToStoreNewFunc ident emptyBodyFunc

    declareEmptyFuncBodiesWithRets rest

declareEmptyFuncBodiesWithRets ((ClassDef pos (Ident ident) (CBlock posB stmts)) : rest) = do
    let emptyBodyClass = ClassData ident 0 [] 0 0 Map.empty Map.empty 0 0 [] []
    insertToStoreNewClass ident emptyBodyClass
    updCurClassName ident

    saveClassAttrsMethods ident stmts

    reverseMethsAttrs ident

    declareEmptyFuncBodiesWithRets rest


updCurClassName name = do
    curState <- get
    put curState {curClassName = name}

saveClassAttrsMethods :: String -> [ClassStmt] -> QuadMonad ()
saveClassAttrsMethods _ [] = return ()

saveClassAttrsMethods className ((ClassEmpty pos) :  rest) = saveClassAttrsMethods className rest

saveClassAttrsMethods className ((ClassDecl pos attrType listOfItems) : rest) = do
    declClassAttrs attrType listOfItems

    saveClassAttrsMethods className rest

saveClassAttrsMethods className ((ClassMethod pos retType (Ident ident) args body) : rest) = do

    updateClassMethods className ident retType args

    let methName = getLabelClassMethod className ident
    let emptyBodyFunc = FuncData methName (getOrigQType retType) [] 0 [] 0 [] 0 0
    insertToStoreNewFunc methName emptyBodyFunc


    saveClassAttrsMethods className rest


reverseMethsAttrs className = do
    cdata <- getClassData className
    let meths = extractMethList cdata
    let attrs = extractAttrList cdata
    
    let rMeth = reverseList meths []
    let rAttr = reverseList attrs []

    let updMethData = updClassDataMethList cdata rMeth
    let updMethAttr = updClassDataAttrList updMethData rAttr

    updClassData className updMethAttr


getClassData className = do
    classDataDict <- gets defClass
    let classData = Map.lookup className classDataDict
    case classData of
        Nothing -> throwError $ "No data in store for class " ++ className
        Just cdata -> return cdata

extractClassName (ClassData className _ _ _ _ _ _ _ _ _ _) = className

extractAttrs (ClassData _ _ _ _ _ attrs _ _ _ _ _) = attrs
extractOffsetAttrs (ClassData _ _ _ _ _ _ _ attrsOffset _ _ _) = attrsOffset

extractMethods (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths _ _) = meths
extractMethOffset (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths _ _) = offMeths

extractAttrList (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) = attrList
extractMethList (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) = methList

extractNumIntsClass (ClassData _ numInts _ _ _ _ _ _ _ _ _) = numInts
extractNumPtrsClass (ClassData _ _ _ numPtrs _ _ _ _ _ _ _) = numPtrs
extractNumBools (ClassData _ _ _ _ numBools _ _ _ _ _ _) = numBools

updNumClassPtrs :: String -> QuadMonad ()
updNumClassPtrs className = do
    cdata <- getClassData className
    let newBody = updClassDataNumPtrs cdata
    updClassData className newBody

updNumClassBools :: String -> QuadMonad ()
updNumClassBools className = do
    cdata <- getClassData className
    let newBody = updClassNumBools cdata
    updClassData className newBody

updNumClassInts :: String -> QuadMonad ()
updNumClassInts className = do
    cdata <- getClassData className
    let newBody = updClassNumInts cdata
    updClassData className newBody

updNumClassIntsCur :: QuadMonad ()
updNumClassIntsCur = do
    name <- gets curClassName
    updNumClassInts name

updNumClassBoolsCur :: QuadMonad ()
updNumClassBoolsCur = do
    name <- gets curClassName
    updNumClassBools name

updNumClassPtrsCur :: QuadMonad ()
updNumClassPtrsCur = do
    name <- gets curClassName
    updNumClassPtrs name

reverseList [] acc = acc
reverseList (x:xs) acc = reverseList xs (x:acc)


updClassDataAttrsBody (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) upd_Attrs upd_OffAttr attrName = (ClassData name numInts stringList numPtrs numBools upd_Attrs meths upd_OffAttr offMeths (attrName : attrList) methList)

updClassDataMethList (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) new_MethList = (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList new_MethList)

updClassDataAttrList (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) new_AttrList = (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths new_AttrList methList)

updClassDataMethodsBody (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) upd_Methods upd_OffMeth methName = (ClassData name numInts stringList numPtrs numBools attrs upd_Methods offAttr upd_OffMeth attrList (methName : methList))

updClassDataNumPtrs (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) = (ClassData name numInts stringList (numPtrs + 1) numBools attrs meths offAttr offMeths attrList methList)

updClassNumInts (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) = (ClassData name (numInts + 1) stringList numPtrs numBools attrs meths offAttr offMeths attrList methList)

updClassNumBools (ClassData name numInts stringList numPtrs numBools attrs meths offAttr offMeths attrList methList) = (ClassData name numInts stringList numPtrs (numBools + 1) attrs meths offAttr offMeths attrList methList)

getClassAttrs className = do
    cdata <- getClassData className
    return (extractAttrs cdata)

getLastAttrOffset className = do
    cdata <- getClassData className
    return (extractOffsetAttrs cdata)

getClassMethods className = do
    cdata <- getClassData className
    return (extractMethods cdata)

getClassMethLastOff className = do
    cdata <- getClassData className
    return (extractMethOffset cdata)

updClassData className cdata = do
    curState <- get --s
    put curState {defClass = Map.insert className cdata (defClass curState)}

updateClassAttrs :: String -> String -> ValType -> QuadMonad ()
updateClassAttrs className attrName attrType = do
    cdata <- getClassData className
    attrs <- getClassAttrs className
    lastOff <- getLastAttrOffset className
    -- update offset and map
    let inserted = Map.insert attrName (attrType, lastOff) attrs
    let updCData = updClassDataAttrsBody cdata inserted (lastOff + 1) (attrType, attrName)
    
    curState <- get --s
    put curState {defClass = Map.insert className updCData (defClass curState)}

createClassMethodLabel className methodName = do
    let defName = className ++ "_func_" ++ methodName
    insertNewLabelToCounter defName

    return defName

getLabelClassMethod className methodName = className ++ "_func_" ++ methodName

updateClassMethods className origName retType args = do
    cdata <- getClassData className
    meths <- getClassMethods className

    -- addLater
    --let selfArg = (Ar defaultPos (Class defaultPos (Ident className)))
    methName <- createClassMethodLabel className origName

    let methodVal = ClassMeth methName (getOrigQType retType)
    
    lastOff <- getClassMethLastOff className
    let methInherited = Map.lookup origName meths
    case methInherited of
        Nothing -> do -- the first decl
            let inserted = Map.insert origName (methodVal, lastOff) meths
            let updCData = updClassDataMethodsBody cdata inserted (lastOff + 1) methName

            updClassData className updCData

        Just (val, offset) -> do -- if it overwrites the class Method
            let inserted = Map.insert origName (methodVal, offset) meths
            let updCData = updClassDataMethodsBody cdata inserted lastOff methName

            updClassData className updCData


updateNumAttrs attrType className = do
    case attrType of
        IntQ -> updNumClassInts className
        BoolQ -> updNumClassBools className
        _ -> updNumClassPtrs className


declClassAttrs :: Type -> [ClassItem] -> QuadMonad ()
declClassAttrs _ [] = return ()

declClassAttrs attrType ((CItem pos (Ident ident)) : rest) = do
    let attrTypeQ = getOrigQType attrType
    curClassName <- gets curClassName

    updateClassAttrs curClassName ident attrTypeQ
    updateNumAttrs attrTypeQ curClassName

    -- attrLoc <- alloc  no, that will be during evaluation, now only save names
    declClassAttrs attrType rest


runQuadGen :: Program -> QuadMonad (ValType, QStore)
runQuadGen (Prog pos topDefs) = do
    declareEmptyFuncBodiesWithRets topDefs
    cur_state <- insOneByOne topDefs --get
    -- cur_state <- get
    return (IntQ, cur_state)

getOrigQType (Int _) = IntQ
getOrigQType (Str _) = StringQ
getOrigQType (Void _) = VoidQ
getOrigQType (Bool _) = BoolQ
getOrigQType (Array _ t) = ArrayQ (getOrigQType t)
getOrigQType (Class _ (Ident ident)) = ClassQ ident

alloc :: QuadMonad Loc
alloc = do
    cur_state <- get
    put cur_state {lastLocQ = lastLocQ cur_state + 1}
    return (lastLocQ cur_state + 1)

insertToStoreNewFunc name funcInfo = do
    cur_state <- get
    put cur_state {defFunc = Map.insert name funcInfo (defFunc cur_state)}

insertToStoreNewClass name classInfo = do
    cur_state <- get
    put cur_state {defClass = Map.insert name classInfo (defClass cur_state)}

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

getFuncRet (FuncData _ rett _ _ _ _ _ _ _) = rett
getFuncArgs (FuncData _ _ args _ _ _ _ _ _) = args
getFuncNumLoc (FuncData _ _ _ numloc _ _ _ _ _) = numloc
getFuncBody (FuncData _ _ _ _ body _ _ _ _) = body
getFuncBodyIntsNum (FuncData _ _ _ _ _ numInts _ _ _) = numInts
getFuncStringList (FuncData _ _ _ _ _ _ strs _ _) = strs
getFuncNumStrVars (FuncData _ _ _ _ _ _ _ numStrs _) = numStrs
getFuncNumBools (FuncData _ _ _ _ _ _ _ _ numBools) = numBools

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
                newBody = FuncData curFName (getFuncRet curFuncBody) (getFuncArgs curFuncBody) (getFuncNumLoc curFuncBody) body (getFuncBodyIntsNum curFuncBody) (getFuncStringList curFuncBody) (getFuncNumStrVars curFuncBody) (getFuncNumBools curFuncBody)
                -- newBody = createNewBody (getFuncNumLoc curFuncBody) curFName curFuncBody
            in
                put curState {defFunc = Map.insert curFName newBody (defFunc curState)} >> return newBody

getArgData (Ar _ (Int _) (Ident ident)) = ArgData ident IntQ
getArgData (Ar _ (Bool _) (Ident ident)) = ArgData ident BoolQ
getArgData (Ar _ (Str _) (Ident ident)) = ArgData ident StringQ
getArgData (Ar _ a@(Array _ t) (Ident ident)) = ArgData ident (getOrigQType a)
getArgData (Ar _ (Class _ (Ident className)) (Ident ident)) = ArgData ident (ClassQ className)


updArgsNum numInts numStrs numBools valType =
    case valType of
        IntQ -> ((numInts + 1), numStrs, numBools)
        StringQ -> (numInts, (numStrs + 1), numBools)
        BoolQ -> (numInts, numStrs, (numBools + 1))
        _ -> (numInts, (numStrs + 1), numBools)

saveArgsToEnv [] numInts numStrs numBools = do
    env <- ask
    return (env, numInts, numStrs, numBools)

saveArgsToEnv ((Ar _ argType (Ident ident)) : args) numInts numStrs numBools = do
    -- insert new label to countLabels
    -- insert to storeQ
    -- alloc
    -- let ident = fname ++ "_"
    let valType = (getOrigQType argType)
    let val = (LocQVal ident valType) --(ParamQVal ident argType)
    let (updInts, updStrs, updBools) = updArgsNum numInts numStrs numBools valType

    -- countIdent <- gets (Map.lookup ident . countLabels)
    -- case countIdent of
    --     Nothing -> do
    insertNewLabelToCounter ident
    newLoc <- alloc
    insertToStoreNewIdentVal ident val newLoc

    local (Map.insert ident newLoc) (saveArgsToEnv args updInts updStrs updBools)

        -- Just curNumId -> do
        --     increaseLabelCounter ident
        --     let newName = ident ++ "_" ++ (show curNumId)
        --     newLoc <- alloc
        --     insertToStoreNewIdentVal newName val newLoc

        --     local (Map.insert ident newLoc) (saveArgsToEnv args updInts updStrs)

clearStoreQNewFunc :: QuadMonad ()
clearStoreQNewFunc = do
    curState <- get
    put curState {countLabels = Map.empty}
    -- eventually add labels to args in order to preserve order

applyFunction appliedFuncData ident exprList updCode isParam depth = do
    let retType = getFuncRet appliedFuncData
    updateLocalEAppRetType retType

    newTmpName <- createTempVarName ident -- move decl depending on param
    callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth

processSingleFunction ident args rettype stmts classIdent = do
    env <- ask
    (envWithParams, numInts, numStrs, numBools) <- local (const env) (saveArgsToEnv args 0 0 0)

    -- let newFuncData = FuncData ident (getOrigQType rettype) (map getArgData args) 0 [] 0 [] 0
    printMesQ $ "args " ++ ident ++ " " ++ (show args)
    let newFuncData = FuncData ident rettype (map getArgData args) (numInts + numStrs) [] numInts [] numStrs numBools -- (getOrigQType rettype)

    insertToStoreNewFunc ident newFuncData
    updateCurFuncName ident

    funcBody <- local (const envWithParams) (genQStmt stmts [])
    -- PERFORM in local env (probably)
    --curEnv <- ask
    newFullFunc <- updateCurFuncBody funcBody
    --newFullFunc <- local (const curEnv) (updateCurFuncBody funcBody)

    tell $ [QFunc classIdent newFullFunc]

insOneByOne :: [TopDef] -> QuadMonad QStore
insOneByOne [] = do
    cur_state <- get
    return cur_state

insOneByOne ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
    -- curState <- get

    -- curState <- get
    -- curFName <- gets curFuncName
    --print (show curFName
    processSingleFunction ident args (getOrigQType rettype) stmts ""
    

    insOneByOne rest

insOneByOne ((ClassDef pos (Ident ident) (CBlock posB stmts)) : rest) = do
    updCurClassName ident

    curEnv <- ask --s
    local (const curEnv) (genClassMethods stmts)
    -- genClassMethods stmts

    updCurClassName ""

    insOneByOne rest

saveAttrsToEnv attrType [] = do
    curEnv <- ask --curEnv
    return curEnv

saveAttrsToEnv attrType ((CItem pos (Ident ident)) : rest) = do
    attrLoc <- alloc
    insertToStoreNewIdentVal ident attrType attrLoc -- check whether does not duplicate

    local (Map.insert ident attrLoc) (saveAttrsToEnv attrType rest)


genClassMethods [] = return ()

genClassMethods ((ClassEmpty pos) : rest) = genClassMethods rest

genClassMethods ((ClassDecl pos attrType listOfItems) : rest) = do
    envWithAttrs <- saveAttrsToEnv (Attr (getOrigQType attrType)) listOfItems

    local (const envWithAttrs) (genClassMethods rest)

genClassMethods ((ClassMethod pos retType (Ident ident) args (Blk posB stmts)) : rest) = do
    curClass <- gets curClassName
    let methodName = getLabelClassMethod curClass ident
    let selfArg = (Ar defaultPos (Class defaultPos (Ident curClass)) (Ident selfClassPtr))

    processSingleFunction methodName (selfArg : args) (getOrigQType retType) stmts curClass

    genClassMethods rest

getClassNameFromvtype (ClassQ name) = name
getClassNameFromvtype (AttrQ (ClassQ name)) = name

getVarClassName (LocQVal ident val) = getClassNameFromvtype val

getMethodRet className methName = do
    meths <- getClassMethods className
    let foundMeth = Map.lookup methName meths
    case foundMeth of
        Nothing -> throwError $ "No such method: " ++ methName ++ " in class: " ++ className
        Just ((ClassMeth label rettype), offset) -> return rettype


    

-- genQIns [] = return [[]] -- [] should be

-- genQIns ((BStmt pos (Blk posB stmts)) : rest) = (genQIns stmts) : (genQIns rest)
    -- curEnv <- ask
    -- return ((local (const curEnv) (genQIns stmts)) : genQIns

-- evalDecl :: Type' -> [Item'] -> QuadMonad Env

createNewBody (Int numLoc) fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) numLoc (getFuncBody fbody) (getFuncBodyIntsNum fbody) (getFuncStringList fbody) (getFuncNumStrVars fbody) (getFuncNumBools fbody)

createIncreaseNumInts numInts fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) ((getFuncBodyIntsNum fbody) + numInts) (getFuncStringList fbody) (getFuncNumStrVars fbody) (getFuncNumBools fbody)

addToStringVars strVal fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (strVal : (getFuncStringList fbody)) (getFuncNumStrVars fbody) (getFuncNumBools fbody)

increaseNumInts = do
    curState <- get
    fname <- gets curFuncName
    fbody <- gets (Map.lookup fname . defFunc)
    case fbody of
        Nothing -> throwError $ fname ++ " curfunc not found"
        Just curBody -> do
            let updatedNumInts = createIncreaseNumInts 1 fname curBody
            put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

updateStringVars strVal fname curBody = do
    curState <- get
    let updatedStringList = addToStringVars strVal fname curBody

    put curState {defFunc = Map.insert fname updatedStringList (defFunc curState)} 

increaseStringVarsNum :: QuadMonad ()
increaseStringVarsNum = do
    cfname <- gets curFuncName
    cbody <- gets (Map.lookup cfname . defFunc)
    case cbody of
        Nothing -> throwError $ "No current function " ++ cfname ++ " in funcDef"
        Just body -> updateStringVarsNum cfname body >> return ()

    -- return updatedStringList

createIncreasedStrVarsNum fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (getFuncStringList fbody) ((getFuncNumStrVars fbody) + 1) (getFuncNumBools fbody)

createIncreasedBoolNum fname fbody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (getFuncStringList fbody) (getFuncNumStrVars fbody) ((getFuncNumBools fbody) + 1)

increaseBoolsNum fname fbody = do
    curState <- get
    let updatedNumBools = createIncreasedBoolNum fname fbody
    put curState {defFunc = Map.insert fname updatedNumBools (defFunc curState)}

-- increasePtrsNumWithoutArgs :: QuadMonad
-- increasePtrsNumWithoutArgs = do
--     curName <- gets curFuncName
--     body <- gets (Map.lookup curName . defFunc)
--     case body of
--         Nothing -> throwError $ "No cur unc to increase ptrs number"
--         Just fbody -> updateStringVarsNum curName fbody >> return ()

updateStringVarsNum fname curBody = do
    curState <- get
    -- curFName <- gets curFuncName
    -- curBody
    let updatedStringVarsNum = createIncreasedStrVarsNum fname curBody

    put curState {defFunc = Map.insert fname updatedStringVarsNum (defFunc curState)} 

    return updatedStringVarsNum

createStrVarListTwoVals False False _ _ strVarList = strVarList
createStrVarListTwoVals True True val1 val2 strVarList = (extractString val1) : (extractString val2) : strVarList
createStrVarListTwoVals True False val1 _ strVarList = (extractString val1) : strVarList
createStrVarListTwoVals False True _ val2 strVarList = (extractString val2) : strVarList

updBothStrNumAndList strVal fname fbody = do
    curState <- get

    let newBody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) (strVal : (getFuncStringList fbody)) ((getFuncNumStrVars fbody) + 1) (getFuncNumBools fbody)

    put curState {defFunc = Map.insert fname newBody (defFunc curState)}

updBothStrNumAndListTwoVals val1 val2 fname fbody = do
    curState <- get

    let isRaw1 = isRawString val1
    let isRaw2 = isRawString val2
    let strVarList = createStrVarListTwoVals isRaw1 isRaw2 val1 val2 (getFuncStringList fbody)
    let strVarNumUpd = (cntIsRawString val1) + (cntIsRawString val2)

    let newBody = FuncData fname (getFuncRet fbody) (getFuncArgs fbody) (getFuncNumLoc fbody) (getFuncBody fbody) (getFuncBodyIntsNum fbody) strVarList ((getFuncNumStrVars fbody) + strVarNumUpd) (getFuncNumBools fbody)

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
                    printMesQ $ "inc loc " ++ (show t)
                    case retType of
                        IntQ -> do 
                            -- updateLocalNumCur

                            let updatedNumInts = createIncreaseNumInts 1 fname curBody
                            put curState {defFunc = Map.insert fname updatedNumInts (defFunc curState)}

                        StringQ -> do
                            updateStringVarsNum fname curBody
                            --printMesQ $ "upd " ++ (show t)
                            --printMesQ $ (show $ getFuncNumStrVars curBody)
                            return () --do
                            --printMesQ $ "STR " ++ (show t)
                        BoolQ -> do
                            increaseBoolsNum fname curBody

                        (ArrayQ _) -> do
                            updateStringVarsNum fname curBody -- should be in a new array generation
                            return ()

                        _ -> do
                            increaseStringVarsNum -- should be in a new array generation
                            return ()




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

                (BoolQVal b) -> do
                    -- let updatedNumBools = createIncreasedBoolNum fname curBody
                    -- put curState {defFunc = Map.insert fname updatedNumBools (defFunc curState)}

                    increaseBoolsNum fname curBody

                _ -> do
                    increaseStringVarsNum

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
    printMesQ $ show declType
    case declType of
        (Int _) -> evalDecl declType ((Init posIn (Ident ident) (ELitInt posIn 0)) : rest) qcode
        (Str _) -> evalDecl declType ((Init posIn (Ident ident) (EString posIn "")) : rest) qcode
        ctype@(Class pos id) -> evalDecl declType ((Init posIn (Ident ident) (ENull pos ctype)) : rest) qcode

specialFuncsList = ["printInt", "printString", "error", "readInt", "readString", concatStr, allocArr]
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
                    updateLocalNumCur

                    addParamsFromList rest (qcode ++ [QParam paramVal]) (max maxDepth depth)

        _ -> addParamsFromList rest (qcode ++ [QParam paramVal]) (max maxDepth depth)

-- add for more than six variable? no, the stck clrears itself
genParamCodeForExprList exprList isParam = do
    let genExpParams exp = genQExpr exp isParam
    valsCodes <- mapM genExpParams exprList -- [(val, code, depth)]
    paramGenCode <- paramsConcatCode valsCodes []  -- concatenated codes from vals
    -- (valsCodes, paramGenCode) <- genParametersValsAndCodes exprList isParam
    addParamsFromList valsCodes paramGenCode 0

genParametersValsAndCodes exprList isParam = do
    let genExpParams exp = genQExpr exp isParam
    valsCodes <- mapM genExpParams exprList -- [(val, code, depth)]
    paramGenCode <- paramsConcatCode valsCodes []  -- concatenated codes from vals

    return (valsCodes, paramGenCode)

--genParamsAddSelf exprList isParam 

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

            --printMesQ $ "cand " ++ candName

            return candName

        Just numLabels -> do
            let newName = candName ++ (show numLabels)
            curState <- get
            put curState {countLabels = Map.insert candName (numLabels + 1) (countLabels curState)}



            --printMesQ $ "newn " ++ newName

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
        (BoolQVal _) -> BoolQ
        (ClassQObj ident) -> ClassQ ident
        (Attr vtype) -> AttrQ vtype-- vtype
        (QNull vtype) ->  vtype

isArray (ArrayQ _) = True
isArray _ = False


isRawString (StrQVal _) = True
isRawString _ = False

cntIsRawString (StrQVal _) = 1
cntIsRawString _ = 0

extractString (StrQVal s) = s

createTempVarNameCurFuncExprs = do
    curFName <- gets curFuncName
    resTmpName <- createTempVarName curFName

    return resTmpName

createDecIncQCode ident qcode rest isDecrement = do
    newVarName <- createTempVarNameCurFuncExprs
    -- let locVar = QLoc newVarName IntQ

    varLoc <- asks (Map.lookup ident)
    case varLoc of
        Nothing -> throwError $ ident ++ " variable not declared"
        Just loc -> do
            varLabel <- gets (Map.lookup loc . storeQ)

            case varLabel of
                Nothing -> throwError $ ident ++ " loc: " ++ (show loc) ++ " not found in storeQ"
                Just (curLabel, varVal) -> do
                    let locVar = QLoc newVarName (getValType varVal)
                    if isDecrement then
                        genQStmt rest (qcode ++ [QDec locVar curLabel])--ident])
                    else
                        genQStmt rest (qcode ++ [QInc locVar curLabel])--ident])

createNegOrNotExpr expr isParam isNeg = do
    (val, code, depth) <- genQExpr expr isParam

    updateLocalNumCur

    resTmpName <- createTempVarNameCurFuncExprs
    let locVar = QLoc resTmpName IntQ

    if isNeg then do
        increaseNumInts
        let newCode = code ++ [QNeg locVar val]

        return ((LocQVal resTmpName IntQ), newCode, depth)
    else do
        increaseBoolsWihoutArgs
        let newCode = code ++ [QNot locVar val]

        return ((LocQVal resTmpName IntQ), newCode, depth)

getRelOperandQuad operand qvar val1 val2 =
    case operand of
        (EQU _) -> [QCond qvar val1 val2 QEQU]
        (NE _) -> [QCond qvar val1 val2 QNE]
        (GE _) -> [QCond qvar val1 val2 QGE]
        (GTH _) -> [QCond qvar val1 val2 QGTH]
        (LE _) -> [QCond qvar val1 val2 QLE]
        (LTH _) -> [QCond qvar val1 val2 QLTH]

getJumpCond operand label val1 val2 isJumpNegated =
    let
        newOp = getNegatedJumpOrNormal isJumpNegated operand
    in
        case operand of
            (EQU _) -> [JumpCondQ label val1 val2 newOp]--QNE]
            (NE _) -> [JumpCondQ label val1 val2 newOp] --QEQU]
            (GE _) -> [JumpCondQ label val1 val2 newOp]--QLTH]
            (GTH _) -> [JumpCondQ label val1 val2 newOp]--QLE]
            (LE _) -> [JumpCondQ label val1 val2 newOp]--QGTH]
            (LTH _) -> [JumpCondQ label val1 val2 newOp]--QGE]

getNegatedJumpOrNormal isJumpNegated operand = 
    if isJumpNegated
    then
        case operand of
            (EQU _) -> QNE
            (NE _) -> QEQU
            (GE _) -> QLTH
            (GTH _) -> QLE
            (LE _) -> QGTH
            (LTH _) -> QGE
    else
        case operand of
            (EQU _) -> QEQU
            (NE _) -> QNE
            (GE _) -> QGE
            (GTH _) -> QGTH
            (LE _) -> QLE
            (LTH _) -> QLTH

getQuadIfOrWhile val label isIfElse =
    case isIfElse of
        True -> [QIf val label]
        False -> [QWhile val label]

getCodeAccordingToExprIfElse expr label qcode isIfElse = do
    case expr of
        (ERel pos expr1 operand expr2) -> do
            (val1, code1, depth1) <- genQExpr expr1 JustLocal
            (val2, code2, depth2) <- genQExpr expr2 JustLocal

            let codeAfterCondExpr = qcode ++ code1 ++ code2 ++ (getJumpCond operand label val1 val2 isIfElse) -- jump negated

            return codeAfterCondExpr
        _ -> do
            (val, codeExpr, depth) <- genQExpr expr JustLocal
            let codeAfterCondExpr = qcode ++ codeExpr ++ (getQuadIfOrWhile val label isIfElse) --[QIf val label]

            return codeAfterCondExpr



isRel expr =
    case expr of
        (ERel _ _ _ _) -> True
        _ -> False

getAndOrQCond isAnd qvar val1 val2 =
    case isAnd of
        True -> [QCond qvar val1 val2 QAND]
        False -> [QCond qvar val1 val2 QOR]

getAndOrExpr expr1 isAnd expr2 isParam = do
    (val1, code1, depth1) <- genQExpr expr1 isParam
    (val2, code2, depth2) <- genQExpr expr2 isParam

    resTmpName <- createTempVarNameCurFuncExprs
    let locVar = QLoc resTmpName BoolQ
    let newCode = code1 ++ code2 ++ (getAndOrQCond isAnd locVar val1 val2)

    return ((LocQVal resTmpName BoolQ), newCode, (max depth1 depth2) + 1)

createCondGenJumpMode mode =
    case mode of
        (EQU _) -> QEQU
        (NE _) -> QNE
        (GE _) -> QGE
        (GTH _) -> QGTH
        (LE _) -> QLE
        (LTH _) -> QLTH

increaseBoolsWihoutArgs = do
    fname <- gets (curFuncName)
    body <- gets (Map.lookup fname . defFunc)
    case body of
        Nothing -> throwError $ "No cur func in increase bool"
        Just fbody -> increaseBoolsNum fname fbody 


changeExprToGenCond expr = do
    --lTrue <- createTempVarNameCurFuncExprs
    lFalse <- createTempVarNameCurFuncExprs
    lEnd <- createTempVarNameCurFuncExprs

    resTmpName <- createTempVarNameCurFuncExprs
    let locVar = QLoc resTmpName BoolQ

    --increaseBoolsNum
    increaseBoolsWihoutArgs
    updateLocalNumCur

    (val, code, depth) <- genCond expr lEnd lFalse  --lTrue lFalse
-- initialize, check, reassign
    --let ifElseAssignCode = [(QDecl locVar (BoolQVal True))] ++ code ++ [(QLabel lTrue), (QDecl locVar (BoolQVal True)), (QGoTo lEnd), (QLabel lFalse), (QDecl locVar (BoolQVal False)), (QLabel lEnd) ]
    let ifElseAssignCode = [(QDecl locVar (BoolQVal True))] ++ code ++ [(QLabel lFalse), (QAss locVar (BoolQVal False)), (QLabel lEnd) ]


    return ((LocQVal resTmpName BoolQ), ifElseAssignCode, depth)

singleValsGenCond expr lTrue lFalse = do
    (val@(LocQVal callTmpName retType), code, depth) <- genQExpr expr JustLocal

    resTmpName <- createTempVarNameCurFuncExprs
    --printMesQ $ "eapp cond  " ++ resTmpName ++ " " ++ (show val)
    increaseBoolsWihoutArgs
    updateLocalNumCur

    let locVar = QLoc callTmpName retType--resTmpName BoolQ

    -- let newCode = code ++ [(QCondJMPAndOr locVar val (BoolQVal True) QAND), (QJumpCMP QNE lTrue), (QGoTo lFalse)]
    let newCode = code ++ [(QCond locVar val (BoolQVal True) QAND), (QJumpCMP QNE lTrue), (QGoTo lFalse)]

    return ((LocQVal callTmpName BoolQ), newCode, depth) -- resTmpName)

eLitToGenCode expr lTrue lFalse = do
    resTmpName <- createTempVarNameCurFuncExprs
    let locVar = QLoc resTmpName BoolQ

    increaseBoolsWihoutArgs
    updateLocalNumCur


    (val, code, depth) <- genQExpr expr JustLocal

    let eLitCode = code ++ [(QCond locVar val (BoolQVal True) QAND), (QJumpCMP QNE lTrue), (QGoTo lFalse)]

    return (val, eLitCode, depth)
-- first Ltrue label, not equal = ZF = 1 (`and` result)

updateLocalEAppRetType retType = do
    updateLocalNumCur
    case retType of
        VoidQ -> return ()
        IntQ -> increaseNumInts
        StringQ -> increaseStringVarsNum
        BoolQ -> increaseBoolsWihoutArgs
        _ -> increaseStringVarsNum

getArrElemType (LocQVal _ (ArrayQ t)) = t

getIdentString (Ident i) = i

getCurnamExprRet ((LocQVal curname _), _, _) = curname



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

    if isRawString retVal
    then do
        cfname <- gets curFuncName
        cfbody <- gets (Map.lookup cfname . defFunc)
        case cfbody of
            Nothing -> throwError $ "returned string not found: " ++ (show retVal)
            Just body -> do
                -- updBothStrNumAndList (extractString val) cfname body
                updateStringVars (extractString retVal) cfname body

                genQStmt rest (qcode ++ codeExpr ++ [QRet retVal])

    else
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

            curLabelVal <- gets (Map.lookup loc . storeQ)

            case curLabelVal of
                Nothing -> throwError $ ident ++ "unassigned value"
                Just (curLabel, oldVal) -> do
                    printMesQ $ "ass " ++ (show val)
                    let updCode = qcode ++ exprCode ++ [QAss (QLoc curLabel (getValType val)) val]
                    newLoc <- alloc
                    -- insertToStoreNewIdentVal ident val newLoc
                    insertToStoreNewIdentVal curLabel val newLoc

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

genQStmt ((Empty _) : rest) qcode = genQStmt rest qcode

genQStmt ((VRet _) : rest) qcode = genQStmt rest (qcode ++ [QVRet])

genQStmt ((Decr _ (Ident ident)) : rest) qcode = createDecIncQCode ident qcode rest True

genQStmt ((Incr _ (Ident ident)) : rest) qcode = createDecIncQCode ident qcode rest False

genQStmt ((Cond _ expr stmt) : rest) qcode = do
    -- (val, codeExpr, depth) <- genQExpr expr JustLocal

    -- if false -> jump further
    labelTrue <- createTempVarNameCurFuncExprs
    labelFalse <- createTempVarNameCurFuncExprs -- after if block
    
    -- case expr of
    --     (ERel pos expr1 operand expr2) -> do
    --         (val1, code1, depth1) <- genQExpr expr1 JustLocal
    --         (val2, code2, depth2) <- genQExpr expr2 JustLocal

    --         let codeAfterCondExpr = qcode ++ codeExpr ++ [QIf val labelFalse]
    --codeAfterCondExpr <- getCodeAccordingToExprIfElse expr labelFalse qcode True
    (val, code, depth) <- genCond expr labelTrue labelFalse
    let codeAfterCondExpr = qcode ++ code ++ [QLabel labelTrue]

    stmtCode <- genQStmt [stmt] codeAfterCondExpr

    genQStmt rest (stmtCode ++ [QLabel labelFalse])

        -- _ -> do
        --     let codeAfterCondExpr = qcode ++ codeExpr ++ [QIf val labelFalse]

        --     stmtCode <- genQStmt [stmt] codeAfterCondExpr

        --     genQStmt rest (stmtCode ++ [QLabel labelFalse])


genQStmt ((CondElse pos expr1 stm1 stm2) : rest) qcode = do
    -- (val, codeExpr, depth) <- genQExpr expr1 JustLocal

    -- labelEnd <- createTempVarNameCurFuncExprs
    -- labelElse <- createTempVarNameCurFuncExprs
    -- --let codeAfterCondExpr = qcode ++ codeExpr ++ [QIf val labelElse]
    -- codeAfterCondExpr <- getCodeAccordingToExprIfElse expr1 labelElse qcode True

    -- stmtsTrue <- genQStmt [stm1] codeAfterCondExpr

    -- let codeAfterTrueToEnd = stmtsTrue ++ [(QGoTo labelEnd), (QLabel labelElse)] 

    -- stmtsElse <- genQStmt [stm2] codeAfterTrueToEnd

    -- genQStmt rest (stmtsElse ++ [QLabel labelEnd])
    labelTrue <- createTempVarNameCurFuncExprs
    labelFalse <- createTempVarNameCurFuncExprs
    labelEnd <- createTempVarNameCurFuncExprs

    (val1, code1, depth1) <- genCond expr1 labelTrue labelFalse
    let codeAftCond = qcode ++ code1 ++ [QLabel labelTrue]
    
    codeTrue <- genQStmt [stm1] codeAftCond
    let codeAftTrue = codeTrue ++ [(QGoTo labelEnd), (QLabel labelFalse)] 

    codeElse <- genQStmt [stm2] codeAftTrue

    genQStmt rest (codeElse ++ [QLabel labelEnd])

genQStmt ((While pos condExpr stmt) : rest) qcode = do
    labelCond <- createTempVarNameCurFuncExprs
    labelStart <- createTempVarNameCurFuncExprs
    labelEnd <- createTempVarNameCurFuncExprs

    let codeStart = qcode ++ [(QGoTo labelCond), (QLabel labelStart)]

    stmtsCode <- genQStmt [stmt] codeStart

    let codeCond = stmtsCode ++ [QLabel labelCond]

    -- codeAfterCondExpr <- getCodeAccordingToExprIfElse condExpr labelStart codeCond False
    (val, code, depth) <- genCond condExpr labelStart labelEnd
    let codeAfterCondExpr = codeCond ++ code ++ [QLabel labelEnd]

    genQStmt rest codeAfterCondExpr

genQStmt ((AssArr pos exprArrVar exprElemNum exprElemVal) : rest) qcode = do
    (val1, code1, depth1) <- genQExpr exprArrVar JustLocal
    (val2, code2, depth2) <- genQExpr exprElemNum JustLocal
    (val3, code3, depth3) <- genQExpr exprElemVal JustLocal
    printMesQ $ "elemval " ++ (show exprElemVal) ++ " loc: " ++ (show val3) 

    genQStmt rest (qcode ++ code1 ++ code2 ++ code3 ++ [QArrAss val1 val2 val3])


genQStmt ((For pos varType varIdent arrExpr stmts) : rest) qcode = do
    (updatedEnv, updCode) <- evalDecl varType [(NoInit pos varIdent)] qcode
    
    counterName <- createTempVarNameCurFuncExprs
    let cntId = (Ident counterName)

    (updEnv, uCode) <- local (const updatedEnv) (evalDecl (Int pos) [(NoInit pos cntId)] updCode)

    labelCond <- createTempVarNameCurFuncExprs
    labelStart <- createTempVarNameCurFuncExprs
    labelEnd <- createTempVarNameCurFuncExprs

    let codeStart = uCode ++ [(QGoTo labelCond), (QLabel labelStart)]

    --(val, code, depth) <- genQExpr arrExpr JustLocal

    let cntEvar = (EVar pos cntId)
    let assCode = (Ass pos varIdent (EArrEl pos arrExpr cntEvar))
    let innerStmts = ((assCode) : [stmts]) ++ [Incr pos cntId]

    stmtsCode <- local (const updEnv) (genQStmt innerStmts codeStart)

    -- newVarName <- createTempVarNameCurFuncExprs
    -- let locVar = QLoc newVarName IntQ
    -- let cntInc = [QInc locVar (getIdentString varIdent)]

    let codeCond = stmtsCode ++ [QLabel labelCond]

    let condExpr = (ERel pos cntEvar (LTH pos) (EAttr pos arrExpr (Ident arrLenIdent)))

    (val, code, depth) <- local (const updEnv) (genCond condExpr labelStart labelEnd)
    let codeAfterCondExpr = codeCond ++ code ++ [QLabel labelEnd]

    genQStmt rest codeAfterCondExpr


    -- let stmsCode = assCode : stmts
    -- let insCode = (While pos condexpr stmtsCode) : rest
    -- --let updQCode = qcode ++ uCode

    -- local (const updEnv) (genQStmt insCode uCode) 






-- fromInteger intVal
genQExpr (ELitInt pos intVal) _ = return ((IntQVal (fromInteger intVal)), [], 1)
            
genQExpr (EString pos strVal) _ = return ((StrQVal strVal), [], 1)

genQExpr (ELitTrue _) _ = return ((BoolQVal True), [], 1)

genQExpr (ELitFalse _) _ = return ((BoolQVal False), [], 1)

genQExpr (EApp pos (Ident ident) exprList) isParam = do
    let isSpecial = isSpecialFuncQ ident --addToSpecialFuncsIfSpecial ident
    (updCode, depth) <- genParamCodeForExprList exprList isParam

    -- updateLocalNumCur

    if isSpecial
    then do
        addToSpecialUncond ident
        newTmpName <- createTempVarName ident
        let retType = getSpecialRetType ident
        updateLocalEAppRetType retType

        callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth
    else do
        --funcs<- gets (defFunc)
        --printMesQ $ show (funcs)
        curClass <- gets curClassName
        if curClass == ""
        then do
            fbody <- gets (Map.lookup ident . defFunc)
            -- return ((IntQVal (fromInteger 1)), [], 1)
            case fbody of
                Nothing -> throwError $ ident ++ " function call error: no such function"  -- if class -> get classLabel
                Just appliedFuncData -> do
                    -- let retType = getFuncRet appliedFuncData
                    -- updateLocalEAppRetType retType

                    -- newTmpName <- createTempVarName ident -- move decl depending on param
                    -- callFuncParamOrLocal ident newTmpName retType exprList updCode isParam depth
                    applyFunction appliedFuncData ident exprList updCode isParam depth
        else do
            let methName = getLabelClassMethod curClass ident
            mbody <- gets (Map.lookup methName . defFunc)
            case mbody of
                Nothing -> throwError $ ident ++ " function call error: no such method in class " ++ curClass  -- if class -> get classLabel
                Just appliedFuncData -> do
                    let newExprlist = (EVar defaultPos (Ident selfClassPtr)) : exprList
                    applyFunction appliedFuncData methName newExprlist updCode isParam depth

genQExpr v@(EVar pos (Ident ident)) isParam = do
    --printMesQ $ "quad " ++ (show v) ++ (show isParam)
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
                            printMesQ $ "evar " ++ (show val)
                            let locVal = LocQVal curName (getValType val)
                            return (locVal, [], 0)
                        Param fname -> do
                            let paramVal = ParamQVal curName (getValType val)
                            return (paramVal, [], 0)

genQExpr (EAdd pos expr1 (Plus posP) expr2) isParam = do
    (val1, code1, depth1) <- genQExpr expr1 isParam
    (val2, code2, depth2) <- genQExpr expr2 isParam

    updateLocalNumCur


    curFName <- gets curFuncName
    resTmpName <- createTempVarName curFName

    case getValType val1 of
        IntQ -> do
            let locVar = QLoc resTmpName IntQ
            let newCode = code1 ++ code2 ++ [QAdd locVar val1 val2]
            increaseNumInts

            return ((LocQVal resTmpName IntQ), newCode, (max depth1 depth2) + 1)

        StringQ -> do
            let concVar = QLoc resTmpName StringQ
            let newCode = code1 ++ code2 ++ [QConcat concVar val1 val2]

            addToSpecialUncond concatStr
-- is string literal
            cfbody <- gets (Map.lookup curFName . defFunc)
            case cfbody of
                Nothing -> throwError $ "Quad Concat error: no cur func"
                Just curBody -> do
                
                    updated <- updateStringVarsNum curFName curBody -- for itermediate (or final) result
                    -- updateLocalNumCur


                    updBothStrNumAndListTwoVals val1 val2 curFName updated --curBody -- checks if any of the arguments is a raw string, adds to string list (data section)

                    return ((LocQVal resTmpName StringQ), newCode, (max depth1 depth2) + 1)


genQExpr (EAdd pos expr1 (Minus posP) expr2) isParam = do
    (val1, code1, depth1) <- genQExpr expr1 isParam
    (val2, code2, depth2) <- genQExpr expr2 isParam

    increaseNumInts
    updateLocalNumCur

    resTmpName <- createTempVarNameCurFuncExprs

    let locVar = QLoc resTmpName IntQ
    let newCode = code1 ++ code2 ++ [QSub locVar val1 val2]

    return ((LocQVal resTmpName IntQ), newCode, (max depth1 depth2) + 1)

genQExpr (Neg pos expr) isParam = createNegOrNotExpr expr isParam True
    

genQExpr exprNot@(Not pos expr) isParam = changeExprToGenCond exprNot --createNegOrNotExpr expr isParam False

genQExpr (EMul pos expr1 mulOperand expr2) isParam = do
    (val1, code1, depth1) <- genQExpr expr1 isParam
    (val2, code2, depth2) <- genQExpr expr2 isParam

    increaseNumInts
    updateLocalNumCur

    resTmpName <- createTempVarNameCurFuncExprs

    let locVar = QLoc resTmpName IntQ

    case mulOperand of
        (Times _) -> do
            let newCode = code1 ++ code2 ++ [QMul locVar val1 val2]

            return ((LocQVal resTmpName IntQ), newCode, (max depth1 depth2) + 1)

        (Div _) -> do
            let newCode = code1 ++ code2 ++ [QDiv locVar val1 val2]

            return ((LocQVal resTmpName IntQ), newCode, (max depth1 depth2) + 1)

        (Mod _) -> do
            let newCode = code1 ++ code2 ++ [QMod locVar val1 val2]

            return ((LocQVal resTmpName IntQ), newCode, (max depth1 depth2) + 1)

genQExpr (ERel pos expr1 operand expr2) isParam = do
    (val1, code1, depth1) <- genQExpr expr1 isParam
    (val2, code2, depth2) <- genQExpr expr2 isParam

    increaseBoolsWihoutArgs
    updateLocalNumCur

    resTmpName <- createTempVarNameCurFuncExprs
    let locVar = QLoc resTmpName BoolQ
    let newCode = code1 ++ code2 ++ (getRelOperandQuad operand locVar val1 val2)

    return ((LocQVal resTmpName BoolQ), newCode, (max depth1 depth2) + 1)

genQExpr expr@(EAnd pos expr1 expr2) isParam = changeExprToGenCond expr
    

    
    
    --getAndOrExpr expr1 True expr2 isParam

genQExpr expr@(EOr pos expr1 expr2) isParam = changeExprToGenCond expr--getAndOrExpr expr1 False expr2 isParam

-- arrays
-- a new array
genQExpr (EArr pos elemType sizeExpr) isParam = do
    (val, code, depth) <- genQExpr sizeExpr JustLocal

    addToSpecialUncond allocArr

    updateLocalNumCur
    increaseStringVarsNum


    resTempName <- createTempVarNameCurFuncExprs
    let arrType = (ArrayQ (getOrigQType elemType))
    let locVar = QLoc resTempName arrType
    let newCode = code ++ [QArrNew locVar val]

    return ((LocQVal resTempName arrType), newCode, depth + 1)

-- ident := classStructArrName
genQExpr (EAttr pos expr (Ident attrName)) isParam = do
    -- evar.evar.evar.IDENT
    (val, code, depth) <- genQExpr expr JustLocal

    resTempName <- createTempVarNameCurFuncExprs

    -- printMesQ (show val)

    -- return ((LocQVal resTempName IntQ), [], depth + 1)
    printMesQ $ "eattr " ++ (show val)
    if isArray (getValType val)
    then do
        let locVal = QLoc resTempName IntQ
        let newCode = code ++ [QAttr locVal val attrName]

        increaseNumInts
        updateLocalNumCur
        -- increase local var num types according to evaluation in leafs

        return ((LocQVal resTempName IntQ), newCode, depth+1)
    else do
        printMesQ $ "not an array"
        let locVal = QLoc resTempName IntQ
        let newCode = code ++ [QAttr locVal val attrName]
        return ((LocQVal resTempName IntQ), newCode, depth+1)

genQExpr (EArrEl pos exprVar exprElemNum) isParam = do
    (valVar, codeVar, depthVar) <- genQExpr exprVar isParam
    (valNum, codeNum, depthNum) <- genQExpr exprElemNum isParam

    resTempName <- createTempVarNameCurFuncExprs

    let arrType = getArrElemType valVar
    let locVal = QLoc resTempName arrType
    let newCode = codeVar ++ codeNum ++ [QArrElem locVal valVar valNum]

    return ((LocQVal resTempName arrType), newCode, (max depthVar depthNum) + 1)

-- classes
-- a new class
genQExpr (EClass pos (Class posC (Ident className))) isParam = do
    addToSpecialUncond allocStruct
    updateLocalNumCur
    increaseStringVarsNum

    resTempName <- createTempVarNameCurFuncExprs
    let classType = ClassQ className
    let locVar = QLoc resTempName classType

    return ((LocQVal resTempName classType), [QClass locVar], 1)

genQExpr (EMethod pos exprClass (Ident methodName) exprList) isParam = do
    (valClass, codeClass, depthClass) <- genQExpr exprClass isParam
    (updCode, depth) <- genParamCodeForExprList (exprClass : exprList) isParam
    
    resTempName <- createTempVarNameCurFuncExprs
    let className = getVarClassName valClass
    methRet <- getMethodRet className methodName

    let locVal = QLoc resTempName methRet

    let newCode = updCode ++ [QCallMethod locVal valClass methodName ((length exprList) + 1)]

    return ((LocQVal resTempName methRet), newCode, depth)

    -- updateLocalEAppRetType methRet

    -- (valClass, codeClass, depthClass) <- genQExpr exprClass isParam
    -- (updCode, depth) <- genParamCodeForExprList exprList isParam
    
    -- resTempName <- createTempVarNameCurFuncExprs

    -- printMesQ $ "getvar " ++ (show valClass)
    -- let className = getVarClassName valClass
    -- methRet <- getMethodRet className methodName

    -- updateLocalEAppRetType methRet

    -- let locVal = QLoc resTempName methRet
    -- let newCode = codeClass ++ [QParam valClass] ++ updCode ++ [QCallMethod locVal valClass methodName ((length exprList) + 1)]

    -- return ((LocQVal resTempName methRet), newCode, (max depth depthClass) + 1)

genQExpr (ENull pos objType) _ =
    case objType of
        (Class pos (Ident ident)) -> return ((QNull (ClassQ ident)), [], 1)


    -- let methName = getLabelClassMethod className methodName

    -- mbody <- gets (Map.lookup methName . defFunc)
    -- case mbody of
    --     Nothing -> throwError $ ident ++ " method call error: no such method in class " ++ className  -- if class -> get classLabel
    --     Just mdata -> do
    --         let newExprlist = (EVar defaultPos (Ident selfClassPtr)) : exprList
    --         applyFunction appliedFuncData methName newExprlist updCode isParam depth
    -- updaet ret type

    



genCond v@(EVar pos (Ident ident)) lTrue lFalse = singleValsGenCond v lTrue lFalse

    --printMesQ ("var " ++ ident) >> genQExpr v JustLocal
genCond v@(ELitFalse _) lTrue lFalse = eLitToGenCode v lTrue lFalse -- genQExpr v JustLocal
genCond v@(ELitTrue _) lTrue lFalse = eLitToGenCode v lTrue lFalse-- genQExpr v JustLocal
-- comparison between numbers is handled in gencond erel
genCond v@(EApp pos (Ident ident) exprList) lTrue lFalse = singleValsGenCond v lTrue lFalse

genCond v@(EMethod pos exprClass (Ident methodName) exprList) lTrue lFalse = singleValsGenCond v lTrue lFalse

    -- not equal - ZF = 1 -> true && true


genCond e@(ERel pos expr1 operand expr2) lTrue lFalse = do
    (val1, code1, depth1) <- genQExpr expr1 JustLocal
    (val2, code2, depth2) <- genQExpr expr2 JustLocal

    --increaseNumInts
    increaseBoolsWihoutArgs
    updateLocalNumCur

    resTmpName <- createTempVarNameCurFuncExprs

    let varLoc = QLoc resTmpName BoolQ
    let mode = createCondGenJumpMode operand
    -- let newCode = code1 ++ code2 ++ [(QCmp val1 val2), (QJumpCMP (createCondGenJumpMode operand) lTrue), (QGoTo lFalse)]
    let newCode = code1 ++ code2 ++ [(QCond varLoc val1 val2 mode),  (QJumpCMP mode lTrue), (QGoTo lFalse)]

    return ((LocQVal resTmpName BoolQ), newCode, (max depth1 depth2 ) + 1)

genCond (EAnd pos expr1 expr2) lTrue lFalse = do
    resTmpName <- createTempVarNameCurFuncExprs

    increaseBoolsWihoutArgs
    updateLocalNumCur

    lMid <- createTempVarNameCurFuncExprs

    (val1, code1, depth1) <- genCond expr1 lMid lFalse

    let codeAft1 = code1 ++ [QLabel lMid]

    (val2, code2, depth2) <- genCond expr2 lTrue lFalse

    let locVar = QLoc resTmpName BoolQ
    -- let codeAft2 = codeAft1 ++ code2 ++ [(QCondJMPAndOr locVar val1 val2 QAND), (QJumpCMP QNE lTrue), (QGoTo lFalse)] -- //(QTrueJMP lTrue), (QGoTo LFalse)]
    let codeAft2 = codeAft1 ++ code2 ++ [(QCond locVar val1 val2 QAND), (QJumpCMP QNE lTrue), (QGoTo lFalse)]

    --return 
    return ((LocQVal resTmpName BoolQ), codeAft2, (max depth1 depth2 ) + 1)

genCond (EOr pos expr1 expr2) lTrue lFalse = do
    resTmpName <- createTempVarNameCurFuncExprs

    lMid <- createTempVarNameCurFuncExprs
    (val1, code1, depth1) <- genCond expr1 lTrue lMid
    let codeAft1 = code1 ++ [QLabel lMid]
    (val2, code2, depth2) <- genCond expr2 lTrue lFalse

    --increaseBoolsNum
    increaseBoolsWihoutArgs
    updateLocalNumCur

    let locVar = QLoc resTmpName BoolQ
    -- let codeAft2 = codeAft1 ++ code2 ++ [(QCondJMPAndOr locVar val1 val2 QOR), (QJumpCMP QNE lTrue), (QGoTo lFalse)] -- // (QTrueJMP lTrue), (QGoto lFalse)]
    let codeAft2 = codeAft1 ++ code2 ++ [(QCond locVar val1 val2 QOR),(QJumpCMP QNE lTrue), (QGoTo lFalse)] -- // (QTrueJMP lTrue), (QGoto lFalse)]

    return ((LocQVal resTmpName BoolQ), codeAft2, (max depth1 depth2 ) + 1)

genCond (Not pos expr) lTrue lFalse = genCond expr lFalse lTrue --do
    -- resTmpName <- createTempVarNameCurFuncExprs
    -- (val, code, depth) <- genCond expr lFalse lTrue

    -- let locVar = QLoc resTmpName BoolQ

