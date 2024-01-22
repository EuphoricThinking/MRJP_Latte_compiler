{-# LANGUAGE FlexibleContexts #-}
-- module Typechecker where

import Latte.Abs
import Latte.Lex
import Latte.Par

import System.IO
import System.Environment

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.Exit

mainName = "main"

-- depth - depth in if...else clause
-- ifDepth - depth in the if or while clause; indicator of a not finished program
-- blockDepth - depth in the inner block (at entering function body set to 0)


main :: IO () 
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= parseFile    
        [filename] -> readFile filename >>= parseFile 

getFileStrinContent :: FilePath -> IO ()
getFileStrinContent path = readFile path >>= parseFile

parseFile :: String -> IO () 
parseFile fileContent =
    let
      tokens = myLexer fileContent
      parsed = pProgram tokens
    in executeProgram parsed

display_tokens :: [Token] -> IO()
display_tokens tokens =  do 
  let 
    parsed = pProgram tokens in
      print parsed


data Value = FnDecl Type [Arg] BNFC'Position | IntT | StringT | BoolT | VoidT | FunT Value | Success | FunRetType | ClassType String | ClassCode ClassBody
             deriving (Eq)

type IfElseRet = Bool
type FreeRet = Bool
type RetType = Type
type Pos = BNFC'Position
data CurFuncData = CurFuncData String IfElseRet FreeRet RetType Pos deriving (Show)

instance Show Value where
    show Success = "Success"
    show BoolT = "BoolT"
    show IntT = "IntT"
    show StringT = "StringT"
    show VoidT = "VoidT"
    show (FnDecl _ _ pos) = "FnDecl " ++ (show pos)
    show (FunT v) = "FunT " ++ (show v)
    show (ClassType s) = "ClassType " ++ s

-- Store przechowuje wszystkie zmienne przez cały czas
-- Env wskazuje na lokacje aktualnie widocznych zmiennych
-- Env -> Store -> Either String (a, Store)

type Loc = Int
type Env = Map.Map String Loc
data Store = Store {
    store :: Map.Map Loc (Value, Int), -- Int is blockDepth (probably)
    lastLoc :: Loc,
    curFunc :: CurFuncData,
    classStruct :: Map.Map String (Map.Map String Value),
    classEnv :: Map.Map String Env,
    isInClass :: Bool
    --Env :: Map.Map String Loc -- env after checking topdefs
} deriving (Show)

type InterpreterMonad a = ReaderT Env (StateT Store (ExceptT String IO)) a 

selfClassEntity = "self"

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

insertNewClass className = do
    curState <- get

    let mapWithSelf = Map.insert selfClassEntity (ClassType className) Map.empty -- instead of an empty map

    put curState {classStruct = Map.insert className mapWithSelf (classStruct curState)}

insertNewClassEnv className = do
    curState <- get
    put curState {classEnv = Map.insert className (Map.empty) (classEnv curState)}

updateClassEnvInStore className newEnv = do
    curState <- get
    put curState {classEnv = Map.insert className newEnv (classEnv curState)}

insertNewAttrMeth className attrMethModifiedMap = do
    curState <- get

    put curState {classStruct = Map.insert className attrMethModifiedMap (classStruct curState)}

evalMaybe :: String -> Maybe a -> InterpreterMonad a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a

getTypeFromMaybe (Just a) = return a

getEitherMessage (Left mes) = mes
getEitherMessage (Right mes) = mes

printMes mes = lift $ lift $ lift $ putStrLn mes

printError mes = hPutStrLn stderr ("ERROR\n" ++ mes)
printOK = hPutStrLn stderr "OK"

checkError :: ExceptT String IO Value -> IO()
checkError resWrapped = do
    res <- runExceptT $ resWrapped
    case res of
        Left mes -> printError mes >> exitFailure
        Right _ -> printOK >> exitSuccess

executeProgram :: Either String Program -> IO ()
executeProgram program = 
    case program of
        Left mes -> printError mes >> exitFailure
        Right p -> checkError $ evalStateT (runReaderT (executeRightProgram p) Map.empty) (Store {store = Map.empty, lastLoc = 0, curFunc = (CurFuncData "" False False (Void Nothing) Nothing), classStruct = Map.empty, classEnv = Map.empty, isInClass = False})--, basalEnv = Map.empty}) 


printSth mes = lift $ lift $ lift $ print mes

fromJust (Just sth) = sth

-- updateBasalEnv newEnv = do
--     curState <- get
--     put curState {basalEnv = newEnv}

executeRightProgram :: Program -> InterpreterMonad Value  
executeRightProgram (Prog pos topDefs) = 
    do
        -- save functions and class name,
        -- class data is not in store, only names in env
        envWithFuncDecl <- findFuncDecl topDefs

        -- updateBasalEnv envWithFuncDecl

        -- class attributes and method names are stored,
        -- but not analyzed (whether delcared class exists or whether the given method is correct)
        local (const envWithFuncDecl) (saveClassInnerData topDefs)
    
        case Map.lookup "main" envWithFuncDecl of
            Nothing -> throwError $ "No main method defined"
            _ ->  local (const envWithFuncDecl) (checkFunction topDefs)


setIsClass boolVal = do
    curState <- get
    put curState {isInClass = boolVal}

initCurFuncData funcName retType pos = do
    curState <- get
    put curState {curFunc = (CurFuncData funcName False False retType pos)}


getFuncRettype (Just ((FnDecl rettype args _), _)) = rettype

getFuncArgs (Just ((FnDecl rettype args _), _)) = args

getFuncPos (Just ((FnDecl _ _ pos), _)) = pos

getFuncArgsWithoutJust (FnDecl rettype args _) = args

getFuncRetTypeWithoutJust (FnDecl rettype args _) = rettype

findFuncDecl [] = do
    curEnv <- ask
    return curEnv

findFuncDecl ((FnDef pos rettype (Ident ident) args stmts) : rest) = do
    prevLoc <- asks (Map.lookup ident)
    case prevLoc of
        Just foundLoc -> throwError $ "Multiple function declaration" ++ (writePos pos)
        Nothing -> do
    
            funDecLoc <- alloc
            let funDeclData = (FnDecl rettype args pos)
            insertToStore (funDeclData, 0) funDecLoc

            local (Map.insert ident funDecLoc) (findFuncDecl rest)

findFuncDecl ((ClassDef pos (Ident className) cbody) : rest) = do --(CBlock posBlock stmts)) : rest) = do
    prevLoc <- asks (Map.lookup className)
    case prevLoc of
        Just founfLoc -> throwError $ "Multiple struct or not extended class declaration" ++ (writePos pos) -- checks if function and a class are named the same
        
        Nothing -> do

            insertNewClass className

            classDecLoc <- alloc
            -- classEnv <- saveOnlyAttrsMethods stmts className
            let classValue = ClassType className
            insertToStore (classValue, 0) classDecLoc

            -- evalClassBody stmts className

            local (Map.insert className classDecLoc) (findFuncDecl rest)

saveClassInnerData [] = return Success

saveClassInnerData ((FnDef pos rettype (Ident ident) args stmts) : rest) = saveClassInnerData rest

saveClassInnerData ((ClassDef pos (Ident className) (CBlock posBlock stmts)) : rest) = do --(CBlock posBlock stmts)) : rest) = do
    prevLoc <- asks (Map.lookup className)
    case prevLoc of
        Nothing -> throwError $ className ++ " class not saved in environtment" ++ (writePos pos) -- checks if function and a class are named the same
        
        Just foundLoc -> do
            --curEnv <- ask
            -- let insertedSelf = Map.insert 
            selfLoc <- alloc
            insertToStore ((ClassType className), 0) selfLoc

            classEnv <- local (Map.insert selfClassEntity selfLoc) (saveOnlyAttrsMethods stmts className) -- instead of (const curEnv)
            updateClassEnvInStore className classEnv

            saveClassInnerData rest

            
evalClassBody :: [ClassStmt] -> String -> InterpreterMonad Value
evalClassBody [] _ = return Success

evalClassBody ((ClassEmpty _) : rest) className = evalClassBody rest className

-- already evaluated
evalClassBody ((ClassDecl pos declType items) : rest) className = evalClassBody rest className
    -- do
    -- if (not (isClass declType)) || ((getClassName declType) == className)
    -- then do
    --     updatedEnv <- evalClassDecl declType items className

    --     local (const updatedEnv) (evalClassBody rest className)
    -- else do
    --     -- envWithFuncClassDecl <- gets basalEnv
    --     -- local (const envWithFuncClassDecl) (evalNestedClass declType)

    --     updEnv <- evalClassDecl declType items className

    --     local (const updEnv) (evalClassBody rest className)

evalClassBody ((ClassMethod pos retType (Ident ident) args (Blk _ stmts)) : rest) className = do --do -- eval in local, but save in global
    -- classData <- getClassMethodsAttrs className pos
    -- let foundMethod = Map.lookup ident classData

    -- case foundMethod of
    --     Just meth -> throwError $ "Multiple method or attribute declaration: " ++ ident ++ " in class: " ++ className ++ " at " ++ (writePos pos)
    --     Nothing -> do
    --         let methodData = (FnDecl retType args pos)
    --         let inserted = Map.insert ident methodData classData
    --         insertNewAttrMeth className inserted

            --envWithFuncClassDecl <- gets basalEnv -- environment with funcs (not needed) and class declarations
    -- env from dict
    printMes $ "classmeth " ++ ident ++ (show stmts)

    initCurFuncData ident retType pos

    curEnv <- ask

    envWithParams <- local (const curEnv) (checkArgs args)
    local (const envWithParams) (checkBody stmts 0 0 0)

    evalClassBody rest className
            -- local (const Map.empty) ()

evalClassDecl :: Type -> [ClassItem] -> String -> InterpreterMonad Env
evalClassDecl _ [] _ = do
    curEnv <- ask
    return curEnv
    --return Success

evalClassDecl declType ((CItem pos (Ident ident)) : rest) className = do
    classData <- getClassMethodsAttrs className pos -- dict with class attrs and methods

    let itemData = Map.lookup ident classData -- atrr/method or Nothing

    case itemData of
        Just foundData -> throwError $ "Multiple attribute declaration: " ++ ident ++ " at " ++ (writePos pos)

        Nothing -> do
            let inserted = Map.insert ident (getTypeOriginal declType) classData
            insertNewAttrMeth className inserted

            itemLoc <- alloc
            insertToStore ((getTypeOriginal declType), 0) itemLoc -- TODO check if depth is needed

            -- let itd = Map.lookup ident classData

            -- fd <- getClassMethodsAttrs className pos

            -- let itd2 = Map.lookup ident fd

            local (Map.insert ident itemLoc) (evalClassDecl declType rest className)


getClassMethodsAttrs className pos = do
    classData <- gets (Map.lookup className . classStruct)
    case classData of
        Nothing -> throwError $ "Class or struct undeclared: " ++ className ++ " " ++ (writePos pos)
        Just dataDict -> return dataDict

evalNestedClass (Class pos (Ident ident)) = do
    classLoc <- asks (Map.lookup ident)
    case classLoc of
        -- if the className exists
        Nothing -> throwError $ "Evaluation of the nested class " ++ ident ++ ": no such class " ++ (writePos pos) -- at least name should have been saved, but it is not
        Just loc -> do
            classData <- gets (Map.lookup loc . store)

            case classData of
                Nothing -> throwError $ "No data for the nested class " ++ ident ++ ": " ++ (writePos pos)
                Just (cdata, depth) -> do
                    if isClassUnprocessed cdata
                    then do
                        let classStmts = getClassStmtsFromClassCode cdata
                        evalClassBody classStmts ident
                    else
                        return Success

isClass (Class _ _) = True
isClass _ = False

getClassName (Class _ (Ident name)) = name

isClassUnprocessed (ClassCode _) = True
isClassUnprocessed (ClassType _) = False

getClassTypeName (ClassType name) = name

getClassCode (ClassCode ccode) = ccode

getClassStmts (CBlock pos stmts) = stmts

getClassBody (ClassDecl pos className classBody) = classBody

getClassStmtsFromClassCode (ClassCode ccode) = getClassStmts ccode

checkExprAttrOrMethod pos var methodAttrName exprs isMethod = do
    varClassType <- getExprType var
    let className = getClassTypeName $ fromJust varClassType
    classMethodsAttrs <- getClassMethodsAttrs className pos

    let method = Map.lookup methodAttrName classMethodsAttrs

    case method of
        Nothing -> throwError $ "Method or attribute" ++ methodAttrName ++ " not found for class " ++ className ++ " at " ++ (writePos pos)

        Just methodAttrData -> do
            if isMethod
            then do
                exprTypes <- mapM getExprType exprs

                let funcArgs = getFuncArgsWithoutJust methodAttrData
                let funcRet = getFuncRetTypeWithoutJust methodAttrData

                checkArgsCall methodAttrName pos funcRet funcArgs exprTypes
            else do
                return (Just methodAttrData)
-- later check only if class exists and eval class methods, now only save their names
saveOnlyAttrsMethods :: [ClassStmt] -> String -> InterpreterMonad Env
saveOnlyAttrsMethods [] _ = do
    curEnv <- ask
    return curEnv
    --return Success

saveOnlyAttrsMethods ((ClassEmpty _) : rest) className = saveOnlyAttrsMethods rest className

saveOnlyAttrsMethods ((ClassDecl pos declType items) : rest) className = do
    if isClass declType
    then do
        let declClassName = getClassName declType
        classLoc <- asks (Map.lookup declClassName)
        case classLoc of
            Nothing -> throwError $ "Attempt to declare instance of a non-existing class or struct " ++ declClassName ++ " at " ++ (writePos pos)
            Just loc -> do
                updatedEnv <- evalClassDecl declType items className

                local (const updatedEnv) (saveOnlyAttrsMethods rest className)
    else do
        updatedEnv <- evalClassDecl declType items className

        local (const updatedEnv) (saveOnlyAttrsMethods rest className)

-- methods available in any order?
saveOnlyAttrsMethods ((ClassMethod pos retType (Ident ident) args (Blk _ stmts)) : rest) className = do
    classData <- getClassMethodsAttrs className pos
    let foundMethod = Map.lookup ident classData

    case foundMethod of
        Just meth -> throwError $ "Multiple method or attribute declaration: " ++ ident ++ " in class: " ++ className ++ " at " ++ (writePos pos)
        Nothing -> do
            let methodData = (FnDecl retType args pos)
            let inserted = Map.insert ident methodData classData
            insertNewAttrMeth className inserted

            methodLoc <- alloc
            insertToStore (methodData, 0) methodLoc

            local (Map.insert ident methodLoc) (saveOnlyAttrsMethods rest className)
-- getClassData className 

isInt (Int a) = True
isInt _ = False

isIntType (Just IntT) = True
isIntType _ = False

isStrType (Just StringT) = True
isStrType _ = False

isBoolType (Just BoolT) = True
isBoolType _ = False

getPos (Just pos) = pos

getTypeOriginal :: Type -> Value
getTypeOriginal (Int _)  = IntT
getTypeOriginal (Bool _) = BoolT
getTypeOriginal (Str _)  = StringT
getTypeOriginal (Void _) = VoidT
getTypeOriginal (Fun _ t _) =  FunT (getTypeOriginal t)
getTypeOriginal (Class _ (Ident ident)) = ClassType ident

checkArgs [] = do
    curEnv <- ask
    return curEnv

checkArgs ((Ar pos argType (Ident argName)): args) = do
    locFound <- asks (Map.lookup argName)
    case locFound of
        Just _ -> throwError $ "Multiple entity declaration in arguments (row, col): " ++ show (getPos pos)
        Nothing -> do
            typeLoc <- alloc
            insertToStore ((getTypeOriginal argType), 0) typeLoc
            local (Map.insert argName typeLoc) (checkArgs args)

checkFunction [] = return Success -- (StringV "OK")

checkFunction ((FnDef pos rettype (Ident ident) args (Blk _ stmts)) : rest) = do
        -- it is enough to replace the current function once
        curState <- get
        put curState {curFunc = (CurFuncData ident False False rettype pos)}
        setIsClass False

        printMes $ ident ++ (show stmts)

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
                local (const curEnv) (checkBody stmts 0 0 0) >> checkFunction rest

        else
            do
            envWithParams <- checkArgs args

            local (const envWithParams) (checkBody stmts 0 0 0) >> checkFunction rest

checkFunction ((ClassDef pos (Ident className) (CBlock posBlock stmts)) : rest) = do --evalClassBody stmts className >> 
    -- classLoc <- asks (Map.lookup className)
    -- case classLoc of
    --     Nothing -> throwError $ "checkFunction: the name of the class " ++ className ++ " should have been saved in env, but it is not " ++ (writePos pos)
    --     Just loc -> do
    printMes $ className
    classDataEnv <- gets (Map.lookup className . classEnv)
    setIsClass True
    case classDataEnv of
        Nothing -> throwError $ "checkFunction: no env data for " ++ className ++ (writePos pos)
        Just cEnv -> do
            local (const cEnv) (evalClassBody stmts className)

            checkFunction rest

                    -- if isClassUnprocessed cdata
                    -- then do
                    --     curEnv <- ask
                    --     local (const curEnv) (evalClassBody stmts className)

                    --     checkFunction rest
                    -- else
                    --     checkFunction rest

checkIfStringsEqual :: String -> String -> Bool
checkIfStringsEqual [] [] = True
checkIfStringsEqual [] _ = False
checkIfStringsEqual _ [] = False
checkIfStringsEqual (s1 : str1) (s2 : str2)
  | s1 == s2 = checkIfStringsEqual str1 str2
  | otherwise = False


checkIfAnyNameFromList :: [String] -> String -> Bool
checkIfAnyNameFromList [] _ = False
checkIfAnyNameFromList (s : ss) name
  | checkIfStringsEqual s name = True
  | otherwise = checkIfAnyNameFromList ss name

funcNoArgs = ["error", "readInt", "readString"]

isSpecialFunc funcName = checkIfAnyNameFromList funcNoArgs funcName

specialRet name = do
    case name of
        "error" -> return (Just VoidT)
        "readInt" -> return (Just IntT)
        "readString" -> return (Just StringT)

getArgPos (Ar pos _ _) = pos
getArgType (Ar _ t _) = Just (getTypeOriginal t)

checkArgsCall _ _ rettype [] [] = return (wrapOrigTypeInJust rettype)
checkArgsCall ident pos _ [] _ = throwError $ "Too many arguments in " ++ ident ++ (writePos pos)
checkArgsCall ident pos _ _ [] = throwError $ "Too few arguments in " ++ ident ++ (writePos pos)

checkArgsCall ident pos rettype (orig: argsOrig) (passed : argsPassed) = do
    if not (matchTypesOrigEval (getArgType orig) passed)
    then
        throwError $ "Argument type mismatch in " ++ ident ++ (writePos pos)
    else
        checkArgsCall ident pos rettype argsOrig argsPassed


checkAndOr pos expr1 expr2 = do
    type1 <- getExprType expr1
    type2 <- getExprType expr2
    
    if not (isBoolType type1) || not (isBoolType type2)
    then
        throwError $ "Non-boolean value passed to a logical operand" ++ (writePos pos)
    else
        return (Just BoolT)

isTrueLit (ELitTrue _) = True
isTrueLit _ = False

isFalseLit (ELitFalse _) = True
isFalseLit _ = False

exprWithoutBDepth (Just (IntT, _)) = (Just IntT)
exprWithoutBDepth (Just (StringT, _)) = (Just StringT)
exprWithoutBDepth (Just (BoolT, _)) = (Just BoolT)
exprWithoutBDepth (Just (VoidT, _)) = (Just VoidT)
exprWithoutBDepth (Just ((FunT _), _)) = (Just FunRetType)
exprWithoutBDepth (Just ((FnDecl _ _ _), _)) = (Just FunRetType)
exprWithoutBDepth (Just (classT, _)) = (Just classT)

getBlockDepth (Just (IntT, d)) = d
getBlockDepth (Just (StringT, d)) = d
getBlockDepth (Just (BoolT, d)) = d
getBlockDepth (Just (VoidT, d)) = d
getBlockDepth (Just (FnDecl _ _ _, d)) = d

-- classes and structs

getExprType (EClass pos (Class posName (Ident className))) = return (Just (ClassType className))

getExprType (EMethod pos var@(EVar posV (Ident classInstanceName)) (Ident methodName) exprs) = checkExprAttrOrMethod pos var methodName exprs True --do
    -- checkIfVar exists
    -- check if class has method
    -- return method type

    -- varClassType <- getExprType var
    -- let className = getClassTypeName $ fromJust varClassType
    -- classMethodsAttrs <- getClassMethodsAttrs className pos

    -- let method = Map.lookup methodName classMethodsAttrs

    -- case method of
    --     Nothing -> throwError $ "Method " ++ methodName ++ " not found for class " ++ className ++ " at " ++ (writePos pos)

    --     Just methodData -> do
    --         exprTypes <- mapM getExprType exprs

    --         let funcArgs = getFuncArgsWithoutJust methodData
    --         let funcRet = getFuncRetTypeWithoutJust methodData

    --         checkArgsCall methodName pos funcRet funcArgs exprTypes

getExprType (ENull pos classType) = return (Just (getTypeOriginal classType))

getExprType (EAttr pos var (Ident attrName)) = checkExprAttrOrMethod pos var attrName [] False
--

getExprType (EVar pos (Ident name)) = do
    typeLoc <- asks (Map.lookup name)
    case typeLoc of
        Nothing -> throwError $ "Unknown variable " ++ show name ++ " (row, col): " ++ show (getPos pos)
        Just loc -> do
            val <- gets (Map.lookup loc . store)
            return $ exprWithoutBDepth val

getExprType (ELitInt pos intVal) = return (Just IntT) 
getExprType (ELitTrue pos) = return (Just BoolT)
getExprType (ELitFalse pos) = return (Just BoolT)
getExprType (EString _ _) = return (Just StringT)

getExprType (EApp pos (Ident "printInt") expr) = do
    case expr of
        [] -> throwError $ "printInt needs an argument (row, col): " ++ show (getPos pos)
        otherwise -> do
            if ((length expr) /= 1)
            then 
                throwError $ "printInt too many arguments (row, col): " ++ show (getPos pos)
            else
                do
                exprType <- getExprType $ head expr
                if (isIntType exprType)
                then
                    return (Just VoidT)
                else
                    throwError $ "printInt argument is not int (row, col): " ++ show (getPos pos)

getExprType (EApp pos (Ident "printString") expr) = do
    case expr of
        [] -> throwError $ "printString needs an argument " ++ (writePos pos)
        otherwise -> do
            if ((length expr) /= 1)
            then 
                throwError $ "printString too many arguments" ++ (writePos pos)
            else
                do
                exprType <- getExprType $ head expr
                if (isStrType exprType)
                then
                    return (Just VoidT)
                else
                    throwError $ "printString argument is not string " ++ (writePos pos)

getExprType (EApp pos (Ident ident) expr) = do
    if (isSpecialFunc ident)
    then do
        case expr of
            [] -> specialRet ident
            otherwise -> throwError $ ident ++ "() does not take any arguments" ++ (writePos pos)
    else do
        varloc <- asks (Map.lookup ident)
        case varloc of
            Nothing -> throwError $ "Function " ++ ident ++ " is undeclared" ++ (writePos pos)
            Just loc -> do
                -- check arguments
                funcData <- gets (Map.lookup loc . store)
                exprTypes <- mapM getExprType expr

                let funcArgs = getFuncArgs funcData
                let funcRet = getFuncRettype funcData

                checkArgsCall ident pos funcRet funcArgs exprTypes

getExprType (Neg pos expr) = do
    exprType <- getExprType expr
    if (isIntType exprType) 
    then
        return exprType
    else
        throwError $ "Negation applied to non-numerical value" ++ (writePos pos)

getExprType (Not pos expr) = do
    exprType <- getExprType expr
    if (isBoolType exprType) 
    then
        return exprType
    else
        throwError $ "Logical negation applied to non-boolean value" ++ (writePos pos)

getExprType (EMul pos expr1 mulOperand expr2) = do
    type1 <- getExprType expr1
    type2 <- getExprType expr2

    if not (isIntType type1) || not (isIntType type2)
    then
        throwError $ "Non-numerical values for arithmetic operation" ++ (writePos pos)
    else
        return (Just IntT)

getExprType (EAdd pos expr1 (Plus posP) expr2) = do
    type1 <- getExprType expr1
    type2 <- getExprType expr2

    if isStrType type1 && isStrType type2
    then
        return (Just StringT)
    else if isIntType type1 && isIntType type2
    then
        return (Just IntT)
    else
        throwError $ "Type mismatch for concatenation or addition" ++ (writePos pos)

getExprType (EAdd pos expr1 (Minus posM) expr2) = do
    type1 <- getExprType expr1
    type2 <- getExprType expr2

    if isIntType type1 && isIntType type2
    then
        return (Just IntT)
    else
        throwError $ "Non-numerical values for arithmetic operation" ++ (writePos pos)

getExprType (ERel pos expr1 operand expr2) = do
    type1 <- getExprType expr1
    type2 <- getExprType expr2

    if not (matchTypesOrigEval type1 type2)
    then
        throwError $ "Type mismatch in logical operation" ++ (writePos pos)
    else
        case operand of
            (EQU p) -> return (Just BoolT)
            (NE p) -> return (Just BoolT)
            otherwise -> do
                -- we know that types must match so it's sufficient to check one result
                if isIntType type1  
                then
                    return (Just BoolT)
                else
                    throwError $ "Value type not supported in logical comparison" ++ (writePos pos)

getExprType (EAnd pos expr1 expr2) = checkAndOr pos expr1 expr2
getExprType (EOr pos expr1 expr2) = checkAndOr pos expr1 expr2

writePos pos = " (row, col): " ++ show (getPos pos)

extractType (Just t) = t
extractType _ = throwError $ "Type not assigned"

wrapOrigTypeInJust vartype = Just (getTypeOriginal vartype)

matchTypesOrigEval (Just a) (Just b) = a == b
matchTypesOrigEval _ _ = False

checkBodyIncDec pos ident rest typeName depth ifdepth blockDepth = do
    varloc <- asks (Map.lookup ident)
    case varloc of
        Nothing -> throwError $ "Undefined variable " ++ ident ++ (writePos pos)
        Just loc -> do
            varType <- gets (Map.lookup loc . store)
            if (isIntType $ exprWithoutBDepth varType)
            then
                checkBody rest depth ifdepth blockDepth
            else
                throwError $ typeName ++ " require int type" ++ (writePos pos)

checkDecl _ [] _ = do
    curEnv <- ask
    return curEnv

checkDecl vartype ((NoInit posIn (Ident ident)) : rest) blockDepth = do
    foundVar <- asks (Map.lookup ident)
    case foundVar of
        Just loc  -> do
            valD <- gets (Map.lookup loc . store)
            if (getBlockDepth valD) == blockDepth
            then
                throwError $ "Multiple variable declaration (row, col): " ++ show (getPos posIn)
            else do
                decVarLoc <- alloc
                insertToStore ((getTypeOriginal vartype), blockDepth) decVarLoc
                local (Map.insert ident decVarLoc) (checkDecl vartype rest blockDepth)
        Nothing -> do
            decVarLoc <- alloc
            insertToStore ((getTypeOriginal vartype), blockDepth) decVarLoc
            local (Map.insert ident decVarLoc) (checkDecl vartype rest blockDepth)

checkDecl vartype ((Init posIn (Ident ident) expr) : rest) blockDepth = do
    foundVar <- asks (Map.lookup ident)
    case foundVar of
        Just loc -> do
            valD <- gets (Map.lookup loc . store)
            if (getBlockDepth valD) == blockDepth
            then
                throwError $ "Multiple variable declaration (row, col): " ++ show (getPos posIn)
            else do
                -- check if expression type is correct
                exprType <- getExprType expr

                if (matchTypesOrigEval (wrapOrigTypeInJust vartype) exprType)
                then do
                    decVarLoc <- alloc
                    insertToStore ((getTypeOriginal vartype), blockDepth) decVarLoc

                    local (Map.insert ident decVarLoc) (checkDecl vartype rest blockDepth)
                else
                    throwError $ "Type mismatch in declaration (row, col): " ++ show (getPos posIn)
        Nothing -> do
            decVarLoc <- alloc
            insertToStore ((getTypeOriginal vartype), blockDepth) decVarLoc
            -- check if expression type is correct
            exprType <- getExprType expr

            if (matchTypesOrigEval (wrapOrigTypeInJust vartype) exprType)
            then
                local (Map.insert ident decVarLoc) (checkDecl vartype rest blockDepth)
            else
                throwError $ "Type mismatch in declaration (row, col): " ++ show (getPos posIn)

-- classes and structs

checkBody ((AssClass pos classVar (Ident attrName) expr) : rest) depth ifdepth blockDepth = do
    classExprType <- checkExprAttrOrMethod pos classVar attrName [] False
    exprType <- getExprType expr

    if not (matchTypesOrigEval classExprType exprType)
    then
        throwError $ "Incompatible types for an attribute assignment: " ++ (writePos pos)
    else
        checkBody rest depth ifdepth blockDepth

--


checkBody [] depth ifdepth blockDepth = do
    if depth == 0 && ifdepth == 0 && blockDepth == 0
    then do
        curFunD <- gets curFunc

        let ident = getFuncNameFromCurFunc curFunD
        
        -- funLoc <- asks (Map.lookup ident)

        -- case funLoc of
        --     Nothing -> throwError $ "Function " ++ ident ++ " is undeclared (internal error)"
        --     Just loc -> do
        --         -- check arguments
        --         funcData <- gets (Map.lookup loc . store)

        if (matchTypesOrigEval (Just VoidT) (wrapOrigTypeInJust (getFuncRetTypeCurFunc curFunD)))--(getFuncRettype funcData)))
        then
            -- no need to check in void type
            return Success
        else do
            -- return types must have been matched before
            let freeRetCur = getFuncFreeRetCurFunc curFunD
            let ifElseRetCur = getFuncIfElseRetCurFunc curFunD



            if freeRetCur || ifElseRetCur
            then
                return Success
            else do
                throwError $ ident ++ " lacks return statement" ++ (writePos (getFuncPosCurFunc curFunD)) --(getFuncPos funcData))
    else
        return Success

checkBody ((Empty pos) : rest) depth ifdepth blockDepth = checkBody rest depth ifdepth blockDepth

checkBody ((Decl pos vartype items) : rest) depth ifdepth blockDepth = do
    updatedEnv <- checkDecl vartype items blockDepth
    printMes $ "declared " ++ (show rest)
    local (const updatedEnv) (checkBody rest depth ifdepth blockDepth)

checkBody ((BStmt pos (Blk posB stmts)) : rest) depth ifdepth blockDepth = do
    curEnv <- ask
    local (const curEnv) (checkBody stmts depth ifdepth (blockDepth + 1)) >> checkBody rest depth ifdepth blockDepth 

checkBody ((SExp pos expr) : rest) depth ifdepth blockDepth = getExprType expr >> checkBody rest depth ifdepth blockDepth

checkBody ((Ass pos (Ident ident) expr) : rest) depth ifdepth blockDepth = do
    varloc <- asks (Map.lookup ident)
    case varloc of
        Nothing -> throwError $ "Unknown variable " ++ ident ++ (writePos pos)
        Just loc -> do
            exprType <- getExprType expr
            varType <- gets (Map.lookup loc . store)
            if not (matchTypesOrigEval (exprWithoutBDepth varType) exprType)
            then
                throwError $ "Incompatible types for assignment: " ++ (writePos pos)
            else
                checkBody rest depth ifdepth blockDepth

checkBody ((Incr pos (Ident ident)) : rest) depth ifdepth blockDepth = checkBodyIncDec pos ident rest "Incrementation" depth ifdepth blockDepth

checkBody ((Decr pos (Ident ident)) : rest) depth ifdepth blockDepth = checkBodyIncDec pos ident rest "Decrementation" depth ifdepth blockDepth

checkBody ((While pos condExpr stmt) : rest) depth ifdepth blockDepth = do
    condType <- getExprType condExpr

    if not (isBoolType condType)
    then
        throwError $ "While loop needs boolean condition" ++ (writePos pos)
    else
        if (isTrueLit condExpr)
        then -- always in
            checkBody [stmt] depth ifdepth blockDepth >> checkBody rest depth ifdepth blockDepth
        else -- when literal is false - never stepped in; same case as possible stepping in (ifdepth + 1)
            checkBody [stmt] depth (ifdepth + 1) blockDepth >> checkBody rest depth ifdepth blockDepth
    
checkBody ((CondElse pos condExpr stm1 stm2): rest) depth ifdepth blockDepth = do
    res <- ifElseCheck pos condExpr stm1 stm2 depth ifdepth blockDepth
    -- the root of the tree
    if res && (depth == 0) && (ifdepth == 0)
    then do
        curFunUpd <- gets curFunc
        curState <- get

        let name = getFuncNameFromCurFunc curFunUpd
        let isFreeRetFound = getFuncFreeRetCurFunc curFunUpd
        let curFuncRetType = getFuncRetTypeCurFunc curFunUpd
        let curFuncPos = getFuncPosCurFunc curFunUpd

        put curState {curFunc = (CurFuncData name True isFreeRetFound curFuncRetType curFuncPos)}

        
        checkBody rest depth ifdepth blockDepth
    else
        checkBody rest depth ifdepth blockDepth

checkBody ((Cond pos expr stmt) : rest) depth ifdepth blockDepth = do
    exprType <- getExprType expr

    if not (isBoolType exprType)
    then
        throwError $ "Non-boolean value in if condition" ++ (writePos pos)
    else
        if isTrueLit expr
        then do
            --checkBody [stmt] depth ifdepth blockDepth >> (printMes $ "cond FIRST " ++ (show pos) ++ " rest " ++ (show rest)) >> checkBody rest depth ifdepth blockDepth
            checkBody (stmt : rest) depth ifdepth blockDepth -- TODO HERE CHANGED!!!
        else
            checkBody [stmt] depth (ifdepth + 1) blockDepth >> checkBody rest depth ifdepth blockDepth

checkBody ((VRet pos) : rest) depth ifdepth blockDepth = retVoidOrValUpd (Just VoidT) pos rest depth ifdepth blockDepth

checkBody ((Ret pos expr) : rest) depth ifdepth blockDepth = do
    exprType <- getExprType expr
    retVoidOrValUpd exprType pos rest depth ifdepth blockDepth


retVoidOrValUpd justType pos rest depth ifdepth blockDepth = do
    -- check functio tupe depth == 0
    curFunD <- gets curFunc
    -- let ident = getFuncNameFromCurFunc curFunD
    -- funLoc <- asks (Map.lookup ident)

    -- case funLoc of
    --     Nothing -> throwError $ "Function " ++ ident ++ " is undeclared (internal error)" ++ (writePos pos)
    --     Just loc -> do
    --         -- check arguments
    --         funcData <- gets (Map.lookup loc . store)

    if not (matchTypesOrigEval justType (wrapOrigTypeInJust (getFuncRetTypeCurFunc curFunD))) --(getFuncRettype funcData)))
    then 
        throwError $ "Mismatched return value" ++ (writePos pos)
    else if depth == 0 && ifdepth == 0
    then do
        curState <- get

        let name = getFuncNameFromCurFunc curFunD
        let ifElse = getFuncIfElseRetCurFunc curFunD
        let curFuncRetType = getFuncRetTypeCurFunc curFunD
        let curFuncPos = getFuncPosCurFunc curFunD

        put curState {curFunc = (CurFuncData name ifElse True curFuncRetType curFuncPos)}

        checkBody rest depth ifdepth blockDepth
    else
        checkBody rest depth ifdepth blockDepth

extractRettypeWrapJust (Just (FnDecl rettype args pos)) = Just rettype

wrapInJust sth = (Just sth)

getFuncNameFromCurFunc (CurFuncData name _ _ _ _) = name
getFuncIfElseRetCurFunc (CurFuncData _ ifElse _ _ _) = ifElse
getFuncFreeRetCurFunc (CurFuncData _ _ frRet _ _) = frRet
getFuncRetTypeCurFunc (CurFuncData _ _ _ retType _) = retType
getFuncPosCurFunc (CurFuncData _ _ _ _ pos) = pos

checkRet ((Ret pos expr) : _) = True
checkRet ((VRet pos) : _) = True
checkRet [(BStmt pos (Blk posB stmts))] = checkRet stmts
-- single if does not change anything
checkRet ((Cond pos (ELitTrue a) stmts) : rest) = checkRet [stmts] || checkRet rest
checkRet ((Cond pos expr stmts) : rest) = checkRet rest
-- condition might not be satisified in while in the beginning and we would never enter return
checkRet ((While pos (ELitTrue a) stms) : rest) = checkRet [stms] || checkRet rest
checkRet ((While pos expr stms) : rest) = checkRet rest
checkRet ((CondElse pos expr1 stm1 stm2) : rest) =
    case expr1 of
        (ELitTrue _) -> res1 || checkRet rest
        (ELitFalse _) -> res2 || checkRet rest
        otherwise -> res1 && res2 || checkRet rest
        where
            res1 = checkRet [stm1]
            res2 = checkRet [stm2]
checkRet [] = False
checkRet (_ : rest) = checkRet rest
        

ifElseCheck pos condExpr stm1 stm2 depth ifdepth blockDepth = do
    exprType <- getExprType condExpr

    if not (isBoolType exprType)
    then
        throwError $ "Non-boolean condition in if-else clause" ++ (writePos pos)
    else
        if (isTrueLit condExpr)
        then
            checkBody [stm1] depth ifdepth blockDepth >> checkBody [stm2] (depth + 1) ifdepth blockDepth >> return (checkRet [stm1])
        else if (isFalseLit condExpr) then
            checkBody [stm1] (depth + 1) ifdepth blockDepth >> checkBody [stm2] (depth + 1) ifdepth blockDepth >> return (checkRet [stm2]) -- TODO CHANGED
        else
            checkBody [stm1] (depth + 1) ifdepth blockDepth >> checkBody [stm2] (depth + 1) ifdepth blockDepth >> return (checkRet [stm1] && checkRet [stm2])