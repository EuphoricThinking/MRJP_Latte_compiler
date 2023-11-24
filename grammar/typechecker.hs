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
             | FnDecl Type [Arg] | IntT | StringT | BoolT | VoidT | FunT Value 
             deriving (Eq) --TypeV Type

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
    show BoolT = "BoolT"
    show IntT = "IntT"

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

getTypeFromMaybe (Just a) = return a

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

getFuncRettype (Just (FnDecl rettype args)) = rettype
--getFuncRettype Nothing = throwError $ "No declaration data for function (rettype)"

getFuncArgs (Just (FnDecl rettype args)) = args
--getFuncArgs Nothing = throwError $ "No declaration data for function (args)"

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
                local (const curEnv) (checkBody stmts) >> checkFunction rest
                -- checkBody stmts what are the consequences?

        else
            do
            envWithParams <- checkArgs args
            printSth envWithParams
            --return VoidV
            -- >> discards the result, however we need to pass the whole program without errors and the last step has to be accepter
            local (const envWithParams) (checkBody stmts) >> checkFunction rest

-- get Expr Type

-- modify store, in function: ret lists [retIf, ertElse, ret]
-- in if add field list of if, in else - else; nested if - add a list; remove after leaving if or else
    -- in the end of functo [] check return number and if void
        -- check return type na on the go
        -- stack
        --- check func arguments, check if function is declared in expressions
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
--getPassedPos (Expr' pos) = pos

-- rettype argsOrig argsPassed
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


-- get type of a variable (x, a, b)
getExprType (EVar pos (Ident name)) = do
    typeLoc <- asks (Map.lookup name)
    case typeLoc of
        Nothing -> throwError $ "Unknown variable " ++ show name ++ " (row, col): " ++ show (getPos pos)
        Just loc -> do
            val <- gets (Map.lookup loc . store)
            return val

getExprType (ELitInt pos intVal) = return (Just IntT) --return IntT
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

--getExprType (Neg pos expr) = checkNegNot pos expr "Negation"

--checkNegNot pos expr typeName = do
getExprType (Neg pos expr) = do
    exprType <- getExprType expr
    if (isIntType exprType) --(isBoolType exprType || isIntType exprType)
    then
        return exprType
    else
        throwError $ "Negation applied to non-numerical value" ++ (writePos pos)

-- getExprType (Not pos expr) = checkNegNot pos expr "Not"
getExprType (Not pos expr) = do
    exprType <- getExprType expr
    if (isBoolType exprType) --(isBoolType exprType || isIntType exprType)
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
                -- if ((isIntType type1) && (isIntType type2))
                -- we know that types must match so it's sufficient to check one result
                if isIntType type1 || isStrType type1
                then
                    return type1
                else
                    throwError $ "Value type not supported in logical comparison" ++ (writePos pos)

getExprType (EAnd pos expr1 expr2) = checkAndOr pos expr1 expr2
getExprType (EOr pos expr1 expr2) = checkAndOr pos expr1 expr2

-- getEpr of EApp - check if arguments are correct
    -- check if the function exists

-- compareTypes (Just a) (Just b) = a == b
-- compareTypes _ _ = False

writePos pos = " (row, col): " ++ show (getPos pos)

extractType (Just t) = t
extractType _ = throwError $ "Type not assigned"

wrapOrigTypeInJust vartype = Just (getTypeOriginal vartype)

matchTypesOrigEval (Just a) (Just b) = a == b
--matchTypesOrigEval v (Just b) = v == b
matchTypesOrigEval _ _ = False
-- matchExprType pos originalType evaluatedTypeToCompare = do
--     origEvaluated <- getExprType originalType

--     return (compareTypes origEvaluated evaluatedTypeToCompare)
    
    -- if (compareTypes origEvaluated evaluatedTypeToCompare)
    --     then return True
    -- else
    --     throwError $ "Type M"

checkBodyIncDec pos ident rest typeName = do
    varloc <- asks (Map.lookup ident)
    case varloc of
        Nothing -> throwError $ "Undefined variable " ++ ident ++ (writePos pos)
        Just loc -> do
            varType <- gets (Map.lookup loc . store)
            if (isIntType varType)
            then
                checkBody rest
            else
                throwError $ typeName ++ " require int type" ++ (writePos pos)

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
            printSth (getTypeOriginal vartype)
            -- check if expression type is correct
            exprType <- getExprType expr

            -- if (matchTypesOrigEval (getTypeOriginal vartype) exprType)
            if (matchTypesOrigEval (wrapOrigTypeInJust vartype) exprType)
            then
                local (Map.insert ident decVarLoc) (checkDecl vartype rest)
            else
                throwError $ "Type mismatch in declaration (row, col): " ++ show (getPos posIn)

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

checkBody ((BStmt pos (Blk posB stmts)) : rest) = checkBody stmts >> checkBody rest 

checkBody ((SExp pos expr) : rest) = printSth "pizda" >> getExprType expr >> checkBody rest

checkBody ((Ass pos (Ident ident) expr) : rest) = do
    varloc <- asks (Map.lookup ident)
    case varloc of
        Nothing -> throwError $ "Unknown variable " ++ ident ++ (writePos pos)
        Just loc -> do
            exprType <- getExprType expr
            --printSth exprType
            varType <- gets (Map.lookup loc . store)
            --printSth varType
            if not (matchTypesOrigEval varType exprType)
            then
                throwError $ "Incompatible types for assignment: " ++ (writePos pos)
            else
                checkBody rest

checkBody ((Incr pos (Ident ident)) : rest) = checkBodyIncDec pos ident rest "Incrementation"

checkBody ((Decr pos (Ident ident)) : rest) = checkBodyIncDec pos ident rest "Decrementation"
    

checkBody _ = printSth "there" >>  return VoidV


-- findFuncDecl ( _ : rest) = do
--     findFuncDecl rest
    