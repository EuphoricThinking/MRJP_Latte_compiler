{-# LANGUAGE FlexibleContexts #-}

import Typechecker hiding (main)
import Quad

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

import System.FilePath
import System.Process
import System.Directory

data AsmRegister = ARAX
    | AEAX
    | ARBP
    | ARBX
    | AR12
    | AR13
    | AR14
    | AR15
    | ARDI
    | ARSI
    | ARDX
    | ARCX
    | AR8
    | AR9
    | AEDI
    | AESI
    | AEDX
    | AECX
    | AR8D
    | AR9D
    | ARSP
    | AESP
    | AEBP
    | AR11D
    | AR11
    | ARIP

data Asm = AGlobl
    | SectText
    | ALabel String
    | ARet
    | ASpace
    | AFuncSpec String
    | AExtern
    | AProlog
    | AEpilog
    | AAllocLocals Int
    | AMov String String
    | AEpiRestMem
    | ANoExecStack
    | AJmp String
    | APush String
    | ACall String
    | ADealloc Int
    | SecStr String
    | SecData
    | StrLabel String String
    | ALea String String String
    | AAdd String String

-- push rbp := sub rsp, 8 \ mov [rsp], rbp
--
-- mov rbp, rsp // rsp to rbp - top of stack in rbp
-- rsp used for memory allocation, rbp for memory addressing
-- return address | old rbp ^^^rbp points here^^^

-- RSP ~ 0 mod 16 (before CALL, therefore 8 after CALL - pushed return address)

instance Show Asm where
    show AGlobl = "\tglobal main"
    show SectText = "section .text"
    show (ALabel s) = s ++ ":"
    show ARet = "\tret"
    show ASpace = "\n"
    show (AFuncSpec s) = s
    show AExtern = "\textern "
    show AProlog = "\tpush rbp\n\tmov rbp, rsp"
    show AEpilog = "\tpop rbp\n\tret" -- check recording 7.28
    show (AAllocLocals num)= "\tsub rsp, " ++ (show num)
    show (AMov s1 s2) = "\tmov " ++ s1 ++ ", " ++ s2
    show AEpiRestMem = "\tmov rsp, rbp"
    show ANoExecStack = "section .note.GNU-stack noalloc noexec nowrite progbits"
    show (AJmp s) = "\tjmp " ++ s
    show (APush s) = "\tpush " ++ s
    show (ACall s) = "\tcall " ++ s
    show (ADealloc v) = "\tadd rsp, " ++ (show v)
    show (SecStr s) = "\tdb " ++ (show s) ++ ", 0" -- in order to preserve ""
    show SecData = "section .data"
    show (StrLabel lbl valStr) = "\t" ++ lbl ++ ": db " ++ (show valStr) ++ ", 0"
    show (ALea r addr1 addr2) = "\tlea " ++ r ++ ", [" ++ addr1 ++ " + " ++ addr2 ++ "]"
    show (AAdd v1 v2) = "\tadd " ++ v1 ++ ", " ++ v2

instance Show AsmRegister where
    show ARAX = "rax"
    show AEAX = "eax"
    show ARBP = "rbp"
    show ARBX = "rbx"
    show AR12 = "r12"
    show AR13 = "r13"
    show AR14 = "r14"
    show AR15 = "r15"
    show ARDI = "rdi"
    show ARSI = "rsi"
    show ARDX = "rdx"
    show ARCX = "rcx"
    show AR8 = "r8"
    show AR9 = "r9"
    show AEDI = "edi"
    show AESI = "esi"
    show AEDX = "edx"
    show AECX = "ecx"
    show AR8D = "r8d"
    show AR9D = "r9d"
    show ARSP = "rsp"
    show AESP = "esp"
    show AEBP = "ebp"
    show AR11D = "r11d"
    show AR11 = "r11"
    show ARIP = "rip"

    -- ah:al in eax

data StoragePlace = OffsetRBP Int | Register AsmRegister | ProgLabel String

instance Show StoragePlace where
    show (OffsetRBP i) = show i
    show (Register reg) = show reg
    show (ProgLabel l) = l

type AsmCode = [Asm]

type AsmEnv = Map.Map String (QVar, StoragePlace)
type AsmMonad a = ReaderT AsmEnv (StateT AStore (ExceptT String (WriterT AsmCode IO))) a 

type StrProgLabel = String
type VarLabel = String

data AStore = AStore {
    -- storeA :: Map.Map Loc
    curFuncNameAsm :: String,
    funcInfo :: Map.Map String FuncData,
    lastAddrRBP :: Int,
    specialFuncExt :: [String],
    curRSP :: Int,
    strLabelsCounter :: Int,
    labelsCounter :: Int
    -- strLabels :: Map.Map VarLabel StrProgLabel
} deriving (Show)

parametersRegistersInts32 = [AEDI, AESI, AEDX, AECX, AR8D, AR9D]
parametersRegisterPoniters64 = [ARDI, ARSI, ARDX, ARCX, AR8, AR9]
calleeSaved = [ARBP, ARBX, AR12, AR13, AR14, AR15] 

intBytes = 4
strPointerBytes = 8

paramsStartOff = 16
stackParamSize = 8

numRegisterParams = 6

endSuffix = "_END"
functionLabel = ".L"
stringLabel = "LS"

labelRegister = ARIP

stackAlignment = 16
pushWord = 8

extractQStore (Right (_, qstore)) = qstore
extractAsmCode (Right (_, acode)) = acode

-- prepareAsmStore :: Either String Store -> AStore
prepareAsmStore qdata = AStore {curFuncNameAsm = "",
funcInfo = (defFunc qdata), lastAddrRBP = 0, specialFuncExt = (specialFunc qdata), curRSP = 0, strLabelsCounter = 0, labelsCounter = 0} -- after call mod = 8 (ret addr + 8 bytes), after push rbp (+8 bytes) -> mod = 8 

main :: IO () 
main = do
    args <- getArgs
    case args of
        -- [] -> hGetContents stdin >>= parseFileExec
        [] -> printError "Filename needed" >> exitFailure    
        [filename] -> do --readFile filename >>= parseFileExec
            fileContent <- readFile filename
            let tokens = myLexer fileContent
            let parsed = pProgram tokens

            case parsed of
                Left mes -> printError mes
                Right p -> do
                    resWrapped <- runExceptT $ evalStateT (runReaderT (executeRightProgram p) Map.empty) (Store {store = Map.empty, lastLoc = 0, curFunc = (CurFuncData "" False False)})
                    case resWrapped of
                        Left msg -> printError msg >> exitFailure
                        Right _ -> do
                            printOK
                            (eitherQuad, quadcode) <- genQuadcode p
                            print $ (show eitherQuad)
                            print $ (show quadcode)
                            let ftuple = splitExtension filename
                            let fname = fst ftuple
                            let finalName = fname ++ ".s"
                            print $ finalName
                            (eithAsm, asmcode) <- genAssembly (extractQStore eitherQuad) quadcode --(prepareAsmStore eitherQuad) quadcode
                            writeToFile filename (unlines $ map show asmcode)
                            exitSuccess
                            --printOK >> getQuadcode p >>= writeToFile filename >> exitSuccess

writeToFile path program =
    let
        ftuple = splitExtension path
        finalName = fst ftuple
        finalNameAsm = finalName ++ ".s"
        finalNameObj = finalName ++ ".o"
    in
        do
        writeFile finalNameAsm program
        callProcess "nasm" [finalNameAsm, "-o", finalNameObj, "-f elf64"]
        callProcess "gcc" [finalNameObj, "-o", finalName, "-no-pie", "lib/runtime.o"]
        removeFile finalNameObj
    

checkErrorOrExecute :: ExceptT String IO Value -> Program -> IO()
checkErrorOrExecute resWrapped program = do
    res <- runExceptT $ resWrapped
    case res of
        Left mes -> printError mes >> exitFailure
        Right _ -> printOK >> print program >> exitSuccess

parseFileExec :: String -> IO () 
parseFileExec fileContent =
    let
      tokens = myLexer fileContent
      parsed = pProgram tokens
    in typeCheckExecute parsed

typeCheckExecute :: Either String Program -> IO ()
typeCheckExecute program =
    case program of
        Left mes -> printError mes >> exitFailure
        Right p -> checkErrorOrExecute (evalStateT (runReaderT (executeRightProgram p) Map.empty) (Store {store = Map.empty, lastLoc = 0, curFunc = (CurFuncData "" False False)})) p 

-- genAssembly :: AStore -> QuadCode -> AsmMonad (Either String AsmCode, AsmCode)
genAssembly quadstore quadcode = 
    let
        astore = prepareAsmStore quadstore
    in
        runWriterT $ runExceptT $ evalStateT (runReaderT (runGenAsm quadcode) Map.empty) astore --quadstore

addExternals :: [String] -> AsmMonad ()
addExternals [] = tell $ [ASpace]
addExternals s = do
        --tell $ []
        --addExternals ss
        tell $ [AFuncSpec (getSpecialWrapped s)]
        -- tell $ [getSpecialWrapped s]
        tell $ [ASpace]

getSpecialWrapped s = (show AExtern) ++ (go s) where
    go (s : []) = s
    go (s : ss) = s ++ ", " ++ (go ss)

updateRSP valueToAdd = do
    curState <- get
    put curState {curRSP = ((curRSP curState) + valueToAdd)}

checkRSPmod :: AsmMonad Int
checkRSPmod = do
    curOffRSP <- gets curRSP
    let absRSP = abs curOffRSP
    return (absRSP `mod` stackAlignment)

getValToRoundUpRSP = do
    curModulo <- checkRSPmod
    return (stackAlignment - curModulo)



checkHowToUpdateRSP candidateVal =
    let
        curMod = (abs candidateVal) `mod` stackAlignment
        toUpd = stackAlignment - curMod
    in
        helperCheckVal candidateVal toUpd

helperCheckVal candidateVal toUpd
    | toUpd == stackAlignment = candidateVal
    | otherwise = (abs candidateVal) + toUpd

alignStack :: AsmMonad Int
alignStack = do
    toRoundUp <- getValToRoundUpRSP
    if toRoundUp == stackAlignment
    then
        return 0
    else do
        updateRSP toRoundUp
        tell $ [AAllocLocals toRoundUp]
        return toRoundUp

dealloc spaceToDealloc = do
    if spaceToDealloc /= 0
    then do
        updateRSP (-spaceToDealloc)
        tell $ [ADealloc spaceToDealloc]
    else
        return ()


sumParamsSizes [] sumParams = sumParams
sumParamsSizes ((ArgData ident valType) : args) sumParams = sumParamsSizes args (sumParams + stackParamSize)
    -- case valType of
    --     IntQ -> sumParamsSizes args (sumParams + intBytes)

sumParamsSizesPastRegisters [] regs = 0
sumParamsSizesPastRegisters args 0 = sumParamsSizes args 0
sumParamsSizesPastRegisters (a : args) numRegs = sumParamsSizesPastRegisters args (numRegs - 1)

allParamsTypeSizes [] sumParams = sumParams
allParamsTypeSizes (a@(ArgData ident valType) : args) sumParams = 
    case valType of
        IntQ -> allParamsTypeSizes args (sumParams + intBytes)
        StringQ -> allParamsTypeSizes args (sumParams + strPointerBytes)

subLocals 0 _ = printMesA "here" >> return ()
-- TODO fix it -> all params are saved in memory
subLocals numLoc (FuncData name retType args locNum body numInts strVars strVarsNum) = do 
    st <- get
    printMesA $ "should not BE "  ++ (show numLoc) ++ " " ++ (curFuncNameAsm st)
    -- let localsSize = numInts*intBytes + strVarsNum*strPointerBytes--TODO add rest
    -- -- let stackParamsSize = sumParamsSizesPastRegisters args numRegisterParams
    -- -- let sumLocalsAndParamsSizes = localsSize + stackParamsSize -- parameters are saved in memory
    -- let paramsSizes = allParamsTypeSizes args 0
    -- let sumLocalsAndParamsSizes = paramsSizes + localsSize
    let sumLocalsAndParamsSizes = numInts*intBytes + strVarsNum*strPointerBytes

    printMesA $ "PARAMS " ++ (show args)

    printMesA $ "sum locals params: " ++ (show sumLocalsAndParamsSizes)
    -- printMesA $ "sum params: " ++ (show paramsSizes)
    -- printMesA $ "sum locals: " ++ (show localsSize)
    printMesA $ "numStrs: " ++ (show strVarsNum)
    printMesA $ "numInts: " ++ (show numInts)

    let stackUpdate = checkHowToUpdateRSP sumLocalsAndParamsSizes
    updateRSP stackUpdate


    -- tell $ [AAllocLocals localsSize] --[AAllocLocals numLoc]

    -- tell $ [AAllocLocals sumLocalsAndParamsSizes]

    tell $ [AAllocLocals stackUpdate]

updateCurFuncNameAsm name = do
    curState <- get
    put curState {curFuncNameAsm = name}

isIntQ IntQ = True
isIntQ _ = False

createRelAddrRBP offset = "[rbp" ++ (show offset) ++ "]"

createAddrIntRBP memStorage = 
    case memStorage of
        OffsetRBP offset -> "dword " ++ (createRelAddrRBP offset) -- was before
        Register reg -> show reg

createAddrPtrRBP memStorage =
    case memStorage of
        OffsetRBP offset -> "qword " ++ (createRelAddrRBP offset)
        Register reg -> show reg
        ProgLabel l -> l  -- ++ [rip] to check

getValToMov (IntQVal val) = val

printMesA mes = lift $ lift $ lift $ lift $ print mes

getNumArgs (FuncData _ _ args _ _ _ _ _) = length args

createEndRetLabel = do
    curFName <- gets curFuncNameAsm
    return ("." ++ curFName ++ endSuffix)

allocInt v = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - intBytes
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    let storageOffset = OffsetRBP newRBPOffset
    -- gen command
    -- if register, mem location, constant
    tell $ [AMov (createAddrIntRBP storageOffset) (show v)]

    return storageOffset
    -- tell $ [AMov (createAddrIntRBP newRBPOffset) (show v)]

    -- return newRBPOffset

-- allocPtr v = do
--     curRBP <- gets lastAddrRBP
--     let newRBPOffset = curRBP - strPointerBytes

--     curState <- get
--     put curState {lastAddrRBP = newRBPOffset}
allocVar v memSize = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - memSize
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    let storageOffset = OffsetRBP newRBPOffset
    -- gen command
    -- if register, mem location, constant
    if memSize == intBytes
    then
        tell $ [AMov (createAddrIntRBP storageOffset) (show v)]
    else -- TODO extend for extensions  at this moment other is ptr
        tell $ [AMov (createAddrPtrRBP storageOffset) (show v)]

    return storageOffset

getNewOffsetUpdRBP memSize = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - memSize
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    return (OffsetRBP newRBPOffset)




--moveParamsToLocal fname fbody = do
    -- move over the lists, up to zero

-- statt
moveFromRegisters args [] [] = moveStackParams args paramsStartOff

moveFromRegisters [] _ _ = do
    curEnv <- ask
    return curEnv

moveFromRegisters ((ArgData ident valType) : args) (reg : regs) (ereg : eregs) = do
    let var = (QLoc ident valType)

    case valType of
        IntQ -> do
            offsetRBP <- allocVar ereg intBytes -- allocInt ereg

            local (Map.insert ident (var, offsetRBP)) (moveFromRegisters args regs eregs)

        StringQ -> do
            offsetRBP <- allocVar reg strPointerBytes

            local (Map.insert ident (var, offsetRBP)) (moveFromRegisters args regs eregs)

--moveParamsToLocal 
moveStackParams [] _ = do
    curEnv <- ask
    return curEnv

moveStackParams ((ArgData ident valType): args) stackOffset = do
    case valType of
        IntQ -> do
            tell $ [AMov (show ARAX) (createAddrIntRBP (OffsetRBP stackOffset))] 
            let var = QLoc ident valType
            offsetRBP <- allocVar ARAX intBytes -- allocInt ARAX

            local (Map.insert ident (var, offsetRBP)) (moveStackParams args (stackOffset + stackParamSize))

getQCallCode qcall@((QCall qvar ident numArgs) : rest) = qcall
getQCallCode ((QParam val) : rest) = getQCallCode rest

paramsToStack qcall@((QCall qvar ident numArgs) : rest) accum = accum
paramsToStack (qparam@(QParam val) : rest) accum = (qparam : accum)

pushParams [] = return ()
pushParams ((QParam val) : rest) = do
    case val of
        (IntQVal v) -> do
            tell $ [APush (show v)]

            updateRSP pushWord

        (LocQVal ident valType) -> do
            varData <- asks (Map.lookup ident)
            case varData of
                Nothing -> throwError $ "No env data for " ++ ident
                Just (var, offset) -> do
                    case valType of
                        (IntQ) -> do
                            tell $ [AMov (show AR11D) (createAddrIntRBP offset)]
                            tell $ [APush (show AR11)]

                            updateRSP pushWord

    pushParams rest

genParams qcall@((QCall qvar ident numArgs) : rest) _ _ = genStmtsAsm qcall
genParams [] _ _ = genStmtsAsm []
genParams qcode [] _ = do
    let qcallcode = getQCallCode qcode
    let reverseParams = paramsToStack qcode []
    pushParams reverseParams

    genStmtsAsm qcallcode


genParams (qp@(QParam val) : rest) (reg : regs) (ereg : eregs)= do
    printMesA qp
    case val of
        (IntQVal v) -> do
            tell $ [AMov (show ereg) (show v)]
            -- genParams rest regs eregs

        (LocQVal ident valType) -> do
            varData <- asks (Map.lookup ident)
            case varData of
                Nothing -> throwError $ "No env data for " ++ ident
                Just (var, offset) -> do
                    case valType of
                        (IntQ) -> do
                            tell $ [AMov (show ereg) (createAddrIntRBP offset)]

                        StringQ -> do
                            tell $ [AMov (show reg) (createAddrPtrRBP offset)]

        (StrQVal s) -> do
            findLbl <- asks (Map.lookup s)
            case findLbl of
                Nothing -> throwError $ "string literal not found in data section (or env error): " ++ s
                Just (_, lbl) -> do
                    tell $ [AMov (show reg) (show lbl)]
                    --gen

    genParams rest regs eregs

assignResToRegister var@(QLoc varTmpId varType) =
    -- case varType of
    --     IntQ -> (var, (Register AEAX))
    --     StringQ -> (var, (Register ARAX))
    case is32bit varType of
        True -> (var, (Register AEAX))
        False -> (var, (Register ARAX))

increaseStrLblCounterByOne curStrLblCnt = do
    curState <- get
    put curState {strLabelsCounter = (curStrLblCnt + 1)}

createStrLiteralLabels :: [String] -> AsmMonad AsmEnv
createStrLiteralLabels [] = do
    curEnv <- ask
    return curEnv

createStrLiteralLabels (s : ss) = do
    curStrLblCnt <- gets strLabelsCounter
    let newStrLbl = stringLabel ++ (show curStrLblCnt)

    increaseStrLblCounterByOne curStrLblCnt

    tell $ [ALabel newStrLbl]
    tell $ [SecStr s]
    tell $ [ASpace]

    curEnv <- ask

    local (Map.insert s (NoMeaning, (ProgLabel newStrLbl))) (createStrLiteralLabels ss)

saveStrLiteralsInDataSec [] = do
    curEnv <- ask
    return curEnv

saveStrLiteralsInDataSec (s: ss) = do
    curStrLblCnt <- gets strLabelsCounter
    let newStrLbl = stringLabel ++ (show curStrLblCnt)

    increaseStrLblCounterByOne curStrLblCnt

    tell $ [StrLabel newStrLbl s]

    curEnv <- ask

    local (Map.insert s (NoMeaning, (ProgLabel newStrLbl))) (saveStrLiteralsInDataSec ss)

iterOverAllFuncs [] = do
    env <- ask
    return env

iterOverAllFuncs ((QFunc finfo@(FuncData name retType args locNum body numInts strVars strVarsNum)) : rest) = do
    let args = getFuncStringList finfo

    env <- saveStrLiteralsInDataSec args

    local (const env) (iterOverAllFuncs rest)

prepareDataSect funcs = do
    tell $ [SecData]

    env <- ask

    updEnv <- local (const env) (iterOverAllFuncs funcs)

    tell $ [ASpace]

    return updEnv

clearCurFuncParams = do
    curState <- get
    put curState {lastAddrRBP = 0, curRSP = 0}

isOffset offset =
    case offset of
        (OffsetRBP _) -> True
        _ -> False

is32bit valType = 
    case valType of
        IntQ -> True
        _ -> False

isIntLiteral (IntQVal _) = True
isIntLiteral _ = False

extractIntVal (IntQVal v) = v

extractLocQvarId (LocQVal id _) = id

findAddr (LocQVal ident _) = do
    idData <- asks (Map.lookup ident)
    case idData of
        Nothing -> throwError $ ident ++ " var not found for address determination"
        Just (_, memStorage) -> return memStorage


-- getAddrOrLiteral val =
--     case val of
--         (IntQVal v) -> show val
--         (LocQVal _ _) -> do
--             valAddr <-
-- TODO extension if others on stack, a simplified version
--numParamsStack numArgs = numArgs - numRegisterParams
clearStackParamsAndAlignment numArgs toDealloc = do
    let leftOnStack = numArgs - numRegisterParams
    if leftOnStack > 0
    then do
        let alignedAndParams = stackParamSize*leftOnStack + toDealloc
        updateRSP (-alignedAndParams)

        tell $ [ADealloc alignedAndParams]
    else
        dealloc toDealloc

createMemAddr memStorage isLoc32bit =
    case memStorage of
        OffsetRBP offset -> do
            case isLoc32bit of
                True -> "dword " ++ (createRelAddrRBP offset)
                False -> "qword " ++ (createRelAddrRBP offset)

        Register reg -> show reg
        ProgLabel l -> l

createMemAddrRBPdword_qword (OffsetRBP offset) isLoc32bit =
    case isLoc32bit of
        True -> "dword " ++ (createRelAddrRBP offset)
        False -> "qword " ++ (createRelAddrRBP offset)

moveTempToR11 memStorageAddr isLoc32bit = 
    case isLoc32bit of
        True -> do
            tell $ [AMov (show AR11D) memStorageAddr]
            return AR11D
        False -> do
            tell $ [AMov (show AR11) memStorageAddr]
            return AR11

movMemoryVals memToL memFromR valType = do
    let isLoc32bit = is32bit valType
    let rightAddr = createMemAddr memFromR isLoc32bit
    let leftAddr = createMemAddr memToL isLoc32bit

    if ((isOffset memFromR) && (isOffset memToL))
    then do
        r11_sized <- moveTempToR11 rightAddr isLoc32bit
        tell $ [AMov leftAddr (show r11_sized)]
    else do
        tell $ [AMov leftAddr rightAddr]

getQVarType (QLoc _ valType) = valType
getQVarType (QArg _ valType) = valType

moveStringToMem memToL fullStr = do
    fndLbl <- asks (Map.lookup fullStr)
    case fndLbl of
        Nothing -> throwError $ "label for string " ++ fullStr ++ " not found"
        Just (_, lbl) -> do
            movMemoryVals memToL lbl StringQ

getTupleLeftIntLiteral val1 val2 = 
    -- when it is known that at least one is intLiteral
    case val1 of
        (IntQVal _) -> (val1, val2)
        _ -> (val2, val1)

-- findMemAddr val = do

runGenAsm :: QuadCode -> AsmMonad Value
runGenAsm q = do--return BoolT
    tell $ [ANoExecStack]

    asmEnv <- prepareDataSect q

    tell $ [SectText]
    tell $ [AGlobl] 

    curState <- get
    addExternals (specialFuncExt curState)

    -- asmEnv <- ask
    local (const asmEnv) (genFuncsAsm q)
    -- genFuncsAsm q

    -- asmEnv <- ask
    -- printMesA "encLoc print"
    -- printMesA asmEnv
    -- case (specialFunc curState) of
    --     [] -> 
    return BoolT


genFuncsAsm :: QuadCode -> AsmMonad ()
genFuncsAsm [] = return ()

genFuncsAsm ((QFunc finfo@(FuncData name retType args locNum body numInts strVars strVarsNum)) : rest) = do
    -- env <- ask
    -- strEnv <- local (const env) (createStrLiteralLabels strVars)

    tell $ [ALabel name]
    tell $ [AProlog]

    printMesA $ "NAME IN |" ++ name ++ "|"
    printMesA "boooodyyy"
    printMesA (show body)

    -- get size of params, subtract from the stack (probably iterate once again)
    -- clear store before function leave

    subLocals locNum finfo
    updateCurFuncNameAsm name

    env <- ask
    curEnv <- local (const env) (moveFromRegisters args parametersRegisterPoniters64 parametersRegistersInts32)

    local (const curEnv) (genStmtsAsm body)

    clearCurFuncParams

    st <- get
    printMesA "curs"
    printMesA st

    -- genStmtsAsm body
    genFuncsAsm rest

    -- what about recursive functions?

-- ret needs to know the value, to move a good one to eax
genStmtsAsm :: QuadCode -> AsmMonad ()
genStmtsAsm ((QRet res) : rest) = do
    printMesA $ "RETCOMP " ++ (show res)
    case res of
        (IntQVal numVal) -> do
            tell $ [AMov (show AEAX) (show numVal)] -- zostaw, później skoncz do ret

        (LocQVal ident valType) -> do
            fndId <- asks (Map.lookup ident)
            case fndId of
                Nothing -> throwError $ "ret: " ++ ident ++ " var not found"
                Just (qvar, memStorageVar) -> do
                    if is32bit valType
                    then
                        movMemoryVals (Register AEAX) memStorageVar valType
                    else
                        movMemoryVals (Register ARAX) memStorageVar valType

        (StrQVal s) -> do
            moveStringToMem (Register ARAX) s

    endLabel <- createEndRetLabel
    tell $ [AJmp endLabel]
    tell $ [ASpace]

    genStmtsAsm rest

genStmtsAsm (QVRet : rest) = do
    endLabel <- createEndRetLabel
    tell $ [AJmp endLabel]
    tell $ [ASpace]

    genStmtsAsm rest

genStmtsAsm [] = do
    endLabel <- createEndRetLabel
    tell $ [ALabel endLabel]
-- was in QRet val
    funcName <- gets curFuncNameAsm
    curBody <- gets (Map.lookup funcName . funcInfo)

    case curBody of
        Nothing -> throwError $ funcName ++ " no such func in asm store"
        Just bodyFunc -> do
            if (getFuncNumLoc bodyFunc) > 0
            then
                do
                tell $ [AEpiRestMem]
                tell $ [AEpilog]
            else
                tell $ [AEpilog]

            tell $ [ASpace]

            -- genStmtsAsm rest

--genStmtsAsm [] = return () -- NIE, zamień miejscmi, w QRET zrób GOTO ret, tutaj dodaj epilog itp.
genStmtsAsm ((QAss var@(QLoc name declType) val) : rest) = do
    id <- asks (Map.lookup name)

    case id of
        Nothing -> throwError $ "Assignment to undeclared var in asm: " ++ name
        Just (var, memStorageL) -> do
            case val of
                (IntQVal v) -> do
                    tell $ [AMov (createAddrIntRBP memStorageL) (show v)]

                    -- memory storage does not change

                (StrQVal s) -> do
                    fndLbl <- asks (Map.lookup s)
                    case fndLbl of
                        Nothing -> throwError $ "No found label for " ++ s
                        Just (_, lbl) ->
                            tell $ [AMov (createAddrPtrRBP memStorageL) (show lbl)]

                (LocQVal ident valType) -> do
                    valStorage <- asks (Map.lookup ident)
                    case valStorage of
                        Nothing -> throwError $ ident ++ " var to be assigned not in env"
                        Just (varFromAssigned, storageR) -> do
                            -- all variables are stored in the memory at this moment, but this is for further extensions
                            -- TODO mov to another procedure, add more options
                            if isOffset storageR
                            then do
                                if is32bit valType
                                then do
                                    tell $ [AMov (show AR11D) (createAddrIntRBP storageR)]
                                    tell $ [AMov (createAddrIntRBP memStorageL) (show AR11D)]
                                else do
                                    tell $ [AMov (show AR11) (createAddrPtrRBP storageR)]
                                    tell $ [AMov (createAddrPtrRBP memStorageL) (show AR11)]
                            
                            else do
                                if is32bit valType
                                then
                                    tell $ [AMov (createAddrIntRBP memStorageL) (show storageR)]
                                else
                                    tell $ [AMov (createAddrPtrRBP memStorageL) (show storageR)]
                            

            genStmtsAsm rest
-- CHANGED
genStmtsAsm ((QDecl var@(QLoc name declType) val) : rest) = do

    curRBP <- gets lastAddrRBP

    case val of
        (IntQVal v) -> do
            -- let newRBPOffset = curRBP - intBytes
            -- curState <- get
            -- put curState {lastAddrRBP = newRBPOffset}
            -- -- gen command
            -- -- if register, mem location, constant
            -- tell $ [AMov (createAddrIntRBP newRBPOffset) (show v)]

            -- curEnv <- ask

            -- printMesA $ "envLoc " ++ name
            -- printMesA curEnv
            newRBPOffset <- allocVar v intBytes -- allocInt v
            local (Map.insert name (var, newRBPOffset)) (genStmtsAsm rest)

        (LocQVal ident valType) -> do
            valStorage <- asks (Map.lookup ident)
            case valStorage of
                Nothing -> throwError $ ident ++ " var not in env"
                Just (varFromAssigned, storage) -> do
                    case valType of
                        IntQ -> do
                            newRBPOffset <- allocVar storage intBytes -- allocInt storage
                            local (Map.insert name (varFromAssigned, newRBPOffset)) (genStmtsAsm rest)

                        StringQ -> do
                            newRBPOffset <- allocVar storage strPointerBytes
                            local (Map.insert name (varFromAssigned, newRBPOffset)) (genStmtsAsm rest)

        (StrQVal s) -> do
            strData <- asks (Map.lookup s)
            case strData of
                Nothing -> throwError $ "No string label for literal value: " ++ s
                Just (_, strLabel) -> do
                    newRBPOffset <- allocVar strLabel strPointerBytes
                    local (Map.insert name (var, newRBPOffset)) (genStmtsAsm rest)


genStmtsAsm params@((QParam val) : rest) = genParams params parametersRegisterPoniters64 parametersRegistersInts32

-- genStmtsAsm ((QCall qvar ident numArgs) : rest) = do

-- genStmtsAsm ((QCall qvar "printInt" numArgs) : rest) = do
--     -- tell $ [AMov (show AEAX) "0"]
--     tell $ [ACall "printInt"]

--     genStmtsAsm rest




-- ALIGN STACK
-- genStmtsAsm ((QCall qvar ident numArgs) : rest) = do
--     -- let isSpecial = isSpecialFuncQ ident

--     -- if isSpecial
--     -- then

--     -- else
--     --     return ()
--     case ident of
--         "printInt" -> do return ()

genStmtsAsm ((QCall qvar@(QLoc varTmpId varType) ident numArgs) : rest) = do
    -- after params generation
    valSubtracted <- alignStack

    case ident of
        "printInt" -> do
            tell $ [ACall "printInt"]
            dealloc valSubtracted

            genStmtsAsm rest

        "readInt" -> do
            tell $ [ACall "readInt"]
            dealloc valSubtracted

            let valStorage = assignResToRegister qvar

            local (Map.insert varTmpId valStorage) (genStmtsAsm rest)

        "printString" -> do
            tell $ [ACall "printString"]
            dealloc valSubtracted

            genStmtsAsm rest

        "readString" -> do
            tell $ [ACall "readString"]
            dealloc valSubtracted

            let valStorage = assignResToRegister qvar

            local (Map.insert varTmpId valStorage) (genStmtsAsm rest)

        "error" -> do
            tell $ [ACall "error"]
            dealloc valSubtracted

            genStmtsAsm rest


        _ -> do
            tell $ [ACall ident]
            -- dealloc valSubtracted -- result in eax, at this moment - without biger args
            -- remove params from stack and alignment
            clearStackParamsAndAlignment numArgs valSubtracted

            case varType of
                VoidQ -> genStmtsAsm rest
                _ -> do
                    let valStorage = assignResToRegister qvar

                    local (Map.insert varTmpId valStorage) (genStmtsAsm rest)

    --genStmtsAsm rest

genStmtsAsm ((QConcat qvar val1 val2) : rest) =
    let 
        newCode = (QParam val1) : (QParam val2) : (QCall qvar concatStr 2) : rest
    in
        genStmtsAsm newCode

genStmtsAsm ((QAdd qvar@(QLoc ident valType) val1 val2) : rest) = do
    -- printMesA $ "1) " ++ (show val1)
    -- printMesA $ "2) " ++ (show val2)
    -- genStmtsAsm rest
    -- qvar = val1 + val2
    -- copy val1 to qvar - without register usage, as in lea
    if (isIntLiteral val1) && (isIntLiteral val2)
    then do
        newValOffset <- allocInt $ extractIntVal val1
        tell $ [AAdd (createAddrIntRBP newValOffset) (show $ extractIntVal val2)]

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else if (isIntLiteral val1) || (isIntLiteral val2)
    then do
        -- printMesA $ "MIX " ++ val1 ++ val2
        let (valInt, valVar) = getTupleLeftIntLiteral val1 val2
        -- addition commutativity
        newValOffset <- allocInt $ extractIntVal valInt
        addrVar <- findAddr valVar
        tell $ [AMov (show AR11D) (createAddrIntRBP addrVar)]
        tell $ [AAdd (createAddrIntRBP newValOffset) (show AR11D)]

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else do -- both are variables
        addrVar1 <- findAddr val1
        addrVar2 <- findAddr val2

        tell $ [AMov (show AR11D) (createAddrIntRBP addrVar1)]
        tell $ [AAdd (show AR11D) (createAddrIntRBP addrVar2)]

        newValAddr <- allocInt AR11D

        local (Map.insert ident (qvar, newValAddr)) (genStmtsAsm rest)


    -- qvarOffset <- getNewOffsetUpdRBP intBytes
    -- let resAddr = createAddrIntRBP qvarOffset
    
    -- if isIntLiteral val1
    -- then
    --     tell $ [AMov resAddr (extractIntVal val1)]
    -- else do
    --     val1Addr <- findAddr val1
    --     tell $ [AMov resAddr val1Addr]

    -- if val1 is offset type: mov to r11d, next to allocInt, then add them
    -- if val2 is offset type
    -- if isIntLiteral val1
    -- then


