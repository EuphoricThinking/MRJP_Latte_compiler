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

import Data.Word
import Data.Bits

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
    | AAL
    | ACL
    | ADL
    | ABL
    | ASIL
    | ADIL
    | ASPL
    | ABPL
    | AR8B
    | AR9B
    | AR10B
    | AR11B
    | AR12B
    | AR13B
    | AR14B
    | AR15B


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
    | ASub String String
    | ANeg String
    | AImul String String
    | ASar String String
    | AIdiv String
    | ACdq
    | AAnd String String
    | AInc String
    | ADec String
    | AMovZX String String
    | ACmp String String
    | AJe String
    | ANot String
    | ATest String String
    | AXor String String
    | AJNE String
    | AJGE String -- >=  GE
    | AJL String -- <   LTH
    | AJG String -- >    GTH
    | AJLE String -- <=  LE
    | ASETE String
    | ASETNE String
    | ASETG String
    | ASETL String
    | ASETLE String
    | ASETGE String
    | AOr String String

-- push rbp := sub rsp, 8 \ mov [rsp], rbp
--
-- mov rbp, rsp // rsp to rbp - top of stack in rbp
-- rsp used for memory allocation, rbp for memory addressing
-- return address | old rbp ^^^rbp points here^^^

-- RSP ~ 0 mod 16 (before CALL, therefore 8 after CALL - pushed return address)

-- xor : if bits are the same = 0 ; otherwise = 1
-- xor eax, eax - zeroing the eax register (faster than xor al, al)

-- test al, al - bitwise `and` operation between al and itself; does not place the result int the target operand, but changes ZF (zero) flag; 
-- test al, al - a standard idiom to test if the 8-bit value stored in AL is zero; checking whether a Boolean is true or false, and branching accordingly

-- mov     BYTE PTR -1[rbp], 1 -- save bool variable
-- movzx   eax, BYTE PTR -1[rbp] -- move boool var to reg, zero-extend sign
-- xor     eax, 1 -- negate
-- test    al, al -- move check if zero (for branching)

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
    show (ASub v1 v2) = "\tsub " ++ v1 ++ ", " ++ v2
    show (ANeg mem) = "\tneg " ++ mem
    show (AImul v1 v2) = "\timul " ++ v1 ++ ", " ++ v2
    show (ASar v1 v2) = "\tsar " ++ v1 ++ ", " ++ v2
    show (AIdiv v1) = "\tidiv " ++ v1
    show ACdq = "\tcdq"
    show (AAnd v1 v2) = "\tand " ++ v1 ++ ", " ++ v2
    show (AInc s) = "\tinc " ++ s
    show (ADec s) = "\t dec " ++ s
    show (AMovZX s1 s2) = "\tmovzx " ++ s1 ++ ", " ++ s2
    show (ACmp s1 s2) = "\tcmp " ++ s1 ++ ", " ++ s2
    show (AJe s) = "\tje " ++ s
    show (ANot mem) = "\tnot " ++ mem
    show (ATest op1 op2) = "\ttest " ++ op1 ++ ", " ++ op2
    show (AXor op1 op2) = "\txor " ++ op1 ++ ", " ++ op2
    show (AJNE s) = "\tjne " ++ s
    show (AJG s) = "\tjg " ++ s
    show (AJGE s) = "\tjge " ++ s
    show (AJL s) = "\tjl " ++ s
    show (AJLE s) = "\tjle " ++ s
    show (ASETE s) = "\tsete " ++ s
    show (ASETNE s) = "\tsetne " ++ s
    show (ASETG s) = "\tsetg " ++ s
    show (ASETL s) = "\tsetl " ++ s
    show (ASETLE s) = "\tsetle " ++ s
    show (ASETGE s) = "\tsetge " ++ s
    show (AOr op1 op2) = "\tor " ++ op1 ++ ", " ++ op2

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
    show AAL = "al"
    show ACL = "cl"
    show ADL = "dl"
    show ABL = "bl"
    show ASIL = "sil"
    show ADIL = "dil"
    show ASPL = "spl"
    show ABPL = "bpl"
    show AR8B = "r8b"
    show AR9B = "r9b"
    show AR10B = "r10b"
    show AR11B = "r11b"
    show AR12B = "r12b"
    show AR13B = "r13b"
    show AR14B = "r14b"
    show AR15B = "r15b"

    -- ah:al in eax

data StoragePlace = OffsetRBP Int | Register AsmRegister | ProgLabel String

instance Show StoragePlace where
    show (OffsetRBP i) = show i
    show (Register reg) = show reg
    show (ProgLabel l) = l

type AsmCode = [Asm]

-- LocQVal ident -> ident : (qvar, memAddr) ; StringQVal s -> s : (_, label (data section or code section))
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
parameterRegistersBools = [ADIL, ASIL, ADL, ACL, AR8B, AR9B]

calleeSaved = [ARBP, ARBX, AR12, AR13, AR14, AR15] 

intBytes = 4
strPointerBytes = 8
boolBytes = 1

paramsStartOff = 16
stackParamSize = 8

numRegisterParams = 6

endSuffix = "_END"
functionLabel = ".L"
stringLabel = "LS"

labelRegister = ARIP

stackAlignment = 16
pushWord = 8
falseVal = 0

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
                            -- print $ (show eitherQuad)
                            -- print $ (show quadcode)
                            let ftuple = splitExtension filename
                            let fname = fst ftuple
                            let finalName = fname ++ ".s"
                            -- print $ finalName
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

subLocals 0 _ = return ()
-- TODO fix it -> all params are saved in memory
subLocals numLoc (FuncData name retType args locNum body numInts strVars strVarsNum numBools) = do 
    st <- get
    --printMesA $ "should not BE "  ++ (show numLoc) ++ " " ++ (curFuncNameAsm st)
    -- let localsSize = numInts*intBytes + strVarsNum*strPointerBytes--TODO add rest
    -- -- let stackParamsSize = sumParamsSizesPastRegisters args numRegisterParams
    -- -- let sumLocalsAndParamsSizes = localsSize + stackParamsSize -- parameters are saved in memory
    -- let paramsSizes = allParamsTypeSizes args 0
    -- let sumLocalsAndParamsSizes = paramsSizes + localsSize
    let sumLocalsAndParamsSizes = numInts*intBytes + strVarsNum*strPointerBytes + numBools*boolBytes

    -- printMesA $ "PARAMS " ++ (show args)

    -- printMesA $ "sum locals params: " ++ (show sumLocalsAndParamsSizes)
    -- -- printMesA $ "sum params: " ++ (show paramsSizes)
    -- -- printMesA $ "sum locals: " ++ (show localsSize)
    -- printMesA $ "numStrs: " ++ (show strVarsNum)
    -- printMesA $ "numInts: " ++ (show numInts)

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

createAddrBoolRBP memStorage =
    case memStorage of
        OffsetRBP offset -> "byte " ++ (createRelAddrRBP offset)
        Register reg -> show reg

getValToMov (IntQVal val) = val

printMesA mes = lift $ lift $ lift $ lift $ print mes

getNumArgs (FuncData _ _ args _ _ _ _ _ _) = length args

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
    --printMesA $ "ALLOC " ++ (show v)
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
    --printMesA $ "ALLOC_VAR " ++ (show v)
    -- gen command
    -- if register, mem location, constant
    if memSize == intBytes
    then
        tell $ [AMov (createAddrIntRBP storageOffset) (show v)]
    else --if memsize == strPointerBytes then do-- TODO extend for extensions  at this moment other is ptr
        tell $ [AMov (createAddrPtrRBP storageOffset) (show v)]
    -- else -- byte
    --     tell $ [AMov (createAddrBoolRBP storageOffset) (showBool v)]

    return storageOffset

allocBool b = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - boolBytes
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    let storageOffset = OffsetRBP newRBPOffset

    --printMesA $ "boolval " ++ (show $ getTrueOrFalseInt b)

    tell $ [AMov (createAddrBoolRBP storageOffset) (show $ getTrueOrFalseInt b)]

    return storageOffset

allocBoolFromMem memStorage = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - boolBytes
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    let storageOffset = OffsetRBP newRBPOffset

    tell $ [AMov (createAddrBoolRBP storageOffset) (show memStorage)]

    return storageOffset



getTrueOrFalseInt b =
    case b of
        True -> 1
        False -> 0

showBool b = show $ getTrueOrFalseInt b

getMemSize valType = 
    case valType of
        IntQ -> intBytes
        StringQ -> strPointerBytes
        BoolQ -> boolBytes

allocVarCopyFromMem memToBeCopied valType = do
    curRBP <- gets lastAddrRBP
    let newRBPOffset = curRBP - (getMemSize valType)
    curState <- get
    put curState {lastAddrRBP = newRBPOffset}

    let storageOffset = OffsetRBP newRBPOffset
   --printMesA $ "alllocVar " ++ (show memToBeCopied) ++ " " ++ (show valType) ++ " " ++ (show storageOffset)

    movMemoryVals storageOffset memToBeCopied valType

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
moveFromRegisters args [] [] [] = moveStackParams args paramsStartOff

moveFromRegisters [] _ _ _ = do
    curEnv <- ask
    return curEnv

moveFromRegisters ((ArgData ident valType) : args) (reg : regs) (ereg : eregs) (areg : aregs) = do
    let var = (QLoc ident valType)

    case valType of
        IntQ -> do
            offsetRBP <- allocVar ereg intBytes -- allocInt ereg

            local (Map.insert ident (var, offsetRBP)) (moveFromRegisters args regs eregs aregs)

        StringQ -> do
            offsetRBP <- allocVar reg strPointerBytes

            local (Map.insert ident (var, offsetRBP)) (moveFromRegisters args regs eregs aregs)

        BoolQ -> do
             offsetRBP <- getNewOffsetUpdRBP boolBytes
             tell $ [AMov (createAddrBoolRBP offsetRBP) (show areg)]
             -- tell $ [AMovZX (show AR11D) (show areg)]


             local (Map.insert ident (var, offsetRBP)) (moveFromRegisters args regs eregs aregs)

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

                        (BoolQ) -> do
                            tell $ [AMovZX (show AR11) (createAddrBoolRBP offset)]
                            tell $ [APush (show AR11)]

                            updateRSP pushWord

        (BoolQVal b) -> do
            tell $ [AMovZX (show AR11) (showBool b)]
            tell $ [APush (show AR11)]

            updateRSP pushWord

    pushParams rest

-- genStmt -> genParams
genParams qcall@((QCall qvar ident numArgs) : rest) _ _ = genStmtsAsm qcall
genParams [] _ _ = genStmtsAsm []
genParams qcode [] _ = do
    let qcallcode = getQCallCode qcode
    let reverseParams = paramsToStack qcode []
    pushParams reverseParams

    genStmtsAsm qcallcode


genParams (qp@(QParam val) : rest) (reg : regs) (ereg : eregs) = do
    --printMesA qp
    case val of
        (IntQVal v) -> do
            tell $ [AMov (show ereg) (show v)]
            -- genParams rest regs eregs

        (LocQVal ident valType) -> do
            varData <- asks (Map.lookup ident)
            case varData of
                Nothing -> throwError $ "No env data for " ++ ident
                Just (var, offset) -> do
                    --printMesA $ "param loc " ++ (show offset)
                    case valType of
                        (IntQ) -> do
                            tell $ [AMov (show ereg) (createAddrIntRBP offset)]

                        StringQ -> do
                            tell $ [AMov (show reg) (createAddrPtrRBP offset)]
                        BoolQ -> do
                            tell $ [AMovZX (show ereg) (createAddrBoolRBP offset)]

        (StrQVal s) -> do
            findLbl <- asks (Map.lookup s)
            case findLbl of
                Nothing -> throwError $ "string literal not found in data section (or env error): " ++ s
                Just (_, lbl) -> do
                    tell $ [AMov (show reg) (show lbl)]
                    --gen

        (BoolQVal b) -> do
            tell $ [AMovZX (show ereg) (showBool b)]

    genParams rest regs eregs

assignResToRegister var@(QLoc varTmpId varType) =
    -- case varType of
    --     IntQ -> (var, (Register AEAX))
    --     StringQ -> (var, (Register ARAX))
    case varType of
        IntQ -> (var, (Register AEAX))
        StringQ -> (var, (Register ARAX))
        BoolQ -> (var, (Register AAL))

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

iterOverAllFuncs ((QFunc finfo@(FuncData name retType args locNum body numInts strVars strVarsNum numBools)) : rest) = do
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

isBoolLiteral (BoolQVal _) = True
isBoolLiteral _ = False

extractIntVal (IntQVal v) = v

extractBoolVal (BoolQVal b) = b

extractLocQvarId (LocQVal id _) = id

findAddr v@(LocQVal ident _) = do
    --printMes $ "locqval lookup " ++ (show v)
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

createMemAddr memStorage locType =
    case memStorage of
        OffsetRBP offset -> do
            case locType of
                IntQ -> "dword " ++ (createRelAddrRBP offset)
                StringQ -> "qword " ++ (createRelAddrRBP offset)
                BoolQ -> "byte " ++ (createRelAddrRBP offset)

        Register reg -> show reg
        ProgLabel l -> l

createMemAddrRBPdword_qword (OffsetRBP offset) valType =
    case valType of
        IntQ -> "dword " ++ (createRelAddrRBP offset)
        StringQ -> "qword " ++ (createRelAddrRBP offset)
        BoolQ -> "byte " ++ (createRelAddrRBP offset)

moveTempToR11 memStorageAddr valType = 
    case valType of
        IntQ -> do
            tell $ [AMov (show AR11D) memStorageAddr]
            return AR11D
        StringQ -> do
            tell $ [AMov (show AR11) memStorageAddr]
            return AR11
        BoolQ -> do
            --printMesA $ "MOVER11 " ++ (show memStorageAddr)
            tell $ [AMov (show AR11B) memStorageAddr]
            return AR11B

-- movVarToR11 varLoc isLoc32bit = do
--     addr <- findAddr varLoc
--     moveTempToR11 (show addr) isLoc32bit

-- NOT suitable for booleans
movMemoryVals memToL memFromR valType = do
   --printMesA $ "memvals to: " ++ (show memToL) ++ " from: " ++ (show memFromR)
    let isLoc32bit = is32bit valType
    let rightAddr = createMemAddr memFromR valType--isLoc32bit
    let leftAddr = createMemAddr memToL valType --isLoc32bit

    if ((isOffset memFromR) && (isOffset memToL))
    then do
        r11_sized <- moveTempToR11 rightAddr valType
        tell $ [AMov leftAddr (show r11_sized)]
    else --if not isBoolType valType then do
        tell $ [AMov leftAddr rightAddr]
    -- else

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

addOrSubOneIntOneVar valInt varVal isAddition = do
    newValOffset <- allocInt $ extractIntVal valInt
    addrVar <- findAddr varVal
    tell $ [AMov (show AR11D) (createAddrIntRBP addrVar)]

    if isAddition
    then
        tell $ [AAdd (createAddrIntRBP newValOffset) (show AR11D)]
    else
        tell $ [ASub (createAddrIntRBP newValOffset) (show AR11D)]

    return newValOffset

-- findMemAddr val = do
addOrSubTwoVars val1 val2 isAddition = do
    addrVar1 <- findAddr val1
    addrVar2 <- findAddr val2

    tell $ [AMov (show AR11D) (createAddrIntRBP addrVar1)]

    if isAddition
    then
        tell $ [AAdd (show AR11D) (createAddrIntRBP addrVar2)]
    else
        tell $ [ASub (show AR11D) (createAddrIntRBP addrVar2)]

    newValAddr <- allocInt AR11D

    return newValAddr

imulOneVarOneInt intVal varVal = do
    addr2 <- findAddr varVal
    tell $ [AMov (show AR11D) (createAddrIntRBP addr2)]
    tell $ [AImul (show AR11D) (show $ extractIntVal intVal)]

    resAddr <- allocInt AR11D

    return resAddr

isPowerOfTwo val =
    case val of
        (IntQVal v) -> (v /= 0) && ((v .&. (v - 1)) == 0)
        otherwise -> False

findPowerOfTwo 1 pow = pow
findPowerOfTwo num pow = findPowerOfTwo (num `shiftR` 1) (pow + 1) -- printMesA $ (show num) ++ " " ++ (show pow) >> 

idivMovEAXR11D movArg1 movArg2 isModulo = do
    tell $ [AMov (show AEAX) movArg1]
    tell $ [ACdq] -- sign exetnd edx:eax
    tell $ [AMov (show AR11D) movArg2]
    tell $ [AIdiv (show AR11D)]

    if not isModulo
    then do
        resAddr <- allocInt AEAX
        return resAddr
    else do
        resAddr <- allocInt AEDX
        return resAddr

showIntLiteral intLit = show $ extractIntVal intLit

findIdentAddr ident = do
    idData <- asks (Map.lookup ident)
    case idData of
        Nothing -> throwError $ ident ++ " var not found for address determination"
        Just (_, memStorage) -> return memStorage

isString StringQ = True
isString _ = False

isBool BoolQ = True
isBool _ = False

increaseCodeLabelsCounter curLblCnt = do
    curState <- get
    put curState {labelsCounter = (curLblCnt + 1)}

createNewCodeLabel curLblCounter = functionLabel ++ (show curLblCounter)

getLabelOfStringOrLabel origLabel = do -- (isNew, label)
    lblData <- asks (Map.lookup origLabel)
    case lblData of
        Nothing -> do -- not created
            newLabel <- createNewLabelUpdateCounter
            return (True, (ProgLabel newLabel))
            --throwError $ origLabel ++ " string or label label not found"
        Just (_, codeLabel) -> return (False, codeLabel)

createNewLabelUpdateCounter = do
    curLabelNr <- gets (labelsCounter)
    let newLabel = createNewCodeLabel curLabelNr
    increaseCodeLabelsCounter curLabelNr

    return newLabel

createAddrLabel (ProgLabel l) = l 


-- a == b  cmp a, b jne
-- performAccordingToOperand operand memLeft memRight = do
--     case operand of
--         QEQU -> do
--             tell $
isArithmMode mode =
    case mode of
        QEQU -> True
        QNE -> True
        QGTH -> True
        QLTH -> True
        QLE -> True
        QGE -> True
        otherwise -> False

getJump mode codeLabel = do
    let label = createAddrLabel codeLabel
    case mode of
        QEQU -> tell $ [AJe label]
        QNE -> tell $ [AJNE label]
        QGTH -> tell $ [AJG label]
        QLTH -> tell $ [AJL label]
        QLE -> tell $ [AJLE label]
        QGE -> tell $ [AJGE label]


compareIntCond val1 val2 = do
    if isIntLiteral val1 && isIntLiteral val2
    then do
        tell $ [AMov (show AR11D) (showIntLiteral val1)]
        tell $ [ACmp (show AR11D) (showIntLiteral val2)]
        
    else if isIntLiteral val1 then do
        addr2 <- findAddr val2
        tell $ [AMov (show AR11D) (showIntLiteral val1)]
        tell $ [ACmp (show AR11D) (createAddrIntRBP addr2)]
        -- getJump mode codeLabel
    else if isIntLiteral val2 then do
        addr1 <- findAddr val1
        tell $ [ACmp (createAddrIntRBP addr1) (showIntLiteral val2)]
        -- getJump mode codeLabel
    else do
        addr1 <- findAddr val1
        addr2 <- findAddr val2
        tell $ [AMov (show AR11D) (createAddrIntRBP addr1)]
        tell $ [ACmp (show AR11D) (createAddrIntRBP addr2)]
            -- getJump mode codeLabel

chooseSETcc mode resultMemByte = do
    case mode of
        QEQU -> tell $ [ASETE resultMemByte]
        QNE -> tell $ [ASETNE resultMemByte]
        QGTH -> tell $ [ASETG resultMemByte] -- JG >
        QLTH -> tell $ [ASETL resultMemByte] -- JL <
        QLE -> tell $ [ASETLE resultMemByte] -- JLE <=
        QGE -> tell $ [ASETGE resultMemByte] --  JGE >=

getBoolCondValLiteralAndOr val1 val2 mode = 
    case mode of
        QAND -> ((extractBoolVal val1) && (extractBoolVal val2))
        QOR -> ((extractBoolVal val1) || (extractBoolVal val2))

--val1: boolLiteral ; val2: boolVar
performAndOr valToR11D addrCompRight mode = do
    --addrVar <- findAddr boolVar
    -- resAddr <- allocBool (extractBoolVal boolLiteral)

    tell $ [AMov (show AR11B) valToR11D] --(showBool $ extractBoolVal boolLiteral)]
    case mode of
        QAND -> do
            tell $ [AAnd (show AR11B) addrCompRight] --(createAddrBoolRBP addrVar)]
        QOR -> do
            tell $ [AOr (show AR11B) addrCompRight] --(createAddrBoolRBP addrVar)]

    resAddr <- allocBoolFromMem AR11B
    return resAddr

getBoolLiteralActionForGenStmt b isIfElse =
    case isIfElse of
        True -> b
        False ->  (not b)


                                            -- HELPER END ---------END----------

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

genFuncsAsm ((QFunc finfo@(FuncData name retType args locNum body numInts strVars strVarsNum numBools)) : rest) = do
    -- env <- ask
    -- strEnv <- local (const env) (createStrLiteralLabels strVars)

    tell $ [ALabel name]
    tell $ [AProlog]

    -- printMesA $ "NAME IN |" ++ name ++ "|"
    -- printMesA "boooodyyy"
    -- printMesA (show body)

    -- get size of params, subtract from the stack (probably iterate once again)
    -- clear store before function leave

    subLocals locNum finfo
    updateCurFuncNameAsm name

    env <- ask
    curEnv <- local (const env) (moveFromRegisters args parametersRegisterPoniters64 parametersRegistersInts32 parameterRegistersBools)

    local (const curEnv) (genStmtsAsm body)

    clearCurFuncParams

    st <- get
    -- printMesA "curs"
    -- printMesA st

    -- genStmtsAsm body
    genFuncsAsm rest

    -- what about recursive functions? 


-- ret needs to know the value, to move a good one to eax
genStmtsAsm :: QuadCode -> AsmMonad ()
genStmtsAsm ((QRet res) : rest) = do
   -- printMesA $ "RETCOMP " ++ (show res)
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
                    else if isString valType then do
                        movMemoryVals (Register ARAX) memStorageVar valType
                    else do
                        case memStorageVar of
                            OffsetRBP offset -> tell $ [AMovZX (show AEAX) (createAddrBoolRBP memStorageVar)]
                            Register r -> tell $ [AMovZX (show AEAX) (show r)]


        (StrQVal s) -> do
            moveStringToMem (Register ARAX) s

        (BoolQVal b) -> tell $ [AMov (show AEAX) (showBool b)]

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

            -- at this moment the variables are stored only in memory
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

                (BoolQVal b) -> do
                   -- printMesA $ "showbool " ++ (showBool b)
                    tell $ [AMov (createAddrBoolRBP memStorageL) (showBool b)]

                (LocQVal ident valType) -> do
                    valStorage <- asks (Map.lookup ident)
                    case valStorage of
                        Nothing -> throwError $ ident ++ " var to be assigned not in env"
                        Just (varFromAssigned, storageR) -> do
                            --printMesA $ "ASSIGN loc " ++ ident ++ " " ++ (show id)
                            -- all variables are stored in the memory at this moment, but this is for further extensions
                            -- TODO mov to another procedure, add more options
                            if isOffset storageR
                            then do
                                if isIntQ valType --is32bit valType
                                then do
                                    tell $ [AMov (show AR11D) (createAddrIntRBP storageR)]
                                    tell $ [AMov (createAddrIntRBP memStorageL) (show AR11D)]
                                else if isString valType then do
                                    tell $ [AMov (show AR11) (createAddrPtrRBP storageR)]
                                    tell $ [AMov (createAddrPtrRBP memStorageL) (show AR11)]
                                else do
                                    tell $ [AMovZX (show AR11D) (createAddrBoolRBP storageR)]
                                    tell $ [AMov (createAddrBoolRBP memStorageL) (show AR11B)]
                            else do
                                if isIntQ valType--is32bit valType
                                then
                                    tell $ [AMov (createAddrIntRBP memStorageL) (show storageR)]
                                else if isIntQ valType then
                                    tell $ [AMov (createAddrPtrRBP memStorageL) (show storageR)]
                                else 
                                    tell $ [AMov (createAddrBoolRBP memStorageL) (show storageR)] -- TODO fix this --> different types of registers
                            

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
                    newOffset <- allocVarCopyFromMem storage valType

                    local (Map.insert name (varFromAssigned, newOffset)) (genStmtsAsm rest)
                    -- case valType of
                    --     IntQ -> do
                    --         printMesA $ "DECL loc " ++ (show valStorage)
                    --         newRBPOffset <- 
                    --         -- newRBPOffset <- allocVar storage intBytes -- allocInt storage
                    --         local (Map.insert name (varFromAssigned, newRBPOffset)) (genStmtsAsm rest)

                    --     StringQ -> do
                    --         newRBPOffset <- allocVar storage strPointerBytes
                    --         local (Map.insert name (varFromAssigned, newRBPOffset)) (genStmtsAsm rest)

        (StrQVal s) -> do
            strData <- asks (Map.lookup s)
            case strData of
                Nothing -> throwError $ "No string label for literal value: " ++ s
                Just (_, strLabel) -> do
                    newRBPOffset <- allocVar strLabel strPointerBytes
                    local (Map.insert name (var, newRBPOffset)) (genStmtsAsm rest)

        (BoolQVal b) -> do
            --printMesA $ "decl bool"
            newRBPOffset <- allocBool b
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

            --printMesA $ "IN PRINT " ++ (show rest)

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
        newValOffset <- addOrSubOneIntOneVar valInt valVar True

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else do -- both are variables
        newValAddr <- addOrSubTwoVars val1 val2 True

        local (Map.insert ident (qvar, newValAddr)) (genStmtsAsm rest)


genStmtsAsm ((QSub qvar@(QLoc ident valType) val1 val2) : rest) = do
    --printMesA $ "\t SUB " ++ (show qvar) ++ " " ++ (show val1) ++ " " ++ (show val2)
    
    if (isIntLiteral val1) && (isIntLiteral val2)
    then do
        newValOffset <- allocInt $ extractIntVal val1
        tell $ [ASub (createAddrIntRBP newValOffset) (show $ extractIntVal val2)]

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else if isIntLiteral val1
    then do
        newValOffset <- addOrSubOneIntOneVar val1 val2 False

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else if isIntLiteral val2 
    then do
        addrVar1 <- findAddr val1
        tell $ [AMov (show AR11D) (createAddrIntRBP addrVar1)]
        tell $ [ASub (show AR11D) (show $ extractIntVal val2)]
        
        resAddr <- allocInt AR11D

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
    else do
        newValAddr <- addOrSubTwoVars val1 val2 False

        local (Map.insert ident (qvar, newValAddr)) (genStmtsAsm rest)

genStmtsAsm ((QNeg qvar@(QLoc ident valType) val) : rest) = do
    if isIntLiteral val
    then do
        let intVal = extractIntVal val
        --printMesA $ "neg intval " ++ (show intVal) ++ " | " ++ (show val)
        intAddr <- allocInt (-intVal)

        local (Map.insert ident (qvar, intAddr)) (genStmtsAsm rest)
    else do -- locVal
        valAddr <- findAddr val
        tell $ [AMov (show AR11D) (createAddrIntRBP valAddr)]
        tell $ [ANeg (show AR11D)]

        resAddr <- allocInt AR11D

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

genStmtsAsm ((QMul qvar@(QLoc ident valType) val1 val2) : rest) = do
    if (isIntLiteral val1) && (isIntLiteral val2)
    then do
        tell $ [AMov (show AR11D) (show $ extractIntVal val1)]
        tell $ [AImul (show AR11D) (show $ extractIntVal val2)]

        newValOffset <- allocInt AR11D

        local (Map.insert ident (qvar, newValOffset)) (genStmtsAsm rest)
    else if isIntLiteral val1 then do
        resAddr <- imulOneVarOneInt val1 val2

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
    else if isIntLiteral val2 then do
        resAddr <- imulOneVarOneInt val2 val1

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
    else do -- both are variables
        addr1 <- findAddr val1
        addr2 <- findAddr val2

        tell $ [AMov (show AR11D) (createAddrIntRBP addr1)]
        tell $ [AImul (show AR11D) (createAddrIntRBP addr2)]

        resAddr <- allocInt AR11D

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

genStmtsAsm ((QDiv qvar@(QLoc ident valType) val1 val2) : rest) = do
    if isPowerOfTwo val2 && isIntLiteral val2
    then do
        let power = findPowerOfTwo (extractIntVal val2) 0
        --printMesA $ "POWER " ++ (show $ extractIntVal val2)
        if isIntLiteral val1 -- then both are && isIntLiteral val2
        then do
            resAddr <- allocInt $ extractIntVal val1
            -- let powerTwo = findPowerOfTwo (extractIntVal val2) 0
            tell $ [ASar (createAddrIntRBP resAddr) (show power)]--(show $ extractIntVal val2)]
        
            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else do -- only val2 is intLIteral
            -- addr1 <- findAddr val1 
            -- r11d <- movVarToR11 val1 (is32bit $ getValType val1)
            addr1 <- findAddr val1
            tell $ [AMov (show AR11D) (createAddrIntRBP addr1)]
            tell $ [ASar (show AR11D) (show power)] --(show $ extractIntVal val2)]
            resAddr <- allocInt AR11D

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

        -- else do -- both vars
        --     r11d <- movVarToR11 val1 (is32bit val1)
        --     tell $ [ASar (show AR11D) (show val2)]
        --     resAddr <- allocInt AR11D

        --     local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
    else do
        if isIntLiteral val1 && isIntLiteral val2
        then do
            -- tell $ [AMov (show AEAX) (show val1)]
            -- tell $ [ACdq] -- sign exetnd edx:eax
            -- tell $ [AMov (show AR11D) (show $ extractIntVal val2)]
            -- tell $ [AIdiv (show AR11D)]

            -- resAddr <- allocInt AEAX
            resAddr <- idivMovEAXR11D (showIntLiteral val1) (showIntLiteral val2) False

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else if isIntLiteral val1 then do
            addr2 <- findAddr val2

            -- tell $ [AMov (show AEAX) (show val1)]
            -- tell $ [ACdq] -- sign exetnd edx:eax
            -- tell $ [AMov (show AR11D) (createAddrIntRBP addr2)]
            -- tell $ [AIdiv (show AR11D)] 
            
            -- resAddr <- allocInt AEAX
            resAddr <- idivMovEAXR11D (showIntLiteral val1) (createAddrIntRBP addr2) False

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else if isIntLiteral val2 then do
            addr1 <- findAddr val1
            -- tell $ [AMov (show AEAX) (createAddrIntRBP addr1)]
            -- tell $ [ACdq] -- sign exetnd edx:eax
            -- tell $ [AMov (show AR11D) (show $ extractIntVal val2)]
            -- tell $ [AIdiv (show $ AR11D)] 
            
            -- resAddr <- allocInt AEAX
            resAddr <- idivMovEAXR11D (createAddrIntRBP addr1) (showIntLiteral val2) False

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else do
            addr1 <- findAddr val1
            addr2 <- findAddr val2

            -- tell $ [AMov (show AEAX) (createAddrIntRBP addr1)]
            -- tell $ [ACdq] -- sign exetnd edx:eax
            -- tell $ [AIdiv (createAddrIntRBP addr2)] 

            -- resAddr <- allocInt AEAX
            resAddr <- idivMovEAXR11D (createAddrIntRBP addr1) (createAddrIntRBP addr2) False

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)


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


genStmtsAsm ((QMod qvar@(QLoc ident valType) val1 val2) : rest) = do
    if isPowerOfTwo val2 && isIntLiteral val2
    then do
        -- let power = findPowerOfTwo (extractIntVal val2) 0
        let andModuloLeft = (extractIntVal $ val2) - 1
        if isIntLiteral val1 -- both are
        then do
            resAddr <- allocInt $ extractIntVal val1
            
            -- res = x & (pow - 1)
            tell $ [AAnd (createAddrIntRBP resAddr) (show andModuloLeft)]

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else do
            addr1 <- findAddr val1
            tell $ [AMov (show AR11D) (createAddrIntRBP addr1)]
            tell $ [AAnd (show AR11D) (show andModuloLeft)]

            resAddr <- allocInt AR11D

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

    else do
        if isIntLiteral val1 && isIntLiteral val2
        then do
            let int1 = extractIntVal val1
            let int2 = extractIntVal val2
            let modulus = int1 `rem` int2

            resAddr <- allocInt modulus

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else if isIntLiteral val1 then do
            addr2 <- findAddr val2
            resAddr <- idivMovEAXR11D (showIntLiteral val1) (createAddrIntRBP addr2) True

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

        else if isIntLiteral val2 then do
            addr1 <- findAddr val1

            resAddr <- idivMovEAXR11D (createAddrIntRBP addr1) (showIntLiteral val2) True

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else do -- both are avars
            addr1 <- findAddr val1
            addr2 <- findAddr val2

            resAddr <- idivMovEAXR11D (createAddrIntRBP addr1) (createAddrIntRBP addr2) True

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

genStmtsAsm ((QDec qvar@(QLoc resName valType) ident) : rest) = do
    varAddr <- findIdentAddr ident
    tell $ [ADec (createAddrIntRBP varAddr)]

    genStmtsAsm rest

genStmtsAsm ((QInc qvar@(QLoc resName valType) ident) : rest) = do
    varAddr <- findIdentAddr ident
    tell $ [AInc (createAddrIntRBP varAddr)]

    genStmtsAsm rest

genStmtsAsm ((QIf val labelFalse) : rest) = do
    -- add a new label
    -- create a new label


    curLabelNr <- gets (labelsCounter)
    let newLabelFalse = createNewCodeLabel curLabelNr
    increaseCodeLabelsCounter curLabelNr

    case val of
        (BoolQVal b) -> do
            case b of 
                False -> do 
                    tell $ [AJmp newLabelFalse]
                    local (Map.insert labelFalse (NoMeaning, (ProgLabel newLabelFalse))) (genStmtsAsm rest)

                True -> do
                    local (Map.insert labelFalse (NoMeaning, (ProgLabel newLabelFalse))) (genStmtsAsm rest)

        qvar@(LocQVal ident valType) -> do
            -- load from memory
            -- cmp 0
            -- must be bool type - due to typechecker
            --printMesA $ "findloc qif "
            loc <- asks (Map.lookup ident)
            --printMesA $ (show loc)
            varAddr <- findAddr qvar
            --printMesA $ "found loc"
            -- not test var, var, because it would require additional loading to the register from the register
            -- tell $ [AMovzx (show AEAX) (createAddrBoolRBP varAddr)]
            -- tell $ [ATest (show AAL) (show AAL)]
            
            tell $ [ACmp (createAddrBoolRBP varAddr) (show falseVal)]
            
            tell $ [AJe newLabelFalse]

            local (Map.insert labelFalse (NoMeaning, (ProgLabel newLabelFalse))) (genStmtsAsm rest)


        -- rel sould be in if

        --(LocQVal ident valType) -> do

-- generate label
genStmtsAsm ((QLabel labelFalse) : rest) = do
    -- hceck label in store
    -- printMesA $ "LABLE HERE " ++ labelFalse
    (isNew, codeLabel) <- getLabelOfStringOrLabel labelFalse
    tell $ [ALabel (createAddrLabel codeLabel)]

    -- genStmtsAsm rest
    if isNew then
        local (Map.insert labelFalse (NoMeaning, codeLabel)) (genStmtsAsm rest)
    else
        genStmtsAsm rest

genStmtsAsm ((QGoTo label) : rest) = do
    --printMesA $ "IN GOTO " ++ (show rest)
    (isNew, codeLabel) <- getLabelOfStringOrLabel label
    --printMesA $ "after codelabel"

    tell $ [AJmp (createAddrLabel codeLabel)]

    if isNew then
        local (Map.insert label (NoMeaning, codeLabel)) (genStmtsAsm rest)
    else
        genStmtsAsm rest

genStmtsAsm ((QNot qvar@(QLoc ident valType) val) : rest) = do
    case val of
        (BoolQVal b) -> do
            boolAddr <- allocBool (not b)

            local (Map.insert ident (qvar, boolAddr)) (genStmtsAsm rest)

        (LocQVal nameLocExpr valType) -> do
            valAddr <- findAddr val -- (LocQVal valTmpName valType)

            -- tell $ [AMovZX (show AR11D) (createAddrBoolRBP valAddr)]
            -- tell $ [ANot (show AR11D)]
            
            -- tell $ [AMovZX (show AEAX) (createAddrBoolRBP valAddr)]
            -- tell $ [ANot (show AEAX)]

            tell $ [AMovZX (show AR11D) (createAddrBoolRBP valAddr)]
            tell $ [AXor (show AR11D) (show 1)]

            --printMesA $ "before NOT ALLOC"

            resAddr <- allocBoolFromMem AR11B --AAL --AR11B -- 

            --printMesA $ " rest NOT " ++ (show rest)   

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)


-- genStmtsAsm ((QCond qvar val1 val2 operand) : rest) = do
--     if isIntLiteral val1 && isIntLiteral val2
--     then do
    
genStmtsAsm (j@(JumpCondQ label val1 val2 mode) : rest) = do
    (isNew, codeLabel) <- getLabelOfStringOrLabel label
    -- printMesA $ "jumpcond " ++ (show j) ++ " label: " ++ (show codeLabel) ++ " is new: " ++ (show isNew)

    if isArithmMode mode
    then do
        --printMesA $ "isA"
        compareIntCond val1 val2
        getJump mode codeLabel

        if isNew then
            local (Map.insert label (NoMeaning, codeLabel)) (genStmtsAsm rest)
        else
            genStmtsAsm rest

    else
        -- possible todo
        if isNew then
            local (Map.insert label (NoMeaning, codeLabel)) (genStmtsAsm rest)
        else
            genStmtsAsm rest

genStmtsAsm ((QCond qvar@(QLoc ident valType) val1 val2 mode) : rest) = do
    if isArithmMode mode then do
        compareIntCond val1 val2
        resAddr <- getNewOffsetUpdRBP intBytes
        chooseSETcc mode (createAddrIntRBP resAddr)

        local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
    else do
        if isBoolLiteral val1 && isBoolLiteral val2 then do
            let conditionRes = getBoolCondValLiteralAndOr val1 val2 mode
            resAddr <- allocBool conditionRes

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)
        else if isBoolLiteral val1 then do
            addr2 <- findAddr val2
            -- resAddr <- performAndOrOneLiteral val1 val2 mode
            resAddr <- performAndOr (showBool $ extractBoolVal val1) (createAddrBoolRBP addr2) mode

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

        else if isBoolLiteral val2 then do
            --resAddr <- performAndOrOneLiteral val2 val1 mode
            addr1 <- findAddr val1
            -- resAddr <- performAndOrOneLiteral val1 val2 mode
            resAddr <- performAndOr (showBool $ extractBoolVal val2) (createAddrBoolRBP addr1) mode

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

        else do
            addr1 <- findAddr val1
            addr2 <- findAddr val2

            resAddr <- performAndOr (createAddrBoolRBP addr1) (createAddrBoolRBP addr2) mode

            local (Map.insert ident (qvar, resAddr)) (genStmtsAsm rest)

genStmtsAsm ((QWhile val labelWhile) : rest) = do
    -- printMesA $ "in while"
    (_, label) <- getLabelOfStringOrLabel labelWhile
    --printMesA $ "label WHILE " ++ (show label)
    case val of
        (BoolQVal b) -> do
            case b of
                True -> do
                    tell $ [AJmp (createAddrLabel label)] -- label must have been assigned previously
                    genStmtsAsm rest

                False -> genStmtsAsm rest
        
        qvar@(LocQVal ident valType) -> do          
            loc <- asks (Map.lookup ident)
            varAddr <- findAddr qvar

            tell $ [ACmp (createAddrBoolRBP varAddr) (show falseVal)]
            tell $ [AJNE (createAddrLabel label)]

            genStmtsAsm rest