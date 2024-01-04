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

data AsmRegister = ARAX
    | AEAX

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
    show (AFuncSpec s) = "\t" ++ s
    show AExtern = "\textern "
    show AProlog = "\tpush rbp\n\tmov rsp, rbp"
    show AEpilog = "\tmov rbp, rsp\n\tpop rbp\n\tret" -- check recording 7.28
    show (AAllocLocals num)= "\tsub rsp, " ++ (show num)
    show (AMov s1 s2) = "\tmov " ++ s1 ++ ", " ++ s2

instance Show AsmRegister where
    show ARAX = "rax"
    show AEAX = "eax"

type AsmCode = [Asm]

type AsmMonad a = ReaderT Env (StateT QStore (ExceptT String (WriterT AsmCode IO))) a 

extractQStore (Right (_, qstore)) = qstore
extractAsmCode (Right (_, acode)) = acode

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
                            (eithAsm, asmcode) <- genAssembly (extractQStore eitherQuad)quadcode
                            writeToFile filename (unlines $ map show asmcode)
                            exitSuccess
                            --printOK >> getQuadcode p >>= writeToFile filename >> exitSuccess

writeToFile path program =
    let
        ftuple = splitExtension path
        fname = fst ftuple
        finalName = fname ++ ".s"
    in
        writeFile finalName program
    

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

genAssembly quadstore quadcode = runWriterT $ runExceptT $ evalStateT (runReaderT (runGenAsm quadcode) Map.empty) quadstore

addExternals :: [String] -> AsmMonad ()
addExternals [] = tell $ [ASpace]
addExternals s = do
        --tell $ []
        --addExternals ss
        tell $ [AFuncSpec (getSpecialWrapped s)]
        tell $ [ASpace]

getSpecialWrapped s = (show AExtern) ++ (go s) where
    go (s : []) = s
    go (s : ss) = (show s) ++ ", " ++ (go ss)

subLocals 0 = return ()
subLocals numLoc = tell $ [AAllocLocals numLoc]

runGenAsm :: QuadCode -> AsmMonad Value
runGenAsm q = do--return BoolT
    tell $ [SectText]
    tell $ [AGlobl] 
    curState <- get
    addExternals (specialFunc curState)
    genFuncsAsm q
    -- case (specialFunc curState) of
    --     [] -> 
    return BoolT


genFuncsAsm :: QuadCode -> AsmMonad ()
genFuncsAsm [] = return ()

genFuncsAsm ((QFunc (FuncData name retType args locNum body)) : rest) = do
    tell $ [ALabel name]
    tell $ [AProlog]

    subLocals locNum

    genStmtsAsm body
    genFuncsAsm rest

    -- what about recursive functions?

-- ret needs to know the value, to move a good one to eax
genStmtsAsm :: QuadCode -> AsmMonad ()
genStmtsAsm ((QRet (IntQVal numVal)) : rest) = do
    tell $ [AMov (show AEAX) (show numVal)]
    tell $ [ARet]
    tell $ [ASpace]
    
    genStmtsAsm rest

genStmtsAsm [] = return ()

-- genStmtsAsm _ = undefined


    