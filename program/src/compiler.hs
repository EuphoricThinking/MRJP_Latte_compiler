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

data Asm = AGlobl
    | SectText
    | ALabel String
    | ARet
    | ASpace
    | AFuncSpec String

instance Show Asm where
    show AGlobl = "\tglobal main"
    show SectText = "section .text"
    show (ALabel s) = s ++ ":\n"
    show ARet = "\tret\n"
    show ASpace = "\n"
    show (AFuncSpec s) = "\t" ++ s ++ "\n"

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
                            let ftuple = splitExtension filename
                            let fname = fst ftuple
                            let finalName = fname ++ ".s"
                            print $ finalName
                            (eithAsm, asmcode) <- genAssembly (extractQStore eitherQuad)quadcode
                            writeToFile filename (show asmcode)
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
addExternals (s : ss) = do
        tell $ [AFuncSpec s]
        addExternals ss

getSpecialWrapped s = AFuncSpec s

runGenAsm :: QuadCode -> AsmMonad Value
runGenAsm q = do--return BoolT
    tell $ [SectText]
    tell $ [AGlobl] 
    curState <- get
    addExternals (specialFunc curState)
    -- case (specialFunc curState) of
    --     [] -> 
    return BoolT
    