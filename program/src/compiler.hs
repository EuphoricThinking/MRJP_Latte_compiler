import Typechecker hiding (main)

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

main :: IO () 
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= parseFileExec    
        [filename] -> readFile filename >>= parseFileExec

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