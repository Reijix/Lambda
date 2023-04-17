module Main where

import Control.Exception (SomeException (SomeException), catch)
import Control.Monad.State (
  MonadIO (liftIO),
  MonadTrans (lift),
  StateT,
  evalStateT,
  gets,
  lift,
  modify,
 )
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Parser (parseLine, parsePrelude, readP)
import Reduction (reduce)
import Syntax (Declaration (..), Term)
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  runInputT,
 )
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- REPL
newtype ReplState = ReplState {declarations :: Map String Term}
insertDeclaration :: Declaration -> ReplM ()
insertDeclaration (Declaration name term) = modify (\state -> state {declarations = Map.insert name term (declarations state)})
type ReplM = StateT ReplState IO
initialState :: ReplState
initialState = ReplState . Map.fromList $ map (\(Declaration name term) -> (name, term)) []

main :: IO ()
main = evalStateT (readPrelude >> runInputT defaultSettings repl) initialState
  where
    repl :: InputT ReplM ()
    repl = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just line -> do
          lift $ evalLine line
          repl

-- Try reading prelude definitions from prelude.lmd
readPrelude :: ReplM ()
readPrelude = do
  decls <- liftIO $ catch getDecls handleException
  mapM_ insertDeclaration decls
  where
    getDecls :: IO [Declaration]
    getDecls = do
      handle <- liftIO $ openFile "prelude.lmd" ReadMode
      content <- liftIO $ hGetContents handle
      let declsE = parsePrelude content
      case declsE of
        Left err -> putStrLn ("Error while parsing prelude:\n" ++ show err) $> []
        Right decls -> putStrLn "Prelude successfully read from prelude.lmd!" $> decls
    handleException :: SomeException -> IO [Declaration]
    handleException e = putStrLn "Couldn't read file prelude.lmd so no prelude was loaded." >> return []

-- evaluate a single line from stdin
evalLine :: String -> ReplM ()
evalLine "" = return ()
evalLine line = either (liftIO . print) computeLine $ parseLine line

-- do computations for a single line
computeLine :: Either Declaration Term -> ReplM ()
computeLine (Left decl) = insertDeclaration decl
computeLine (Right term) = do
  decls <- gets declarations
  liftIO $ reduce 0 False decls term
