{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Dedalus.REPL
    (
    repl
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    hiding (concatMap, map)
import           Database.Dedalus.Backend
import           Database.Dedalus.DeSugar
import           Database.Dedalus.Datalog
import           Database.Dedalus.Parser
import           Database.Dedalus.PrettyPrint
import qualified Data.List                    as L
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import qualified Database.Datalog             as D
import           System.Console.Haskeline
import           Text.Parsec.Error
import           Text.Parsec.Prim             (getState, runParser, setState)

-- This code is initially based on
-- https://github.com/pchiusano/datalog-refactoring/blob/master/src/REPL.hs


-- ---------------------------------------------------------------------


main :: IO ()
main = do
  io <- stateLowerIO (DB True mempty)
  repl io


type ReplS = Env

showDoc :: Pretty p => p -> String
showDoc = show . doc

dlCmd :: Backend f => f (D.Database ValueInfo)
dlCmd = do
  d <- fullDb
  let r = mkDb $ dlFacts d
  return r

commands2 :: Backend f => [(String, f String)]
commands2 =
  [ ("facts",   liftM showDoc facts)
  , ("rules",   liftM showDoc rules)
  , ("desugar", liftM (showDoc . map desugar) rules)
  , ("last",    liftM showDoc queries)
  , ("dump",    liftM show    fullDb)
  , ("dl",      liftM show    dlCmd )
  ]

runCommands :: Backend f => (forall a . f a -> IO a) -> [(String, IO ())]
runCommands run = map (\(a,b) -> (a, run b >>= putStrLn)) commands2

-- ac f = get >>= lift . outputStrLn . show . doc . f . db . fst
ac f = get >>= lift . outputStrLn . show . doc . f . datalogDb . fst

repl :: Backend f => LowerIO f -> IO ()
repl io = let
   table = runCommands (trans io)

   parser :: P (Datalog, Env)
   parser = do
     adb <- statements
     env2 <- getState
     return (toDatalog adb, env2)

   parserQ :: P (Atom Term, Env)
   parserQ = do
     q <- queryP
     env2 <- getState
     return (q, env2)

   handleResultQ :: (Atom Term, Env) -> IO Env
   handleResultQ (q, env) = do
     res <- trans io $ query (desugarQuery q)
     -- res is [(Var,Con)], where the Con has Id -1
     putStrLn $ "Query result:\n" ++ (showDoc res)
     return env

   handleResult :: (Datalog, Env) -> IO Env
   handleResult (adb, env) = trans io $ declare adb >> return env

   handleError :: Env -> ParseError -> IO Env
   handleError env err = do
     putStrLn ("Parse error:" ++ (show err))
     return env

   parse :: Env -> String -> IO Env
   parse env line = either (handleError env) handleResult $
     runParser parser env "<console>" (T.pack line)

   parseQ :: Env -> String -> IO Env
   parseQ env line = either (handleError env) handleResultQ $
     runParser parserQ env "<console>" (T.pack line)


   loop :: [(String, IO ())]
        -> (Env -> String -> IO Env)
        -> (Env -> String -> IO Env)
        -> Env
        -> InputT IO Env
   loop commands stmt doQuery env = do
     minput <- getInputLine "% "
     case minput of
       Nothing -> return env
       Just ":q" -> return env
       Just ('?' : '-' : input) -> lift (doQuery env input) >>= \env2 -> loop commands stmt doQuery env2
       Just (':' : t) ->
         maybe (unrecognized t) lift (lookup t commands) >>
         loop commands stmt doQuery env
       Just input -> lift (stmt env input) >>= \env2 -> loop commands stmt doQuery env2

   unrecognized c = outputStrLn ("Unrecognized command " ++ c)

   in
   runInputT defaultSettings $ loop table parse parseQ initialEnv >> return ()


data NT m n = NT { trans :: forall a . m a -> n a }

type LowerIO m = NT m IO

stateLowerIO :: s -> IO (LowerIO (State s))
stateLowerIO s = do
  ref <- newIORef s
  return $ NT (\ma -> do
    si <- readIORef ref
    (a,s2) <- return $ runState ma si
    _ <- writeIORef ref s2
    return a)


