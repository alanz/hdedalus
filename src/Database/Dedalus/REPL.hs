{-# LANGUAGE Rank2Types #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Dedalus.REPL
    (
    repl
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text hiding (map,concatMap)
import Database.Datalog
import Database.Dedalus.Backend
import Database.Dedalus.Parser
import Database.Dedalus.PrettyPrint
import Database.Dedalus.Wrapper
import System.Console.Haskeline
import Text.ParserCombinators.Poly.State
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as L

-- This code is initially based on 
-- https://github.com/pchiusano/datalog-refactoring/blob/master/src/REPL.hs


-- ---------------------------------------------------------------------

-- Dummy declarations for now
data DB = DB { fullyExtended :: Bool, db :: Datalog }   


initialEnv :: Env
initialEnv = Env { envNextFree = 0, envConMap = Map.empty } 

getState = stGet

-- ---------------------------------------------------------------------

combine :: Datalog -> Datalog -> Datalog
combine a b = g (mappend a b) where
  g (x, y) = (L.nub x, L.nub y)

-- return all derived facts, but don't commit them
derive :: State DB DB
derive = do 
  DB b db <- get
  return $ if b then DB b db 
                -- else DB True (combine db ((uncurry seminaive db, [])))
                else DB b db

instance Backend (State DB) where
   facts = liftM (fst . db) derive 
   rules = liftM (snd . db) derive 
   memoAll = derive >>= put
   declare db = modify (\(DB _ db0) -> DB False (combine db0 db)) 

-- ---------------------------------------------------------------------

main :: IO ()
main = do 
  io <- stateLowerIO (DB True mempty)
  repl io 


type ReplS = Env

showDoc :: Pretty p => p -> String
showDoc = show . doc

commands2 :: Backend f => [(String, f String)]
commands2 = [
  -- ("facts", liftM showDoc facts), 
  -- ("rules", liftM showDoc rules)
  ]

runCommands :: Backend f => (forall a . f a -> IO a) -> [(String, IO ())]
runCommands run = map (\(a,b) -> (a, run b >>= putStrLn)) commands2  

ac f = get >>= lift . outputStrLn . show . doc . f . db . fst

repl :: Backend f => LowerIO f -> IO ()
repl io = let 
   table = runCommands (trans io)

   parser :: P (Datalog, Env)
   parser = do 
     db <- statements
     env2 <- getState
     return (db, env2)

   handleResult :: (Datalog, Env) -> IO Env 
   handleResult (db, env) = trans io $ declare db >> return env

   parse :: Env -> String -> IO Env
   parse env line = either (error . show) handleResult $
     runParser parser env "<console>" (T.pack line)

   loop :: [(String, IO ())] 
        -> (Env -> String -> IO Env) 
        -> Env 
        -> InputT IO Env
   loop commands stmt env = do
     minput <- getInputLine "% "
     case minput of
       Nothing -> return env
       Just ":q" -> return env
       Just (':' : t) -> 
         maybe (unrecognized t) lift (lookup t commands) >> 
         loop commands stmt env
       Just input -> lift (stmt env input) >>= \env2 -> loop commands stmt env2 

   unrecognized c = outputStrLn ("Unrecognized command " ++ c)

   in 
   runInputT defaultSettings $ loop table parse initialEnv >> return ()


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


