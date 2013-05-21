{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Provide an interface to https://github.com/travitch/datalog for
-- the backend
module Database.Dedalus.Datalog
  (
    DB(..)
  , mkDb
  ) where


import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text hiding (map,concatMap)
import Database.Datalog
import Database.Dedalus.Backend
import Database.Dedalus.PrettyPrint
import Database.Dedalus.Wrapper
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------

data DB = DB { fullyExtended :: Bool, db :: Datalog }


combine :: Datalog -> Datalog -> Datalog
-- combine a b = g (mappend a b) where
--  g (x, y) = (L.nub x, L.nub y)
combine (fa,ra) (fb,rb) = (f,r)
  where
    f = Map.unionWith combineOp fa fb
    r = Map.unionWith combineOp ra rb

    combineOp as bs = L.nub (as++bs)

-- return all derived facts, but don't commit them
derive :: State DB DB
derive = do
  DB b db <- get
  return $ if b then DB b db
                -- else DB True (combine db ((uncurry seminaive db, [])))
                else DB b db

instance Backend (State DB) where
   -- facts = liftM (fst . db) derive
   facts = liftM (L.concat . Map.elems . fst . db) derive
   rules = liftM (L.concat . Map.elems . snd . db) derive
   memoAll = derive >>= put
   declare adb = modify (\(DB _ db0) -> DB False (combine db0 adb))

   fullDb = do 
      d <- get
      return (db d)

-- ---------------------------------------------------------------------

{-

Generate a database from the facts:

*Database.Dedalus.Parser> run "a(b,c)."
Right ([Atom (C 0 "a") [C 1 "b",C 2 "c"]],[])

And rules:

*Database.Dedalus.Parser> run "c(B,C) :- c(A,B),c(A,C)."
Right ([],[Rule (Atom (C 0 "c") [Var (V "B"),Var (V "C")]) [Pat (Atom (C 0 "c") [Var (V "A"),Var (V "B")]),Pat (Atom (C 0 "c") [Var (V "A"),Var (V "C")])]])
*Database.Dedalus.Parser> 


A Datalog database, organised by name/arity
(fromList 
[
("a:2",[Atom (C 1 "a") [C 2 "b",C 3 "c"]]),
("a:3",[Atom (C 1 "a") [C 2 "b",C 3 "c",C 6 "d"]]),
("x:2",[Atom (C 0 "x") [C 4 "y",C 5 "z"]])
],

fromList 
[
("x:2",
  [
  Rule (Atom (C 0 "x") [Var (V "A"),Var (V "B")]) [Pat (Atom (C 1 "a") [Var (V "A"),Var (V "B")])],
  Rule (Atom (C 0 "x") [Var (V "Y"),Var (V "Z")]) [Pat (Atom (C 0 "x") [Var (V "Y"),Var (V "X")]),Pat (Atom (C 0 "x") [Var (V "X"),Var (V "Z")])]])
]
)

-}

mkDb :: Map.Map AtomSelector [Fact] -> Database ValueInfo
mkDb factMap = fromJust mkDb
  where
    mkDb = makeDatabase $ do
      mapM_  makeRelation $ Map.toList factMap


makeRelation ((name,arity),facts) = do
  rel <- addRelation (T.pack name) arity
  mapM_ (assertFact rel) (map toFact facts)

toFact :: Fact -> [ValueInfo]
toFact f = map (\p -> VV $ T.pack $ conName p) (atomArgs f)


