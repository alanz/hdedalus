{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Database.Dedalus.Backend
import Database.Dedalus.Parser
import Database.Dedalus.PrettyPrint
import Database.Dedalus.Wrapper
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.Datalog as D

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
  DB b adb <- get
  return $ if b then DB b adb
                -- else DB True (combine db ((uncurry seminaive db, [])))
                else DB True adb

instance Backend (State DB) where
   -- facts = liftM (fst . db) derive
   facts = liftM (L.concat . Map.elems . fst . db) derive
   rules = liftM (L.concat . Map.elems . snd . db) derive
   memoAll = derive >>= put
   declare adb = modify (\(DB _ db0) -> DB False (combine db0 adb))

   -- query :: Atom Term -> f (Maybe Subst)
   query q = do 
     DB _b adb <- get
     let res = executeQuery q adb
     return $ fromMaybe Nothing res

   fullDb = do 
      d <- get
      return (db d)

-- ---------------------------------------------------------------------

executeQuery :: (D.Failure D.DatalogError m)
   => Atom Term -> Datalog -> m (Maybe Subst)
executeQuery q (factMap,ruleMap) = do
  let
     edb = mkDb factMap
     dq  = mkQuery ruleMap q
  r <- D.queryDatabase edb dq
  -- TODO: identify the original Con Id value
  let res = map (\[VV var,VV val] -> (V $ T.unpack var,C 0 (T.unpack val)) ) r
  return $ Just res

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

-- ---------------------------------------------------------------------

relationName :: (String,Int) -> T.Text
relationName (name,arity) = T.pack (name ++ ":" ++ (show arity))

-- ---------------------------------------------------------------------

mkDb :: Map.Map AtomSelector [Fact] -> D.Database ValueInfo
mkDb factMap = fromJust mkDb
  where
    mkDb = D.makeDatabase $ do
      mapM_  makeRelation $ Map.toList factMap

makeRelation ((name,arity),facts) = do
  rel <- D.addRelation (relationName (name,arity)) arity
  mapM_ (D.assertFact rel) (map toFact facts)

toFact :: Fact -> [ValueInfo]
toFact f = map (\p -> VV $ T.pack $ conName p) (atomArgs f)

-- ---------------------------------------------------------------------

mkQuery :: (D.Failure D.DatalogError m )
  => Map.Map AtomSelector [Rule] -> Atom Term 
  -> D.QueryBuilder m ValueInfo (D.Query ValueInfo)
mkQuery ruleMap q = do
 -- Identify all the inferencePredicate relations
 -- Identify all the relationPredicate relations

 rels <- mapM makeQueryRelation $ Map.toList ruleMap

 D.issueQuery (L.head rels) [D.Atom (VV "b"),D.LogicVar "X"]

{-
      parentOf <- relationPredicateFromName "parentOf"
      ancestorOf <- inferencePredicate "ancestorOf"
      let x = LogicVar "x"
          y = LogicVar "y"
          z = LogicVar "z"
      (ancestorOf, [x, y]) |- [ lit parentOf [x, y] ]
      (ancestorOf, [x, y]) |- [ lit parentOf [x, z], lit ancestorOf [z, y] ]
      issueQuery ancestorOf [x, Atom "John" ]

-}

makeQueryRelation :: D.Failure D.DatalogError m =>
   ((String,Int),[Rule]) -> D.QueryBuilder m ValueInfo (D.Relation)
makeQueryRelation ((name,arity),rules) = do
  rel <- D.inferencePredicate (relationName (name,arity))
  let varNames = map (T.pack . varName)  $ varsInRules rules
  let vars = Map.fromList $ map (\n -> (n,D.LogicVar n)) varNames

  mapM_ (\rule -> queryClause rel rule vars) rules

  return rel

-- ---------------------------------------------------------------------

queryClause ::
  D.Failure D.DatalogError m =>
  D.Relation -> Rule -> Map Text (D.Term ValueInfo) -> D.QueryBuilder m ValueInfo ()
queryClause rel rule vars = do
  let (Rule head body) = rule

  let headVars = map toDTerm $ atomArgs head -- Must be either
                                             -- D.LogicVar or D.Atom
      toDTerm (Var n) = D.LogicVar (T.pack $ varName n)
      toDTerm (Con n) = D.Atom (VV (T.pack $ conName n))

  let bodyTerms = map toDClause body

      toRel x = D.inferencePredicate (relationName ((atomName x),arity))
         where arity = L.length $ atomArgs x

      toDClause (Pat term) = do
        rel <- toRel term
        let clauses = map toDTerm $ atomArgs term
        D.lit    rel clauses
      toDClause (Not term) = do
        rel <- toRel term
        let clauses = map toDTerm $ atomArgs term
        D.negLit rel clauses

  (rel, headVars) D.|- bodyTerms

-- ---------------------------------------------------------------------

varsInRules :: [Rule] -> [Var]
varsInRules rules = L.nub $ go [] rules
  where
    go vs [] = vs
    go vs (r:rs) = go ((varsForRule r) ++ vs) rs
    
    varsForRule (Rule _ terms) = map (\(Var v) -> v) 
                                 $ L.filter isVar $ concatMap atomArgs 
                                       $ map patAtom terms

    isVar :: Term -> Bool
    isVar (Var _) = True
    isVar _       = False

-- ---------------------------------------------------------------------
-- Testing

td :: IO ()
td = do
  let (Right db) = run "a(b,c). a(x,y,z). p(X,Z) :- p(X,Y), p(Y,Z). p(X,Y) :- a(X,Y)."
  let ddb = toDatalog db
  let r = mkDb $ fst ddb
  putStrLn $ "(ddb,r)=" ++ (show (ddb,r))
  return ()

tq :: IO ()
tq = do
  let (Right db) = run "a(b,c). a(X,Y) :- a(X,Y)."
  let ddb@(factMap,ruleMap) = toDatalog db
  let pq = tp queryP "a(b,X)."

  let edb = mkDb factMap
  -- rels <- mapM makeQueryRelation $ Map.toList ruleMap

  r <- executeQuery pq ddb
  putStrLn $ "(edb)=" ++ (show (edb))
  putStrLn $ "(ddb)=" ++ (show (ddb))
  putStrLn $ "(pq)=" ++ (show (pq))
  putStrLn $ "(r)=" ++ (show (r))
  return ()
