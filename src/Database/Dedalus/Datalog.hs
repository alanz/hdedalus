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
  , ValueInfo(..)
  ) where


import Data.Hashable
import Control.Monad ( liftM )
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Maybe
import Data.Text hiding (map,concatMap)
import Database.Dedalus.Backend
import Database.Dedalus.DeSugar
import Database.Dedalus.Parser
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.Datalog as D

-- ---------------------------------------------------------------------

-- |Base types that can be stored in the database
data ValueInfo = VV !Text    -- ^String Value
               | VI !Integer -- ^Integer Value
                   deriving (Eq,Ord,Show)

instance Hashable ValueInfo where
  hashWithSalt s (VV r)  = s `hashWithSalt` r `hashWithSalt` (1 :: Int)
  hashWithSalt s (VI r)  = s `hashWithSalt` r `hashWithSalt` (2 :: Int)

-- ---------------------------------------------------------------------

data DB = DB { fullyExtended :: Bool, datalogDb :: Datalog }


combineDbs :: Datalog -> Datalog -> Datalog
-- combineDbs a b = g (mappend a b) where
--  g (x, y) = (L.nub x, L.nub y)
combineDbs (DL fa ra qra) (DL fb rb qrb) = (DL f r qr)
  where
    f = Map.unionWith combineOp fa fb
    r = Map.unionWith combineOp ra rb

    combineOp as bs = L.nub (as++bs)
    qr = L.nub (qra++qrb)

-- return all derived facts, but don't commit them
derive :: State DB DB
derive = do
  DB b adb <- get
  return $ if b then DB b adb
                -- else DB True (combineDbs db ((uncurry seminaive db, [])))
                else DB True adb

instance Backend (State DB) where
   -- facts = liftM (fst . db) derive
   facts   = liftM (L.concat . Map.elems . dlFacts . datalogDb) derive
   rules   = liftM (L.concat . Map.elems . dlRules . datalogDb) derive
   queries = liftM (                     dlQueries . datalogDb) derive

   memoAll = derive >>= put
   declare adb = modify (\(DB _ db0) -> DB False (combineDbs db0 adb))

   -- query :: Atom Term -> f ([Fact]
   query q = do
     DB _b adb <- get
     -- let r= (executeQuery q adb) :: Maybe [Fact]
     let r= (executeQuery q (desugar adb)) :: Maybe [Fact]
     let res = fromMaybe [] r
     -- TODO: currently replacing old query results, perhaps keep them?
     put (DB False (adb { dlQueries = [(q,res)] }))
     return res

   fullDb = do
      d <- get
      return (datalogDb d)

-- ---------------------------------------------------------------------

executeQuery :: (D.Failure D.DatalogError m)
   => Atom Term -> Datalog -> m ([Fact])
executeQuery q (DL factMap ruleMap _qr) = do
  let
     edb = mkDb factMap
     dq  = mkQuery ruleMap q
  r <- D.queryDatabase edb dq
  -- TODO: identify the original Con Id value
  --       will have to post-process, it is not available in this environment
  -- let res = map (\[VV var,VV val] -> (V $ T.unpack var,C (-1) (T.unpack val)) ) r

  let res = map oneFact r
      -- TODO: what is an appropriate TS value?
      oneFact bs = Fact (Atom (atomPred q) vars) TSNext
        where vars = map toVar bs
              toVar (VV v) = C (-1) (S $ T.unpack v)
              toVar (VI v) = C (-1) (I v)

  return res

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
mkDb factMap = newDb
  where
    Just newDb = D.makeDatabase $ do
      mapM_  makeRelation $ Map.toList factMap

makeRelation :: (D.Failure D.DatalogError m)
             => ((String, Int), [Fact])
             -> D.DatabaseBuilder m ValueInfo ()
makeRelation ((name,arity),relFacts) = do
  rel <- D.addRelation (relationName (name,arity)) arity
  mapM_ (D.assertFact rel) (map toFact relFacts)

toFact :: Fact -> [ValueInfo]
toFact (Fact f _) = map toValueInfo (atomArgs f)
  where
    toValueInfo c@(C _ (S _)) = VV $ T.pack $ conName c
    toValueInfo   (C _ (I i)) = VI i

-- ---------------------------------------------------------------------

mkQuery :: (D.Failure D.DatalogError m )
  => Map.Map AtomSelector [Rule] -- ^existing rule base
  -> Atom Term                   -- ^the query predicate
  -> D.QueryBuilder m ValueInfo (D.Query ValueInfo)
mkQuery ruleMap q = do

 mapM_ makeQueryRelation $ Map.toList ruleMap

 qrel <- toRel q
 D.issueQuery qrel (headVars q)
 -- D.issueQuery qrel [D.Atom (VV "b"),D.LogicVar "X"]

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

makeQueryRelation :: D.Failure D.DatalogError m
   => ((String,Int),[Rule])
   -> D.QueryBuilder m ValueInfo (D.Relation)
makeQueryRelation ((name,arity),qrules) = do
  rel <- D.inferencePredicate (relationName (name,arity))
  let varNames = map (T.pack . varName)  $ varsInRules qrules
  let vars = Map.fromList $ map (\n -> (n,D.LogicVar n)) varNames

  mapM_ (\rule -> queryClause rel rule vars) qrules

  return rel

-- ---------------------------------------------------------------------

toRel :: D.Failure D.DatalogError m
  => Atom t -> D.QueryBuilder m a D.Relation
toRel x = D.inferencePredicate (relationName ((atomName x),arity))
    where arity = L.length $ atomArgs x

headVars :: Atom Term -> [D.Term ValueInfo]
headVars h = map toDTerm $ atomArgs h

toDTerm :: Term -> D.Term ValueInfo
-- Must be either D.LogicVar or D.Atom
toDTerm (Var n) = D.LogicVar (T.pack $ varName n)
toDTerm (Con n) = D.Atom (VV (T.pack $ conName n))


-- ---------------------------------------------------------------------

queryClause :: D.Failure D.DatalogError m
            => D.Relation
            -> Rule
            -> Map Text (D.Term ValueInfo)
            -> D.QueryBuilder m ValueInfo ()
queryClause rel rule vars = do
  let (Rule ruleHead ruleBody _) = rule

  let bodyTerms = map toDClause ruleBody

      toDClause (Pat term) = do
        crel <- toRel term
        let clauses = map toDTerm $ atomArgs term
        D.lit    crel clauses
      toDClause (Not term) = do
        crel <- toRel term
        let clauses = map toDTerm $ atomArgs term
        D.negLit crel clauses

  (rel, headVars ruleHead) D.|- bodyTerms

-- ---------------------------------------------------------------------

varsInRules :: [Rule] -> [Var]
varsInRules = L.nub . go []
  where
    go vs [] = vs
    go vs (r:rs) = go ((varsForRule r) ++ vs) rs

    varsForRule (Rule _ terms _) = map (\(Var v) -> v)
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
  let r = mkDb $ dlFacts ddb
  putStrLn $ "(ddb,r)=" ++ (show (ddb,r))
  return ()

tq :: IO ()
tq = do
  let (Right db) = run "a(b,c). a(X,Y) :- a(X,Y)."
  let ddb@(DL factMap ruleMap _) = toDatalog db
  let pq = tp queryP "a(b,X)."

  let edb = mkDb factMap
  -- rels <- mapM makeQueryRelation $ Map.toList ruleMap

  r <- executeQuery pq ddb
  putStrLn $ "(edb)=" ++ (show (edb))
  putStrLn $ "(ddb)=" ++ (show (ddb))
  putStrLn $ "(pq)=" ++ (show (pq))
  putStrLn $ "(r)=" ++ (show (r))
  return ()

tqn :: IO ()
tqn = do
  let (Right db) = run "a(b,0). a(X,Y) :- a(X,Y)."
  let ddb@(DL factMap ruleMap _) = toDatalog db
  let pq = tp queryP "a(b,X)."

  let edb = mkDb factMap
  -- rels <- mapM makeQueryRelation $ Map.toList ruleMap

  r <- executeQuery pq ddb
  putStrLn $ "(edb)=" ++ (show (edb))
  putStrLn $ "(ddb)=" ++ (show (ddb))
  putStrLn $ "(pq)=" ++ (show (pq))
  putStrLn $ "(r)=" ++ (show (r))
  return ()

tqs :: IO ()
tqs = do
  let (Right db) = run "a(b,0). a(X,Y) :- a(X,Y)."
  let ddb@(DL factMap ruleMap _) = toDatalog db
  -- let pq = tp queryP "a(b,X)."
  let pq = tp queryP "a(b,X,T)."

  let edb = mkDb factMap
  -- rels <- mapM makeQueryRelation $ Map.toList ruleMap

  r <- executeQuery pq ddb
  putStrLn $ "(edb)=" ++ (show (edb))
  putStrLn $ "(ddb)=" ++ (show (ddb))
  putStrLn $ "(pq)=" ++ (show (pq))
  putStrLn $ "(r)=" ++ (show (r))
  return ()

