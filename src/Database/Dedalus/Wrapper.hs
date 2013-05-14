{-# LANGUAGE OverloadedStrings #-}

module Database.Dedalus.Wrapper
    (
      ValueInfo(..)
    , AttributeDefinition(..)
    ) where

import Data.Hashable
import Data.Text hiding (map,concatMap)
import Database.Datalog

-- ---------------------------------------------------------------------

type TransactionId = String
    --               deriving (Eq,Ord,Show)

-- instance Hashable TransactionId where
--   hashWithSalt s t  = s `hashWithSalt` t `hashWithSalt` (1 :: Int)

data Ref = Ref !Text !Text
           deriving (Eq,Ord,Show)

instance Hashable Ref where
  hashWithSalt s (Ref d v)  = s `hashWithSalt` d `hashWithSalt` v `hashWithSalt` (1 :: Int)

-- |Base types that can be stored in the database
data ValueInfo = TID !TransactionId -- ^Transaction ID. Will be a timestamp initially
               | VR !Ref -- ^Ref type
               | VA !Text -- ^Attribute
               | VV !Text -- ^Value
               | VL ![ValueInfo] -- ^values for many relation
                   deriving (Eq,Ord,Show)

instance Hashable ValueInfo where
  hashWithSalt s (VR r)  = s `hashWithSalt` r `hashWithSalt` (1 :: Int)
  hashWithSalt s (VA r)  = s `hashWithSalt` r `hashWithSalt` (2 :: Int)
  hashWithSalt s (VV r)  = s `hashWithSalt` r `hashWithSalt` (3 :: Int)
  hashWithSalt s (VL r)  = s `hashWithSalt` r `hashWithSalt` (4 :: Int)
  hashWithSalt s (TID r) = s `hashWithSalt` r `hashWithSalt` (5 :: Int)

-- ---------------------------------------------------------------------

-- Each datom is an addition or retraction of a relation between an
-- entity, an attribute, a value, and a transaction.



-- Schema
--
-- The set of possible attributes a datom can specify is defined by a
-- database's schema.
--
-- Each Datomic database has a schema that describes the set of
-- attributes that can be associated with entities. A schema only
-- defines the characteristics of the attributes themselves. It does
-- not define which attributes can be associated with which entities.

-- Every new attribute is described by three required attributes:
--   :db/ident
--   :db/valueType
--   :db/cardinality
-- optional attribute
--   :db/doc
--   :db/unique
--   and some others

{-

As a transaction

[{:db/id #db/id[:db.part/db -1]
  :db/ident :person/name
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/one
  :db/doc "A person's name"}
 [:db/add :db.part/db :db.install/attribute #db/id[:db.part/db -1]]]

Simpler form

[{:db/id #db/id[:db.part/db]
  :db/ident :person/name
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/one
  :db/doc "A person's name"
  :db.install/_attribute :db.part/db}]

Both explode to the following datoms, in (tid,entity,attribute,value)
form, where tid is the transaction id.

[ (tid,#db/id[:db.part/db],:db/ident,      :person/name)
, (tid,#db/id[:db.part/db],:db/valueType,  :db.type/string)
, (tid,#db/id[:db.part/db],:db/cardinality,:db.cardinality/one)
, (tid,#db/id[:db.part/db],:db/doc,        "A person's name")
]

-}

data AttributeDefinition
  = Attribute ValueInfo -- ^Must be Ref
              [(ValueInfo,ValueInfo)] -- ^ [(Attribute,Value)]


-- Example attribute definition
attrPersonName = Attribute (VR (Ref ":db.part/db" "-1"))
                 [(VA ":db/ident",       VV ":person/name")
                 ,(VA ":db/valueType",   VV ":db.type/string")
                 ,(VA ":db/cardinality", VV ":db.cardinality/one")
                 ,(VA ":db/doc",         VV "A person's name")
                 ]

tid1 :: TransactionId
tid1 = "tid1"

toTransaction :: TransactionId -> AttributeDefinition
  -> [[ValueInfo]]
toTransaction tid definition = res
  where
    (Attribute ref vals) = definition
    res = map (\(a,v) -> [TID tid,ref,a,v]) vals

pp1 = toTransaction tid1 attrPersonName


schema :: TransactionId -> [AttributeDefinition] -> Maybe (Database ValueInfo)
schema tid definitions = makeDatabase $ do

  db <- addRelation "theDb" 4

  let schemaFacts = concatMap (toTransaction tid) definitions

  mapM_ (assertFact db) schemaFacts

pp2 = schema tid1 [attrPersonName]

{-

Dedalus0 restrictions

Schema:

  We require that the final attribute of every Dedalus0 predicate
  range over the domain Z. In a typical interpretation, Dedalus0
  programs will use this final attribute to connote a “timestamp,” so we
  refer to this attribute as the time suffix of the corresponding
  predicate.

Time Suffix:

  In a well-formed Dedalus0 rule, every subgoal must use the same exis-
  tential variable T as its time suffix. A well-formed Dedalus0 rule must
  also have a time suffix S as its rightmost head attribute, which must
  be constrained in exactly one of the following two ways:

  1. The rule is deductive if S is bound to the value T; that is, the
     body contains the subgoal S = T.

  2. The rule is inductive if S is the successor of T; that is, the body
     contains the subgoal successor(T, S).

Positive and Negative Predicates:

  For every extensional predicate r in a Dedalus0 program P,we add to
  P two distinguished predicates r_pos and r_neg with the same schema as
  r. We define r pos using the following rule:

    r_pos(A1, A2,[...],An,S) <- r(A1, A2,[...],An,T), S=T;

  That is, for every extensional predicate r there is an intensional
  predicate r pos that contains at least the contents of r. Intuitively,
  this rule allows extensional facts to serve as ground for r_pos, while
  enabling other rules to derive additional r_pos facts.

  The predicate r_pos may be referenced in the body or head of any
  Dedalus0 rule. We will make use of the predicate r_neg later to
  capture the notion of mutable state; we return to it in Section 3.2.
  Like r_pos, the use of r_neg in the heads and bodies of rules is
  unrestricted.

Guarded EDB: 

  No well-formed Dedalus0 rule may involve any extensional predicate,
  except for a rule of the form above.

-}
