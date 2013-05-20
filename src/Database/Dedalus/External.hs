-- |External interface to Dedalus, managing context between external
-- storage, EDB, and transactions.

module Database.Dedalus.External
    (
      Tuple(..)
    , buildEdb
    ) where

import Data.Hashable
import Data.Maybe
import Data.Text hiding (map,concatMap)
import Database.Datalog
import Database.Dedalus.Wrapper

-- ---------------------------------------------------------------------

-- |The structure which will be stored in external format
data Tuple = Tuple TransactionId Ref AttrName AttrValue Stamp

data AttrName  = AttrName !Text 

data AttrValue = AttrValue !Text

-- |A Dedalus "timestamp". This is simply an integer that increases
-- monotonically as time passes in each subsystem
data Stamp = TS !Integer

-- ---------------------------------------------------------------------

-- |Convert an external tuple into a format suitable for use in the
-- database
toFact :: Tuple -> [ValueInfo]
toFact (Tuple tid ref (AttrName attr) (AttrValue val) (TS ts)) = [TID tid,VR ref,VA attr, VV val, VT ts]


-- |Build the extensional database from the tuples provided in
-- external storage format
buildEdb :: [Tuple] -> Database ValueInfo
buildEdb tuples = fromJust maybeDb
  where
   maybeDb = mkDb (map toFact tuples)

   mkDb :: [[ValueInfo]] -> Maybe (Database ValueInfo)
   mkDb facts = makeDatabase $ do
     db <- addRelation dbName 5

     mapM_ (assertFact db) facts


-- ---------------------------------------------------------------------


mkRule dbName dbTupleLength facts = makeDatabase $ do
  db <- addRelation dbName dbTupleLength
  mapM_ (assertFact db) facts

