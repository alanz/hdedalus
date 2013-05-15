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
data Tuple = Tuple TransactionId Ref AttrName AttrValue

data AttrName  = AttrName !Text 

data AttrValue = AttrValue !Text

-- ---------------------------------------------------------------------

-- |Convert an external tuple into a format suitable for use in the
-- database
toFact :: Tuple -> [ValueInfo]
toFact (Tuple tid ref (AttrName attr) (AttrValue val)) = [TID tid,VR ref,VA attr, VV val]


-- |Build the extensional database from the tuples provided in
-- external storage format
buildEdb :: [Tuple] -> Database ValueInfo
buildEdb tuples = fromJust maybeDb
  where
   maybeDb = mkDb (map toFact tuples)

   mkDb :: [[ValueInfo]] -> Maybe (Database ValueInfo)
   mkDb facts = makeDatabase $ do
     db <- addRelation dbName 4

     mapM_ (assertFact db) facts


