import Data.Hashable
import Data.Text hiding (map)
import Data.Time.Clock
import Data.Typeable
import Database.Datalog
import Data.EDN

main = do 
  putStrLn "Hello World"




-- Datomic play

-- Schema
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


