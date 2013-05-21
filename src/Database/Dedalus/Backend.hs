
module Database.Dedalus.Backend
  (
    Datalog(..)
  , Annotation(..)
  , Fact(..)
  , Rule(..)
  , Head(..)
  , Body(..)
  , Sign(..)
  , Backend(..)
  ) where

-- ---------------------------------------------------------------------

type Datalog = ([Fact], [Rule])

data Annotation = ANone | ANext | ASpecific !Integer
                deriving (Show,Eq)

data Fact = Fact String [String] Annotation
             deriving (Show,Eq)

data Rule = Rule Head [Body]
             deriving (Show,Eq)

data Head = Head String [String] Annotation
             deriving (Show,Eq)

data Body = Body String [String] Sign
             deriving (Show,Eq)

data Sign = Add | Negate
             deriving (Show,Eq)

-- ---------------------------------------------------------------------

-- f could be State Datalog
class Monad f => Backend f where
  
  -- the list of all facts, including derived rules 
  facts :: f [Fact]
  
{-
  -- the list of all facts for the given name 
  factsFor :: Name -> f [Fact]
  factsFor n = liftM (filter (\x -> atomName x == n)) facts 

  factsForId :: Int -> f [Fact]
  factsForId n = liftM (filter (\x -> atomId x == n)) facts 
-}

  -- the list of all rules
  rules :: f [Rule]

{-
  -- the list of all rules for a given table 
  rulesFor :: Name -> f [Rule]
  rulesFor n = liftM (filter (\r -> atomName (ruleHead r) == n)) rules
-}
  -- only memoize facts for the given table
  -- memo :: Name -> f ()

  -- hint to the backend that it should memoize derived facts for the given Name 
  memoAll :: f () 

{-
  -- returns the first set of variable bindings for which the assertion holds, 
  -- or Nothing if no facts unify with the given query
  query :: Atom Term -> f (Maybe Subst)
  query q = do 
    fs <- facts
    return $ join (find isJust (map (\f -> unifyAtom [] q f) fs))
-}

  -- add new facts and rules to knowledge base
  declare :: Datalog -> f () 

