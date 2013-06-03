
-- |Convert sugared Dedalus statements to standard Datalog

module Database.Dedalus.DeSugar
  (
    desugar
  , desugarRule
  , desugarFact
  , desugarQuery
  ) where

import Database.Dedalus.Backend
import qualified Data.Map as Map

-- ---------------------------------------------------------------------
-- Fact desugaring
--
--
-- Facts MUST have a specific time annotation, this is added as the
-- last value in the fact term
--
-- a(b,c)@3.
--    => a(b,c,3).
--
--
-- Rule desugaring
--
--
-- a) If no specific time annotation, it is implicit in all predicates
--
--   toggle(1) :- state(0).
--     => toggle(1, T) :- state(0, T).
--
--   toggle(0) :- state(1).
--     => toggle(0, T) :- state(1, T).
--
-- b) The @next annotation brings in a successor relation for indictive rules
--
--   state(X)@next :- toggle(X).
--     => state(X, S) :- toggle(X, T), succ(T, S).
--
-- c) The @async annotation brings in the choice operator
--
--   announce(X)@async :- toggle(X).
--     => announce(X, S) :- toggle(X, T), choice({X,T}, {S}).
--
--
-- succ(T,S) holds if S == T+1
--
-- choice({X, T}, {S}), indicates that for each pair of assignments to
--   variables {X, T}, a value S is non-deterministically chosen. In
--   practice S is the time value at the receiving node, after
--   communication.



-- ---------------------------------------------------------------------
-- Implicit
-- ========
-- "a(X,Y) :- z(X,A),z(Z,Y)." is parsed to
-- Rule
--   (Atom (C 0 (S "a")) [Var (V "X"),Var (V "Y")])
--   [ Pat (Atom (C 1 (S "z")) [Var (V "X"),Var (V "A")])
--   , Pat (Atom (C 1 (S "z")) [Var (V "Z"),Var (V "Y")])
--   ]
--   TSImplicit
--
-- becomes
-- "a(X,Y,T) :- z(X,A,T),z(Z,Y,T)." parses to
-- Rule
--  (Atom (C 0 (S "a")) [Var (V "X"),Var (V "Y"),Var (V "T")])
--  [ Pat (Atom (C 1 (S "z")) [Var (V "X"),Var (V "A"),Var (V "T")])
--  , Pat (Atom (C 1 (S "z")) [Var (V "Z"),Var (V "Y"),Var (V "T")])
--  ]
--  TSImplicit

desugarRule :: Rule -> Rule
desugarRule (Rule lhs body (TSImplicit)) = Rule lhs' body' TSImplicit
  where
    lhs' = appendT lhs
    body' = map appendTToPat body

-- ---------------------------------------------------------------------
-- @next
-- =====
-- "state(X)@next :- toggle(X)." is parsed to
-- Rule
--   (Atom (C 0 (S "state")) [Var (V "X")])
--   [ Pat (Atom (C 1 (S "toggle")) [Var (V "X")])
--   ]
--   TSNext
--
-- becomes
-- "state(X, S) :- toggle(X, T), succ(T, S)." is parsed to
-- Rule
--   (Atom (C 0 (S "state")) [Var (V "X"),Var (V "S")])
--   [ Pat (Atom (C 1 (S "toggle")) [Var (V "X"),Var (V "T")])
--   , Pat (Atom (C 2 (S "succ")) [Var (V "T"),Var (V "S")])]
--   TSImplicit

desugarRule (Rule lhs body TSNext) = Rule lhs' body' TSImplicit
  where
    lhs' = appendS lhs
    body' = (map appendTToPat body) ++ [succST]

-- ---------------------------------------------------------------------
-- @async
-- ======
-- "announce(X)@async :- toggle(X)." is parsed to
-- Rule
--   (Atom (C 0 (S "announce")) [Var (V "X")])
--   [ Pat (Atom (C 1 (S "toggle")) [Var (V "X")])
--   ]
--   TSAsync
--
--  according to theroy should be
-- "announce(X, S) :- toggle(X, T), choice({X,T}, {S})."
--
-- but in practice becomes
-- "announce(X) :- toggle(X, T)."
-- Rule
--   (Atom (C 0 (S "announce")) [Var (V "X")])
--   [ Pat (Atom (C 1 (S "toggle")) [Var (V "X"),Var (V "T")])
--   ]
--   TSImplicit

desugarRule (Rule lhs body (TSAsync)) = Rule lhs' body' TSImplicit
  where
    lhs' = lhs
    body' = map appendTToPat body

-- ---------------------------------------------------------------------

desugarRule drule@(Rule _lhs _body (TS _specific))
  = error $ "desugar attempted of specific time rule:" ++ (show drule)

-- ---------------------------------------------------------------------

-- Helpers
appendS, appendT :: Atom Term -> Atom Term
appendS (Atom c args)   = Atom c (args++[tsVarS])
appendT (Atom c args)   = Atom c (args++[tsVarT])

appendC :: Atom t -> t -> Atom t
appendC (Atom c args) v = Atom c (args++[v])

appendTToPat :: Pat -> Pat
appendTToPat (Not p) = Not (appendT p)
appendTToPat (Pat p) = Pat (appendT p)

succST :: Pat
succST = Pat (Atom (C (-1) (S "succ")) [tsVarT,tsVarS])

tsVarT,tsVarS :: Term
tsVarT = Var (V ".T.")
tsVarS = Var (V ".S.")

-- ---------------------------------------------------------------------

desugarQuery :: Atom Term -> Atom Term
desugarQuery q = appendS q

desugarFact :: Fact -> Fact
desugarFact (Fact c ts@(TS timestamp))
  = Fact (appendC c (C (-1) (I timestamp))) ts

-- ---------------------------------------------------------------------

desugar :: Datalog -> Datalog
desugar (DL factMap ruleMap qs) = (DL factMap' ruleMap' qs)
  where
    factMap' = Map.fromList $ map desugarFactList $ Map.toList factMap
    ruleMap' = Map.fromList $ map desugarRuleList $ Map.toList ruleMap

    desugarFactList ((name,arity),facts)
      = ((name,arity+1), map desugarFact facts)

    desugarRuleList ((name,arity),rules)
      = ((name,arity+1), map desugarRule rules)

