{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Parse Dedalus statements and convert them to internal representation

module Database.Dedalus.Parser
    (
      statements
    , Env(..)
    , initialEnv
    , P
    ) where

import Control.Applicative ((<$>),(<*>))
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Text hiding (map,concatMap)
import Database.Dedalus.Backend
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim hiding (State)
import qualified Data.Map as Map
import qualified Data.Text as T



-- This module based on example
-- https://github.com/kawu/tokenize/blob/master/Text/Regex/Parse.hs

{- 

Examples to parse

  p(A, B) :- e(A, B).
  q(A, B)@next :- e(A, B).

  e(1, 2)@10.

  --

  p_pos(A, B) :- p(A, B). 
  p_pos(A, B)@next :- p_pos(A, B), !p_neg(A, B).

  p(1,2)@101. 
  p(1,3)@102. 
  p_neg(1,2)@300.

  -- 

  seq(B)@next :- seq(A), successor(A,B), event(_). 
  seq(A)@next :- seq(A), !event(_). 
  seq(0).

-}


-- ---------------------------------------------------------------------

data Dedalus = F Fact
             | R Rule
             deriving (Show)

-- ---------------------------------------------------------------------

type Con = String
type Id = Integer

data Env = Env 
    { envConMap :: Map String Con
    , envNextFree :: !Id
    } deriving (Show)

initialEnv :: Env
initialEnv = Env { envNextFree = 0, envConMap = Map.empty } 

-- type P a = Parser Char a
type P a = Parsec Text Env a

instance (Monad m) => Stream Text m Char where
    uncons = return . T.uncons


statements :: P ([Fact],[Rule])
statements = do 
    spaces
    result <- (many statement) 
    -- lineSep
    -- eof
    return $ partitionEithers result


statement :: P (Either Fact Rule)
statement = (Left <$> factTP)
        <|> (Right <$> ruleTP) 

dedalusP :: P [Dedalus]
dedalusP = many (F <$> factTP <|> R <$> ruleTP)

factTP :: P Fact
factTP = do
  f <- factP
  char '.'
  return f

factP :: P Fact
factP = Fact <$> word <*> headParamsP  <*> annotationSpecificP

headParamsP :: P [String]
headParamsP = betweenParens paramsP

paramsP :: P [String]
paramsP = word `sepBy` char ','

annotationSpecificP :: P Annotation
annotationSpecificP = do
  char '@'
  v <- many1 digit
  return (ASpecific $ read v)

anyAnnotationP :: P Annotation
anyAnnotationP = option ANone (choice [annotationSpecificP, annotationWordP])

annotationWordP :: P Annotation
annotationWordP = do
  matchWord "@next"
  return ANext

-- ---------------

ruleTP :: P Rule
ruleTP = do
  r <- ruleP
  char '.'
  return r

ruleP :: P Rule
ruleP = Rule <$> ruleHeadP <*> bodyP `sepBy` char ','

ruleHeadP :: P Head
ruleHeadP = do
  h <- headP
  s <- word
  if (s /= ":-")
    then fail $ "missing <-, got [" ++ s ++ "]"
    else return h
  -- _ <-matchWord "<-"
  -- return h

headP :: P Head
headP = Head <$> word <*> headParamsP <*> anyAnnotationP

bodyP :: P Body
bodyP = do
  sign <- option Negate signP
  name <- word
  params <- headParamsP
  return (Body name params sign)

signP :: P Sign
signP = do
  spaces
  char '!'
  return Negate

-- ---------------

-- TODO: skip preceding whitespace
matchWord :: String -> P String
matchWord s = mapM char s

word :: P String
word = many1 letter

open :: P ()
open = (char '(' >> spaces >> return ()) <?> "("

close :: P ()
close = (spaces >> char ')' >> return ()) <?> ")"

betweenParens :: P a -> P a
betweenParens p = between open close p

lineSep :: P ()
lineSep = spaced period -- <?> "."

spaced :: P a -> P a
spaced p = do
  spaces
  r <- p
  spaces
  return r

period :: P ()
period = char '.' >> return ()

{-
parseDedalus :: String -> [Dedalus]
parseDedalus xs = case runParser dedalusP xs of
    (Left msg, _) -> error $ "[parseDedalus] " ++ msg
    (Right x, []) -> x
    (_, ys) -> error $
        "[parseDedalus] parsing: " ++ xs ++ "\n" ++
        "[parseDedalus] tokens not consumed: " ++ ys
-}

run :: String -> Either ParseError ([Fact],[Rule])
run = runParser statements initialEnv "-" . T.pack

-- ---------------------------------------------------------------------


tp parser xs = case runParser parser initialEnv "-" (T.pack xs) of
    Left msg -> error $ "[parser] " ++ (show msg)
    Right x -> x

-- tt = parseDedalus "a(C)@1.b(A)@23.c(A,B)@100."

-- tb = tp ruleTP "p(A, B)@2 <- e(A, B)."
tb = tp ruleTP "p_pos(A, B)@next :- p_pos(A, B), -p_neg(A, B)."

pd = tp dedalusP

t = run "x(B,C) :- a(B,c)."

