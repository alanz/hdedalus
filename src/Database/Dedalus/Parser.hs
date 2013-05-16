-- |Parse Dedalus statements and convert them to internal representation

module Database.Dedalus.Parser
    (
    parseDedalus
    ) where

import Data.Hashable
import Data.Maybe
import Data.Text hiding (map,concatMap)
import Database.Datalog
import Database.Dedalus.Wrapper
import Text.ParserCombinators.Poly
import Text.Parse

-- This module based on example
-- https://github.com/kawu/tokenize/blob/master/Text/Regex/Parse.hs

{- 

Examples to parse

  p(A, B) <- e(A, B);
  q(A, B)@next <- e(A, B);

  e(1, 2)@10;

  --

  p_pos(A, B) <- p(A, B); 
  p_pos(A, B)@next <- p_pos(A, B), -p_neg(A, B);

  p(1,2)@101; 
  p(1,3)@102; 
  p_neg(1,2)@300;

  -- 

  seq(B)@next <- seq(A), successor(A,B), event(_); 
  seq(A)@next <- seq(A), -event(_); 
  seq(0);

-}


-- ---------------------------------------------------------------------

data Annotation = ANone | ANext | ASpecific !Integer
                deriving (Show)
data Dedalus = FactList [Dedalus]
             | Fact String [String] Annotation
             | Rule Head [Body]
             deriving (Show)

data Head = Head String [String] Annotation
             deriving (Show)

data Body = BodyList [Body]
          | Body String [String] -- Sign
             deriving (Show)

data Sign = Add | Negate
             deriving (Show)

-- ---------------------------------------------------------------------

type P a = Parser Char a

dedalusP :: P Dedalus
-- dedalusP = FactList <$> factP `sepBy` char ';'
dedalusP = FactList <$> many factTP

factTP :: P Dedalus
factTP = do
  f <- factP
  char ';'
  return f

factP :: P Dedalus
factP = Fact <$> word <*> headParamsP  <*> annotationSpecificP

headParamsP :: P [String]
headParamsP = bracketP '(' ')' paramsP

paramsP :: P [String]
paramsP = word `sepBy` char ','

annotationSpecificP :: P Annotation
annotationSpecificP = do
  char '@'
  v <- parseDec
  return (ASpecific v)

anyAnnotationP :: P Annotation
anyAnnotationP = do
  ma <- optional $ oneOf [annotationSpecificP, annotationWordP]
  case ma of
    Nothing ->  return ANone
    Just a -> return a

annotationWordP :: P Annotation
annotationWordP = do
  matchWord "@next"
  return ANext

-- ---------------

ruleP :: P Dedalus
ruleP = Rule <$> ruleHeadP <*> many bodyP

ruleHeadP :: P Head
ruleHeadP = do
  h <- headP
  _ <-matchWord "<-"
  return h

headP :: P Head
headP = Head <$> word <*> headParamsP <*> anyAnnotationP


bodyP :: P Body
bodyP = Body <$> word <*> headParamsP 

-- ---------------

matchWord :: String -> P String
matchWord s = mapM char s

char :: Char -> P Char
char c = satisfy (==c)

bracketP :: Char -> Char -> P a -> P a
bracketP beg end p = bracket (char beg) (char end) p


parseDedalus :: String -> Dedalus
parseDedalus xs = case runParser dedalusP xs of
    (Left msg, _) -> error $ "[parseDedalus] " ++ msg
    (Right x, []) -> x
    (_, ys) -> error $
        "[parseDedalus] parsing: " ++ xs ++ "\n" ++
        "[parseDedalus] tokens not consumed: " ++ ys


-- ---------------------------------------------------------------------


tp parser xs = case runParser parser xs of
    (Left msg, _) -> error $ "[parser] " ++ msg
    (Right x, []) -> x
    (_, ys) -> error $
        "[parser] parsing: " ++ xs ++ "\n" ++
        "[parser] tokens not consumed: " ++ ys


tt = parseDedalus "a(C)@1;b(A)@23;c(A,B)@100;"

tb = tp ruleP "p(A, B) <- e(A, B);"
