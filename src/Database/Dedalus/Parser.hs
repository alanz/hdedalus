-- |Parse Dedalus statements and convert them to internal representation

module Database.Dedalus.Parser
    (
      parseDedalus
    , statements
    ) where

import Data.Char
import Data.Either
import Data.Hashable
import Data.Maybe
import Data.Text hiding (map,concatMap)
import Database.Datalog
import Database.Dedalus.Backend
import Database.Dedalus.Wrapper
-- import Text.Parse
import Text.ParserCombinators.Poly.State
import qualified Data.Map as Map
import Data.Map (Map)


-- This module based on example
-- https://github.com/kawu/tokenize/blob/master/Text/Regex/Parse.hs

{- 

Examples to parse

  p(A, B) :- e(A, B).
  q(A, B)@next :- e(A, B).

  e(1, 2)@10.

  --

  p_pos(A, B) :- p(A, B). 
  p_pos(A, B)@next :- p_pos(A, B), -p_neg(A, B).

  p(1,2)@101. 
  p(1,3)@102. 
  p_neg(1,2)@300.

  -- 

  seq(B)@next :- seq(A), successor(A,B), event(_). 
  seq(A)@next :- seq(A), -event(_). 
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

-- type P a = Parser Char a
type P a = Parser Text Env a


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
-- bodyP = Body <$> word <*> headParamsP 
bodyP = do
  sign <- optional signP
  name <- word
  params <- headParamsP
  case sign of
    Nothing -> return (Body name params Add)
    Just _  -> return (Body name params Negate)

signP :: P Sign
signP = do
  spaces
  char '-'
  return Negate

-- ---------------

-- TODO: skip preceding whitespace
matchWord :: String -> P String
matchWord s = mapM char s

char :: Char -> P Char
char c = satisfy (==c)

bracketP :: Char -> Char -> P a -> P a
bracketP beg end p = bracket (char beg) (char end) p

spaces  :: P ()
spaces  = do {many (satisfy isSpace); return ()}

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


parseDedalus :: String -> [Dedalus]
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


tt = parseDedalus "a(C)@1.b(A)@23.c(A,B)@100."

-- tb = tp ruleTP "p(A, B)@2 <- e(A, B)."
tb = tp ruleTP "p_pos(A, B)@next :- p_pos(A, B), -p_neg(A, B)."

pd = tp dedalusP
