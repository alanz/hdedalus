{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Parse Dedalus statements and convert them to internal representation
module Database.Dedalus.Parser (
  P,
  Env(..),
  initialEnv,
  statements,
  queryP,
  run,
  tp
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Map as M
import Data.Map (Map)

import Control.Applicative ((<$>),(<*>))
import Control.Monad ( forM_, when )

import Data.Either (partitionEithers)

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Token (natural)

import Database.Dedalus.Backend

type P = Parsec Text Env

instance (Monad m) => Stream Text m Char where
    uncons = return . T.uncons

data Env = Env
    { envConMap :: Map Literal Con
    , envNextFree :: !Id
    } deriving (Show)

initialEnv :: Env
initialEnv = Env { envNextFree = 0, envConMap = M.empty }

fresh :: P Int
fresh = do
    env <- getState
    let free = envNextFree env
    putState $ env { envNextFree = free + 1 }
    return free

mkCon :: Literal -> P Con
mkCon k = do
    m <- envConMap <$> getState
    case M.lookup k m of
        Just c -> return c
        Nothing -> do
            i <- fresh
            let result = C i k
            modifyState $ \env -> env { envConMap = M.insert k result m }
            return result

-- parser

word :: P String
word = many1 letter

var :: P Var
var = do
    l <- upper <?> "variable"
    ls <- many letter
    return $ V (l:ls)

conS :: P Con
conS = do
    u <- lower <?> "constructor"
    us <- many letter
    mkCon (S $ u:us)

conI :: P Con
conI = do
    i <- number
    mkCon (I i)

con :: P Con
con = conS
  <|> conI

neg :: P ()
neg = (string "\\+" <|> string "~") >> return ()

term :: P Term
term =  Var <$> var
    <|> Con <$> con


number :: P Integer
number = do { ds <- many1 digit; return (read ds) } <?> "number"

turnstile :: P ()
turnstile = string ":-" >> return ()

period :: P ()
period = char '.' >> return ()

comma :: P ()
comma = char ',' >> return ()

open :: P ()
open = (char '(' >> spaces >> return ()) <?> "("

close :: P ()
close = (spaces >> char ')' >> return ()) <?> ")"

atSign :: P ()
atSign = (spaces >> char '@' >> return ()) <?> "@"

betweenParens :: P a -> P a
betweenParens = between open close

spaced :: P a -> P a
spaced = between spaces spaces

atom :: P a -> P (Atom a)
atom t = do
    p <- con
    Atom p <$> (betweenParens (t `sepBy` spaced comma) <|> return [])
  <?> "atom"

pat :: P Pat
pat = do { neg; spaces; Not <$> atom term } <|> Pat <$> atom term

explicitTime :: P TimeSuffix
explicitTime = try $ do { atSign; TS <$> number }

asyncTime :: P TimeSuffix
asyncTime = try $ do { string "@async"; return TSAsync}

nextTime :: P TimeSuffix
nextTime = try $ do {string "@next"; return TSNext}

anyTime :: P TimeSuffix
anyTime = nextTime <|> asyncTime <|> explicitTime
   

fact :: P Fact
fact = Fact <$> atom con <*> explicitTime
  <?> "fact"

queryP :: P (Atom Term)
queryP = spaced (atom term)
  <?> "query"

rule :: P Rule
rule = do
    h <- atom term
    ts <- option TSImplicit anyTime
    spaced turnstile <?> ":-"
    -- body <- pat `sepBy` many1 space
    body <- pat `sepBy` spaced comma
    safe $ Rule h body ts
  <?> "rule"

safe :: Rule -> P Rule
safe r@(Rule h body _) = do
        forM_ headVars $ \v ->
            when (v `notElem` bodyVars) $ do
                unexpected $ "variable " ++ show (varName v) ++ " appears in head, but not occur positively in body"
        forM_ subVars $ \v ->
            when (v `notElem` bodyVars) $ do
                unexpected $ "variable " ++ show (varName v) ++ " appears in a negated subgoal, but not occur positively in body"
        return r
    where
        headVars, bodyVars, subVars :: [Var]
        headVars = [ v | Var v <- atomArgs h ]
        bodyVars = concatMap posVars body
        subVars  = concatMap negVars body

        posVars, negVars :: Pat -> [Var]
        posVars (Pat a) = [ v | Var v <- atomArgs a ]
        posVars (Not _) = []
        negVars (Not a) = [ v | Var v <- atomArgs a ]
        negVars (Pat _) = []

statement :: P (Either Fact Rule)
statement = try (Left <$> fact)
            <|> (Right <$> rule)

lineSep :: P ()
lineSep = spaced period <?> "."

statements :: P ([Fact],[Rule])
statements = do
    spaces
    result <- partitionEithers <$> statement `sepEndBy` lineSep
    eof
    return result

run :: String -> Either ParseError ([Fact],[Rule])
run = runParser statements initialEnv "-" . T.pack

-- ---------------------------------------------------------------------
-- Test stuff

tp :: Parsec Text Env a -> String -> a
tp parser xs = case runParser parser initialEnv "-" (T.pack xs) of
    Left msg -> error $ "[parser] " ++ (show msg)
    Right x -> x

testCase :: Either ParseError ([Fact], [Rule])
testCase = run "x(B,C) :- a(B,C), a(B,c)."

