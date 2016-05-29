module Lib
    ( someFunc
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Name = String

data Term = T
          | F
          | Var Name
          | Not Term
          | Or Term Term
          | And Term Term

-- newtype Counter = Counter { getCounter :: Int }

type Counter = Int

initCounter :: Counter
initCounter = 0

fresh :: State Counter Counter
fresh = do
  modify (+1)
  get

-- Variable environment.
type Env = M.Map Name Counter

-- http://www.satcompetition.org/2004/format-benchmarks2004.html
--
-- TODO need to add Reader for var name to integer
toDimacs :: Term -> ReaderT Env (State Counter) String
toDimacs T = return "true"
toDimacs F = return "false"
toDimacs (Var n) = do
  env <- ask
  let m = M.lookup n (traceShowId env)
  s <- case m of
         Just i ->
           return i
         Nothing -> do
           i <- lift fresh
           lift (put i)
           env <- ask
           return i
  return $ show s
toDimacs (Not t) = do
  ts <- toDimacs t
  return $ "-" ++ ts
toDimacs (Or t1 t2) = do
  t1s <- toDimacs t1
  t2s <- toDimacs t2
  return $ t1s ++ " " ++ t2s
toDimacs (And t1 t2) = do
  t1s <- toDimacs t1
  t2s <- toDimacs t2
  return $ t1s ++ "\n" ++ t2s

run :: ReaderT Env (State Counter) a -> a
run s = evalState (runReaderT s M.empty) initCounter

-- putStrLn $ run (toDimacs (And (Or (Var "a") (Not (Var "a"))) (Var "b")))
