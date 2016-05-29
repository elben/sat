module Lib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Data.List (intercalate)

type Name = String

data Term = T
          | F
          | Var Name
          | Not Term
          | Or [Term]
          | And [Term]
  deriving (Show, Eq)

type Counter = Int

fresh :: State Counter Counter
fresh = do
  modify (+1)
  get

-- Variable environment.
type Env = M.Map Name Counter

-- Evaulate terms and emit DIMACS body format.
--
-- http://www.satcompetition.org/2009/format-benchmarks2009.html
--
emit :: Term -> StateT Env (State Counter) String
emit T = return "true"
emit F = return "false"
emit (Var n) = do
  env <- get
  let m = M.lookup n env
  s <- case m of
         Just i ->
           return i
         Nothing -> do
           i <- lift fresh
           lift (put i)
           modify (M.insert n i)
           return i
  return $ show s
emit (Not t) = do
  ts <- emit t
  return $ "-" ++ ts
emit (Or terms) = do
  s <- mapM emit terms
  return $ unwords s
emit (And terms) = do
  s <- mapM emit terms
  return $ intercalate " 0\n" s ++ " 0"

-- Returns number of conjunctions in terms.
-- https://en.wikipedia.org/wiki/Conjunctive_normal_form
numConjunctions :: Term -> Int
numConjunctions (And terms) = length terms
numConjunctions _ = 0

dimacsHeaders :: Int -> Int -> String
dimacsHeaders terms conjs = "p cnf " ++ show terms ++ " " ++ show conjs

-- Take term and emit a valid DIMACS string content.
--
emitDimacs :: Term -> String
emitDimacs term = do
  let stm = emit term
  let (str, env) = evalState (runStateT stm M.empty) 0
  let numVars = M.size env
  let numConjs = numConjunctions term
  dimacsHeaders numVars numConjs ++ "\n" ++ str

-- run :: StateT Env (State Counter) a -> a
-- run s = evalState (evalStateT s M.empty) 0
-- putStrLn $ run (emit (And [(Or [(Var "a"), (Not (Var "b"))]), (Var "a")]))
-- putStrLn $ emitDimacs (And [(Or [(Var "a"), (Not (Var "b"))]), (Var "a")])

