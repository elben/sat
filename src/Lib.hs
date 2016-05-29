module Lib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Data.List (intercalate)

type Name = String

data Term = Var Name
          | Not Term
          | Or [Term]
          | And [Term]
  deriving (Show, Eq)

type Counter = Int

-- Get a fresh variable. Also updates the counter.
--
fresh :: State Counter Counter
fresh = do
  modify (+1)
  get

-- Variable environment.
type Env = M.Map Name Counter

-- Converting to CNF form
--
-- http://cs.jhu.edu/~jason/tutorials/convert-to-CNF
-- https://april.eecs.umich.edu/courses/eecs492_w10/wiki/images/6/6b/CNF_conversion.pdf
-- http://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form

-- Converts a Term into CNF format, if possible.
cnf :: Term -> Maybe Term
cnf (Var n) = Just $ Var n
cnf (Not term) =
  --      ~a  ==> ~a
  --    ~(~a) ==> a
  -- ~(a v b) ==> ~a ^ ~b
  -- ~(a ^ b) ==> ~a v ~b
  case term of
    Var n -> Just $ Not (Var n)
    Not t -> cnf t
    Or terms -> cnf $ And $ map Not terms
    And terms -> cnf $ Or $ map Not terms

-- (a ^ b) v c v d
-- ==>
-- ((a v c) ^ (b v c)) v d
-- ==>
-- (a v c v d) ^ (b v c v d)
--
cnf (Or terms) =
  let f = foldl (\b a -> b) [] terms
  in _
cnf (Or terms) =
  case terms of
    -- Base case: every term is a var
    [Var _, Var _] -> return $ Or terms


    -- (a ^ b ^ c) v d v rest...
    -- ==>
    -- (a v d) ^ (b v d) ^ (c v d) ^ rest...
    (And terms'):term:rest -> do
      terms'' <- sequence $ map cnf terms'
      let terms''' = map (\t -> (Or [t, term])) terms''
      return $ Or terms'''
    -- (Var p):(And terms'):rest -> _
cnf (And terms) = undefined

reduceOr :: Term -> Maybe[Term]
reduceOr terms@(Or [Var _, Var _]) = Just terms
reduceOr terms@(Or [Var _, Var _]) = Just terms


-- Emit DIMACS body format.
--
-- http://www.satcompetition.org/2009/format-benchmarks2009.html
--
-- TODO!!! Describe this.
emit :: Term -> StateT Env (State Counter) String
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

