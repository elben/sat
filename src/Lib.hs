module Lib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Data.List (intercalate)

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Test.QuickCheck

type Name = String

data Term = Var Name
          | Not Term
          | Or [Term]
          | And [Term]
  deriving Eq

type Counter = Int

-- Get a fresh variable. Also updates the counter.
--
fresh :: State Counter Counter
fresh = do
  modify (+1)
  get

-- Variable environment.
type Env = M.Map Name Counter

-- | Converts a Term into CNF format.
--
-- >>> cnf $ Var "a"
-- a
--
-- >>> cnf $ Not (Not (Var "a"))
-- a
--
-- ~(a v b)
-- ~a ^ ~b
-- >>> cnf $ Not (Or [Var "a", Var "b"])
-- (~a ^ ~b)
--
-- ~(a ^ b)
-- ~a v ~b
-- >>> cnf $ Not (And [Var "a", Var "b"])
-- (~a v ~b)
--
-- (a ^ b) ^ c
-- a ^ b ^ c
-- >>> cnf $ And [And [Var "a", Var "b"], Var "c"]
-- (a ^ b ^ c)
--
-- a v (b v c)
-- (a v b v c)
-- >>> cnf (Or [Var "a", Or [Var "b", Var "c"]])
-- (a v b v c)
--
-- (b ^ c) v a
-- (b v a) ^ (c v a)
-- >>> cnf (Or [And [Var "b", Var "c"], Var "a"])
-- ((b v a) ^ (c v a))
--
-- (b ^ c) v a v d
-- ((b v a) ^ (c v a)) v d
-- ((b v a) ^ (c v a)) v d
-- ((b v a) v d) ^ ((c v a) v d)
-- (b v a v d) ^ (c v a v d)
-- >>> cnf (Or [And [Var "b", Var "c"], Var "a", Var "d"])
-- ((b v a v d) ^ (c v a v d))
--
-- (a ^ (b v c)) v (d ^ e ^ f) v (g ^ h)
-- >>> cnf (Or [And [Var "a", Or [Var "b", Var "c"]], And [Var "d", Var "e", Var "f"], And [Var "g", Var "h"]])
-- ((g v d v a) ^ (h v d v a) ^ (g v e v a) ^ (h v e v a) ^ (g v f v a) ^ (h v f v a) ^ (g v d v b v c) ^ (h v d v b v c) ^ (g v e v b v c) ^ (h v e v b v c) ^ (g v f v b v c) ^ (h v f v b v c))
--
cnf :: Term -> Term
cnf (Var n) = Var n
cnf (Not term) =
  --      ~a  ==> ~a
  --    ~(~a) ==> a
  -- ~(a v b) ==> ~a ^ ~b
  -- ~(a ^ b) ==> ~a v ~b
  case term of
    Var n -> Not (Var n)
    Not t -> cnf t
    Or terms -> cnf $ And $ map Not terms -- De Morgan's
    And terms -> cnf $ Or $ map Not terms -- De Morgan's
cnf (And terms) =
  -- Because we're aiming for CNF, ands are easy to handle. We just evaluate the
  -- inner terms and flatten them if necessary.
  --
  -- a ^ b  => a ^ b
  -- a ^ (b ^ c) => a ^ b ^ c
  -- a ^ (b v c) => a ^ (b v c)
  -- a ^ !b => a ^ !b
  --
  let terms' = map cnf terms
  in flattenTerm $ And $ flattenAnd terms'
cnf (Or (And terms : q : rest)) =
  -- Use Distributive property to convert (a ^ b) v q v rest...
  -- ==> ((a v q) ^ (b v q)) v rest...
  --
  -- Then, call `cnf` again on this new thing, because it may have to distribute
  -- again.
  --
  -- Normalize each set of terms:
  let terms' = map cnf terms
      q' = cnf q
      rest' = map cnf rest
      terms'' = map (\t -> Or [t,q']) terms'
      -- ^ This interleaves @q'@ into the ^ terms.
      -- So that (a ^ b) v q v rest... ==> ((a v q) ^ (b v q)) v rest...
  in cnf $ Or (And terms'': rest') -- Call `cnf` again to distribute further.
cnf (Or (q : And terms : rest)) =
  -- Use commutative property to just switch terms and use above implementation.
  cnf (Or (And terms : q : rest))
cnf (Or terms) =
  -- Base case: there is no leading ^ term, or there is nothing left to
  -- distribute. So just flatten things if necessary.
  let terms' = map cnf terms
  in flattenTerm $ Or $ flattenOr terms'

-- | Flatten And using associative property.
--
-- y ^ (x ^ z) ^ (y ^ z) ^ (u v w)
-- y ^ x ^ z ^ y ^ z ^ (u v w)
-- >>> flattenAnd [Var "y", And [Var "x", Var "z"], And [Var "y", Var "z"], Or [Var "u", Var "w"]]
-- [y,x,z,y,z,(u v w)]
--
flattenAnd :: [Term] -> [Term]
flattenAnd [] = []
flattenAnd (And terms:rest) = terms ++ flattenAnd rest
flattenAnd (t:rest) = t : flattenAnd rest

-- | Flatten Or using associative property.
--
flattenOr :: [Term] -> [Term]
flattenOr [] = []
flattenOr (Or terms:rest) = terms ++ flattenOr rest
flattenOr (t:rest) = t : flattenOr rest

flattenTerm :: Term -> Term
flattenTerm (And [t]) = t
flattenTerm (Or [t]) = t
flattenTerm t = t

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

instance Show Term where
  show (Var v) = v
  show (Not t) = "~" ++ show t
  show (And terms) = "(" ++ intercalate " ^ " (map show terms) ++ ")"
  show (Or terms) = "(" ++ intercalate " v " (map show terms) ++ ")"
