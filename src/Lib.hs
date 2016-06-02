module Lib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
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
-- ~(y ^ (i v j))
-- ~y v (~i ^ ~j)
-- (~y v ~i) ^ (~y v ~j)
-- >>> cnf $ Not (And [Var "y",Or [Var "i",Var "j"]])
-- ((~i v ~y) ^ (~j v ~y))
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
-- ((g v a v d) ^ (h v a v d) ^ (g v a v e) ^ (h v a v e) ^ (g v a v f) ^ (h v a v f) ^ (g v b v c v d) ^ (h v b v c v d) ^ (g v b v c v e) ^ (h v b v c v e) ^ (g v b v c v f) ^ (h v b v c v f))
--
-- TODO infinite loop. because we only look at first two elements for ands
-- Instead, we need to iteratively go through each pair, and if one is an And,
-- fix it. Then go throguh the list agian. So in this case below, we test a and
-- b for Ands, then b and (c ^ d). Seeing that, we trigger the expansion. Then
-- we cnf the entire list again.
--
-- When we see an ^, this infects the entire list. Every element in the Or will
-- be touched. This seems more like a zipper. Or us n^2 iteration.
--
-- a v b v (c ^ d) v e v f
-- a v ((b v c) ^ (b v d)) v e v f
-- ((a b c) ^ (a b d)) e f
-- ((a b c e) ^ (a b d e)) f
-- (a b c e f) ^ (a b d e f)
--
-- >>> cnf $ Or [Var "a", Var "b", And [Var "c", Var "d"]]
-- ((c v b v a) ^ (d v b v a))
--
-- >>> cnf $ Not (And [Not (Var "m"),Not (Var "s")])
-- (m v s)
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
  in erase $ flatten $ And terms'
cnf (Or terms) =
  let terms' = map cnf terms
  in
    if isCnf (Or terms')
    then erase $ flatten $ Or terms'
    else
      let terms'' = distributeOnce terms'
      in cnf $ erase $ flatten (Or terms'')

-- | Run through the list of or-ed terms, distributing terms into any found
-- and-ed terms. The returned list of terms is not guaranteed to be in CNF. Will
-- need to call `isCnf` to check whether or not another pass into distributeOnce
-- is required.
--
-- Really, this looks like a zippers problem. But the zipper tool would need to
-- able to delete-and-replace a node to substitue.
--
-- a v b v (c ^ d) v e v (f ^ g)
-- >>> distributeOnce [Var "a", Var "b", And [Var "c", Var "d"], Var "e", And [Var "f", Var "g"]]
-- [a,((c v b) ^ (d v b)),((f v e) ^ (g v e))]
--
-- Continuing the above.
-- >>> distributeOnce $ distributeOnce [Var "a", Var "b", And [Var "c", Var "d"], Var "e", And [Var "f", Var "g"]]
-- [((c v b v a) ^ (d v b v a)),((f v e) ^ (g v e))]
--
-- Continuing the above.
-- >>> distributeOnce $ distributeOnce $ distributeOnce [Var "a", Var "b", And [Var "c", Var "d"], Var "e", And [Var "f", Var "g"]]
-- [((c v b v a v ((f v e) ^ (g v e))) ^ (d v b v a v ((f v e) ^ (g v e))))]
--
distributeOnce :: [Term] -> [Term]
distributeOnce [] = []
distributeOnce [t] = [t]
distributeOnce (And ands : t : rest) =
  let ands' = map cnf ands
      t' = cnf t
      ands'' = map (\a -> flatten (Or [a,t'])) ands'
      rest' = distributeOnce rest
      andTerm = if length ands'' == 1 then head ands'' else And ands''
  in andTerm : rest'
distributeOnce (t : And ands : rest) = distributeOnce (And ands : t : rest)
distributeOnce (t1 : rest) = t1 : distributeOnce rest

topLevelAnds :: [Term] -> Bool
topLevelAnds [] = False
topLevelAnds (And _ : _) = True
topLevelAnds (_ : rest) = topLevelAnds rest

-- | Erase useless AND or OR wrapper.
--
erase :: Term -> Term
erase (And [t]) = t
erase (Or [t]) = t
erase t = t

-- | Flatten ANDs and ORs using associative property.
--
-- y ^ (x ^ z) ^ (y ^ z) ^ (u v w)
-- >>> flatten (And [Var "y", And [Var "x", Var "z"], And [Var "y", Var "z"], Or [Var "u", Var "w"]])
-- (y ^ x ^ z ^ y ^ z ^ (u v w))
--
-- y v (x v z) v (y ^ z) v (u v w)
-- >>> flatten (Or [Var "y", Or [Var "x", Var "z"], And [Var "y", Var "z"], Or [Var "u", Var "w"]])
-- (y v x v z v (y ^ z) v u v w)
--
flatten :: Term -> Term
flatten (And []) = And []
flatten (And (And ands:rest)) = And (ands ++ getTerms (flatten (And rest)))
flatten (And (t:rest)) = And (t : getTerms (flatten (And rest)))
flatten (Or []) = Or []
flatten (Or (Or ors:rest)) = Or (ors ++ getTerms (flatten (Or rest)))
flatten (Or (t:rest)) = Or (t : getTerms (flatten (Or rest)))
flatten t = t

getTerms :: Term -> [Term]
getTerms (And terms) = terms
getTerms (Or terms) = terms
getTerms _ = error "Not supported"

-- | Evaluate term with given mapping.
--
-- >>> eval (M.fromList [("a", True),("b",False)]) (And [Var "a", Not (Var "b")])
-- Just True
--
-- >>> eval (M.fromList [("a", True),("b",False)]) (And [Var "a", Var "b"])
-- Just False
--
-- >>> eval (M.fromList [("a", True),("b",False)]) (And [Var "a", Or [Var "a", Var "b"]])
-- Just True
--
eval :: M.Map Name Bool -> Term -> Maybe Bool
eval m (Var n) = M.lookup n m
eval m (Not t) = do
  b <- eval m t
  return $ not b
eval m (And terms) = do
  bools <- mapM (eval m) terms
  return $ and bools
eval m (Or terms) = do
  bools <- mapM (eval m) terms
  return $ or bools

-- | Find all variable names in term.
--
-- >>> vars (And [Or [Not (Var "a"), Var "a", Var "b"], Var "c", Not (Not (Not (Var "d")))])
-- fromList ["a","b","c","d"]
--
vars :: Term -> Set.Set Name
vars (Var n) = Set.singleton n
vars (Not t) = vars t
vars (And terms) = Set.unions (map vars terms)
vars (Or terms) = Set.unions (map vars terms)

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

-- | Checks if Term is in CNF form.
--
-- >>> isCnf (Var "a")
-- True
--
-- >>> isCnf (Not (Var "a"))
-- True
--
-- a ^ b ^ (c v d v ~a v ~b)
-- >>> isCnf (And [Var "a",Var "b",Or [Var "c",Var "d",Not (Var "a"), Not (Var "b")]])
-- True
--
-- ~y v (~i ^ ~i)
-- >>> isCnf (Or [Not (Var "y"),And [Not (Var "i"),Not (Var "i")]])
-- False
--
-- a ^ b ^ (c v d v ~(a v b))
-- >>> isCnf (And [Var "a",Var "b",Or [Var "c",Var "d",Not (Or [Var "a", Var "b"])]])
-- False
--
-- (a v ~b) ^ (c v (d ^ e))
-- >>> isCnf (And [Or [Var "a", Not (Var "b")], Or [Var "c", And [Var "d", Var "e"]]])
-- False
--
isCnf :: Term -> Bool
isCnf (Var _) = True
isCnf (Not (Var _)) = True
isCnf (Not _) = False
isCnf (And terms) = all noAndTerms terms
isCnf (Or terms) = all noAndTerms terms

-- | Checks that there are no And terms in the given term.
--
noAndTerms :: Term -> Bool
noAndTerms (Var _) = True
noAndTerms (Not (Var _)) = True
noAndTerms (Not _) = False
noAndTerms (And _) = False
noAndTerms (Or terms) = all noAndTerms terms
