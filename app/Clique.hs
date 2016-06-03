module Clique where

import Lib
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.Monad (liftM)

-- https://en.wikipedia.org/wiki/Clique_(graph_theory)
-- http://blog.computationalcomplexity.org/2006/12/reductions-to-sat.html

-- | Takes a list of edge pairs and returns the correponding SAT term.
--
--   1---2---3
--
-- >>> buildGraph 3 2 [(1,2),(2,3)]
-- "((y[1][1] v y[2][1] v y[3][1]) ^ (y[1][2] v y[2][2] v y[3][2]) ^ (~y[1][1] v ~y[2][1]) ^ (~y[1][1] v ~y[3][1]) ^ (~y[2][1] v ~y[3][1]) ^ (~y[1][2] v ~y[2][2]) ^ (~y[1][2] v ~y[3][2]) ^ (~y[2][2] v ~y[3][2]) ^ (~y[1][1] v ~y[3][2]) ^ (~y[1][2] v ~y[3][1]))"
--
--
--       3----4
--      / \   |
--     2---5--6
--    /
--   1
--
-- x>> buildGraph 6 3 [(1,2), (2,3), (2,5), (3,4), (3,5), (4,6), (5,6)]
-- Not really displayable.
--
buildGraph :: Int -> Int -> [(Int,Int)] -> Term
buildGraph n k edges = cnf $
  And [nodeInEveryPosition n k,
       oneNodePerCliquePosition n k,
       edgeDefinition n k edges]

-- | Builds a Var term based on the node position and the clique position.
--
-- Node 1 in the 4th position of the clique.
-- >>> var 1 4
-- Var "y[1][4]"
--
var :: Int -> Int -> Term
var i r = Var ("y[" ++ show i ++ "][" ++ show r ++ "]")

-- | Proposition that there exists a node in the clique for every position in
-- the clique. Takes @n@, the number of nodes, and @k@, the size of the clique.
--
-- >>> display $ nodeInEveryPosition 3 2
-- "((y[1][1] v y[2][1] v y[3][1]) ^ (y[1][2] v y[2][2] v y[3][2]))"
--
nodeInEveryPosition :: Int -> Int -> Term
nodeInEveryPosition n k = And
  [Or [var i r
       | i <- [1..n]] -- For each node i
   | r <- [1..k]] -- For each position in the clique

-- | Proposition that there is only one node per clique position.
--
-- >>> display $ oneNodePerCliquePosition 3 2
-- "((~(y[1][1] ^ y[2][1]) ^ ~(y[1][1] ^ y[3][1]) ^ ~(y[2][1] ^ y[3][1])) ^ (~(y[1][2] ^ y[2][2]) ^ ~(y[1][2] ^ y[3][2]) ^ ~(y[2][2] ^ y[3][2])))"
--
oneNodePerCliquePosition :: Int -> Int -> Term
oneNodePerCliquePosition n k = And
  [And [Not (And [var i r, var j r])
        | i <- [1..n], -- For each node i
          j <- [1..n], -- For each node j
          i /= j,      -- Where i != j
          i < j]       -- With undirected edges
   | r <- [1..k]] -- For each position in the clique.

-- | Proposition stating that if there is no edge between node i and j, then i
-- and j cannot be in the clique.
--
-- Example: in Graph B below, 1 and 3 is not connected. Thus, we test that (for
-- clique of size 3):
--
--   (node 1 position 1) and (node 2 position 2) is not in the clique
--   (node 1 position 1) and (node 2 position 3) is not in the clique
--   (node 1 position 2) and (node 2 position 1) is not in the clique
--   (node 1 position 2) and (node 2 position 3) is not in the clique
--
-- Notice how we check every possible combination of the clique position.
--
--   1---2---3     Graph A
--
-- >>> display $ edgeDefinition 3 2 [(1,2),(2,3)]
-- "(~(y[1][1] ^ y[3][2]) ^ ~(y[1][2] ^ y[3][1]))"
--
--
--   1---2---3     Graph B
--    \ /
--     4
--
-- >>> display $ edgeDefinition 4 3 [(1,2),(1,4),(2,3),(2,4)]
-- "(~(y[1][1] ^ y[3][2]) ^ ~(y[1][2] ^ y[3][1]) ^ ~(y[3][1] ^ y[4][2]) ^ ~(y[3][2] ^ y[4][1]) ^ ~(y[1][1] ^ y[3][3]) ^ ~(y[1][3] ^ y[3][1]) ^ ~(y[3][1] ^ y[4][3]) ^ ~(y[3][3] ^ y[4][1]) ^ ~(y[1][2] ^ y[3][3]) ^ ~(y[1][3] ^ y[3][2]) ^ ~(y[3][2] ^ y[4][3]) ^ ~(y[3][3] ^ y[4][2]))
--
edgeDefinition :: Int -> Int -> [(Int,Int)] -> Term
edgeDefinition n k edges = flatten $ And
  [And [Not (And [var a r, var b s]), Not (And [var a s, var b r])] | (r,s) <- pairsInCliques k, (a,b) <- inverseEdges n edges
      ]

-- | Return inverse edges.
--
--       3----4
--      / \   |
--     2---5--6
--    /
--   1
--
-- >>> inverseEdges 6 [(1,2), (2,3), (2,5), (3,4), (3,5), (4,6), (5,6)]
-- [(1,3),(1,4),(1,5),(1,6),(2,4),(2,6),(3,6),(4,5)]
--
inverseEdges :: Int -> [(Int,Int)] -> [(Int,Int)]
inverseEdges n edges =
  -- Maps each node to all the nodes it's attached to.
  --
  -- So the above example, we would have a mapping:
  --
  --   1 -> {2}
  --   2 -> {1, 3, 5}
  --   ...
  --
  let mappings = foldl'
                   (\m (a,b) ->
                     -- Add a -> b, b -> a. If key exist, just add to set. If
                     -- doesn't exist, create new singleton set.
                     let m'  = if M.member a m
                               then M.adjust (Set.insert b) a m
                               else M.insert a (Set.singleton b) m
                         m'' = if M.member b m'
                               then M.adjust (Set.insert a) b m'
                               else M.insert b (Set.singleton a) m'
                     in m'')
                   M.empty edges
  in [(i,j) | i <- M.keys mappings, -- For each node i
              j <- [1..n],          -- For each node j
              i /= j,               -- Where i != j
              i < j,                -- And for undirected edges
              -- Check if j in the set of i's edges. If not, then add it to this
              -- list.
              fromJust (liftM (not . Set.member j) (M.lookup i mappings))]

-- | Builds pairs of clique positions.
--
-- >>> pairsInCliques 4
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
--
pairsInCliques :: Int -> [(Int,Int)]
pairsInCliques k = [(r,s) | r <- [1..k], s <- [1..k], r /= s, r < s]
