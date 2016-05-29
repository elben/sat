module TicTacToe where

import Lib

-- Builds CNF file that checks whether or not tic-tac-toe can end in a tie.
--
-- To specify our state, we first specify positions on the board as such:
--
-- 123
-- 456
-- 789
--
-- Our vars are player positions. @x1@ is true if an 'X' has been played in
-- position 1. False otherwise.
--
-- To build our statement of a tie, we need to specify:
--
-- - Player X is not in a winning position
-- - Player O is not in a winning position
--
--     !(x1 ^ x2 ^ x3) ^
--     !(x4 ^ x5 ^ x6) ^ ...
--
--     Applying De Morgan's to get to BNF:
--
--     (!x1 v !x2 v !x3) ^
--     (!x4 v !x5 v !x6) ^ ...
--
-- - For every location, either X or O is true
--
--     (x1 v o1) ^ (x2 v o2) ^ ...
--
-- - For every location, exactly one of X or O is true
--
--    !(x1 ^ o1) ^
--    !(x2 ^ o2) ^ ...
--
--    Applying De Morgan's to get to BNF:
--
--    (!x1 v !o1) ^
--    (!x2 v !o2) ^ ...
--
-- - The game is played-out. That is, since X always starts first, X has 5
--   plays, and O has 4 plays.
--
--     This is true by the constructs above. We can write a proof to show that
--     if X has *more* than 5 plays, then X *must* be in a winning position.

-- List of winning positions.
winningPositions :: [[Int]]
winningPositions =
  [[(r*3)+1..(r*3)+3] | r <- [0..2]] ++
  [[c,c+3,c+6] | c <- [1..3]] ++
  [[1,5,9], [3,5,7]]

-- Converts ["a", "b", "c"] to (!a v !b v !c)
negateOrs :: [String] -> Term
negateOrs terms = Or (map (Not . Var) terms)

-- Statement that player is not in a winning position.
playerNotInWinningPosition :: String -> Term
playerNotInWinningPosition player =
  let positions = map (map (\cell -> player ++ show cell)) winningPositions
  in And (map negateOrs positions)

-- Every location has one of two players on it.
everyLocationFilled :: Term
everyLocationFilled = And [Or [Var ("x" ++ show c), Var ("o" ++ show c)] | c <- [1..9]]

-- Every location occupied only by one player.
playerNotInSamePosition :: Term
playerNotInSamePosition = And [Or [Not (Var ("x" ++ show c)), Not (Var ("o" ++ show c))] | c <- [1..9]]

canEndInTie :: Term
canEndInTie =
  let And p1Terms = playerNotInWinningPosition "x"
      And p2Terms = playerNotInWinningPosition "o"
      And x       = playerNotInSamePosition
      And y       = everyLocationFilled
  in And (p1Terms ++ p2Terms ++ x ++ y)

-- putStrLn $ toDimacsBody noWinner
