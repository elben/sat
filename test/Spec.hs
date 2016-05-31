import Test.QuickCheck

import Lib

main :: IO ()
main = do
  quickCheck prop_isCnf

prop_isCnf :: Term -> Bool
prop_isCnf t = isCnf $ cnf t

prop_revrev xs = reverse xs == xs
  where types = xs::[Int]

-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
--
instance Arbitrary Term where
  -- arbitrary :: Gen Term
  arbitrary = do
    n <- choose (1, 2) :: Gen Int
    case n of
      1 -> do depth <- choose (2, 4) :: Gen Int
              t <- arbitraryWithDepth depth
              return $ Not t
      2 -> do depth <- choose (2, 4) :: Gen Int
              arbitraryWithDepth depth

  -- | Shrink Term.
  --
  -- >>> shrink (Var "v")
  -- []
  --
  -- >>> shrink (Not (Var "v"))
  -- [v]
  --
  -- a v (~b v c)
  -- a, (~b v c), ~b, c
  -- >>> shrink (Or [Var "a", Or [Not (Var "b"),Var "c"]])
  -- [a,(~b v c),(),(~b v c v (b) v ())]
  --
  -- shrink :: Term -> [Term]
  shrink (Var v) = []
  shrink (Not t) = t : shrink t
  shrink (Or terms) =
    -- order by most aggressive shrinking first
    -- shrink to subterms
    terms ++
    -- recursively shrink each subterm
    [Or ts | ts <- map shrink terms]
  shrink (And terms) =
    -- order by most aggressive shrinking first
    -- shrink to subterms
    terms ++
    -- recursively shrink each subterm
    [And ts | ts <- map shrink terms]

arbitraryWithDepth :: Int -> Gen Term
arbitraryWithDepth n =
  case n of
    -- At bottom. Only do vars. Stop at 'u' so we don't hit 'v' to confuse with
    -- "or".
    0 -> do v <- choose ('a', 'u') :: Gen Char
            return $ Var [v]
    _ -> do
      m <- choose (1, 4) :: Gen Int
      case m of
        1 -> do v <- choose ('a', 'z') :: Gen Char
                return $ Var [v]
        2 -> do t <- arbitraryWithDepth (n-1)
                return $ Not t
        3 -> do numTerms <- choose (1,1) :: Gen Int -- 2 to 3 terms
                terms <- mapM (const (arbitraryWithDepth (n-1))) [0..numTerms]
                return $ And terms
        4 -> do numTerms <- choose (1,1) :: Gen Int -- 2 to 3 terms
                terms <- mapM (const (arbitraryWithDepth (n-1))) [0..numTerms]
                return $ Or terms

-- :m +Test.QuickCheck.Arbitrary
-- :m +Test.QuickCheck.Gen
-- sample' arbitrary :: IO [Bool]
-- sample' arbitrary :: IO [Term]
-- generate $ vectorOf 1 arbitrary :: IO [Term]
-- generate arbitrary :: IO [Term]
