import Test.QuickCheck

import Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"

prop_revrev xs = reverse xs == xs
  where types = xs::[Int]

-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
--
-- generate 10 (System.Random.mkStdGen 2) arbitrary :: [Term]
--
instance Arbitrary Term where
  -- arbitrary:: Gen Term
  arbitrary = do
    n <- choose (1, 2) :: Gen Int
    case n of
      1 -> do depth <- choose (2, 4) :: Gen Int
              t <- arbitraryWithDepth depth
              return $ Not t
      2 -> do depth <- choose (2, 4) :: Gen Int
              arbitraryWithDepth depth

  shrink = undefined

arbitraryWithDepth :: Int -> Gen Term
arbitraryWithDepth n =
  case n of
    -- At bottom. Only do vars.
    0 -> do v <- choose ('a', 'z') :: Gen Char
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
