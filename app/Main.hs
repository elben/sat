module Main where

import Lib
import qualified TicTacToe

main :: IO ()
main = putStrLn $ emitDimacs TicTacToe.canEndInTie
