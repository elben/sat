# Satisfiability (SAT) Solver

`sat` is a little program that builds files in the [DIMACS CNF
format](http://www.satcompetition.org/2009/format-benchmarks2009.html) used in
SAT solvers that accepts this format, like
[clasp](http://www.cs.uni-potsdam.de/clasp/) and [MiniSat](http://minisat.se/).

You can also write predicates in Haskell that can be converted to conjunctive normal form.

In a `stack ghci` (note that `^` means AND, and `v` means OR):

```haskell
-- Convert the predicate (b ^ c) v a v d ==> ((b v a v d) ^ (c v a v d))
>> display $ cnf (Or [And [Var "b", Var "c"], Var "a", Var "d"])
"((b v a v d) ^ (c v a v d))"

-- Convert the predicate, then emit the DIMACS format.
putStrLn $ emitDimacs $ cnf (Or [And [Var "b", Var "c"], Var "a", Var "d"])
-- p cnf 4 2
-- 1 2 3 0
-- 4 2 3 0
```

# Installing

`sat` needs an external SAT solver to work. On OS X, [clasp](http://www.cs.uni-potsdam.de/clasp/) is the easiest to install:

```bash
brew install clasp
```

On Linux and Windows, try [MiniSat](http://minisat.se/).

# Using

```
stack ghci
stack build
stack exec doctest -- -isrc -Wall -fno-warn-type-defaults src/Lib.hs

stack exec sat-exe
```

## Tic Tac Toe

I have the `TicTacToe` example that proves that tic-tac-toe can end in a tie.
Even though we all learned this at the age of five, it's a small problem that
we can all understand.

The `TicTacToe.hs` file contains detailed description on how we specify this
problem.

```bash
stack ghci
putStrLn $ emitDimacs TicTacToe.canEndInTie

stack build
stack exec sat-exe > tictactoe.txt
clasp 3 tictactoe.txt

# c clasp version 3.1.3
# c Reading from tictactoe.txt
# c Solving...
# c Answer: 1
# v -1 2 -3 4 -5 -6 7 -8 9 10 -11 12 -13 14 15 -16 17 -18 0
# c Answer: 2
# v -1 2 -3 -4 -5 6 7 -8 9 10 -11 12 13 14 -15 -16 17 -18 0
# c Answer: 3
# v -1 2 -3 4 -5 6 7 -8 9 10 -11 12 -13 14 -15 -16 17 -18 0
# s SATISFIABLE
# c
# c Models         : 3+
# c Calls          : 1
# c Time           : 0.001s (Solving: 0.00s 1st Model: 0.00s Unsat: 0.00s)
# c CPU Time       : 0.000s
```

## Clique

The `Clique` module allows you to build Clique propositions.

A [clique](https://en.wikipedia.org/wiki/Clique_(graph_theory)) is a sub-graph
of an undirected graph where every node is connected to every other node.

For example, the graph below has a clique of size 3. Namely, the nodes
`{2,3,5}` form the clique because each node is connected to every other node.

```
       3----4
      / \   |
     2---5--6
    /
   1
```

To use:

```
stack ghci
:l app/Clique.hs

>>> putStrLn $ emitDimacs $ buildGraph 6 3 [(1,2), (2,3), (2,5), (3,4), (3,5), (4,6), (5,6)]

# Copy-paste output into clique.txt.

clasp 1 clique.txt

# c Reading from clique.txt
# c Solving...
# c Answer: 1
# v -1 2 -3 -4 -5 -6 -7 -8 -9 -10 11 -12 -13 -14 15 -16 -17 -18 0
# s SATISFIABLE
```

## Others

Other problem to solve via SAT:

- [SuDoku](http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html)
- 3-Coloring
- Hamiltonian cycle

# Development

Run doc tests:

```
stack exec doctest -- -isrc -Wall -fno-warn-type-defaults src/Lib.hs
```

Run QuickCheck:

```
stack ghci
:l test/Spec.hs
>> main
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
```

# References

Inspired by Felienne Hermans' [Quarto](https://github.com/Felienne/Quarto
) talk at LambdaConf 2016.

[clasp](http://www.cs.uni-potsdam.de/clasp/)

http://www.cs.duke.edu/courses/summer13/compsci230/restricted/lectures/L03.pdf
