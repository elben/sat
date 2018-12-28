# Satisfiability (SAT) Solver

As children we learn that the game of tic-tac-toe can end in ties. We know this
from experience. But can we ask a machine to prove that ties exist in
tic-tac-toe? Or any other game?

If we can represent this question in terms of a boolean formula, we can ask
whether or not this formula is satisfiable. That is, whether we can set the
various varibles in such a way that the formula returns `True`.

This is known as the [Boolean
satisfiability](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
problem, or SAT for short.

For example, consider this simple predicate:

```
A and B
```

Where `A` and `B` are variables that can either be set to `True` or `False`.
The statement above can be satified by setting both variables to
`True`.

An example statement that cannot be satisfied is `A and (not A)`.

It turns out we can state a wide range of questions in this fashion. But when
the posed question contains a lot of variables and is very large, we need the
help of a *solver*. These are specialized programs that can take in large
questions and spit out whether or not the posed question is satisfiable.

This library is not such a solver. Instead, this library helps you write
questions in the format that solvers take in. Namely, the [DIMACS CNF
format](http://www.satcompetition.org/2009/format-benchmarks2009.html). Examples
of solvers include [clasp](http://www.cs.uni-potsdam.de/clasp/) and
[MiniSat](http://minisat.se/).

`sat` also allows you to write boolean formulas in Haskell, which can then be
converted to conjunctive normal form, which is in intermediary format that DMACS
CNF accepts. Example (note that `^` means AND, and `v`
means OR):

```haskell
-- Convert the formula (b ^ c) v a v d ==> ((b v a v d) ^ (c v a v d))
>> display $ cnf (Or [And [Var "b", Var "c"], Var "a", Var "d"])
"((b v a v d) ^ (c v a v d))"

-- Convert the formula, then emit the DIMACS format.
putStrLn $ emitDimacs $ cnf (Or [And [Var "b", Var "c"], Var "a", Var "d"])
-- p cnf 4 2
-- 1 2 3 0
-- 4 2 3 0
```

The format that `emitDimacs` spat out looks like random stuff, but that is the
exact text that you would feed the SAT solvers. Continue reading this README to
see real-world examples.

# Installing

`sat` needs an external SAT solver to work. On OS X, [clasp](http://www.cs.uni-potsdam.de/clasp/) is the easiest to install:

```bash
brew install clasp
```

On Linux and Windows, try [MiniSat](http://minisat.se/).

SAT uses Nix to build and run the REPL. If you don't have Nix installed:

```
curl https://nixos.org/nix/install | sh
```

# Using

To run a REPL, first open a Nix shell. All `cabal` commands should be executed
inside of a Nix shell:

```bash
nix-shell --attr env release.nix
```

Then, we can run the sample program (which is the TicTacToe program described
below), or open a REPL like so:

```
cabal new-run sat-exe

cabal new-repl
```

## Tic Tac Toe

I have the `TicTacToe` example that proves that tic-tac-toe can end in a tie.
Even though we all learned this at the age of five, it's a small problem that
we can all understand.

The `TicTacToe.hs` file contains detailed description on how we specify this
problem.

```bash
cabal new-repl
:l app/TicTacToe
putStrLn $ emitDimacs TicTacToe.canEndInTie
```

To prove that TicTacToe can indeed end in a tie:

```
cabal run sat-exe > tictactoe.txt

# In bash:
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

Note that the answer is `SATISFIABLE`, which means that TicTacToe can indeed end
in a tie.

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

We can form a boolean proposition that asks, for a given graph, whether or not
there is a clique of size `k` in a graph with `n` nodes.

To use:

```
# Inside of a REPL
:l app/Clique.hs

# Note that n = 6, k = 3.
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

Install nix, if you haven't already:

```
curl https://nixos.org/nix/install | sh
```

Building a new `sat.nix`:

```
# If cabal2nix is not installed:
nix-env --install cabal2nix

rm -f sat.nix && cabal2nix . > sat.nix
```

Build using Nix:

```
nix-build release.nix
```

Getting a Nix shell, and using Cabal to build:

```
nix-shell --attr env release.nix

cabal new-configure
cabal new-build
```

Run doc tests:

```
doctest -isrc -Wall -fno-warn-type-defaults src/Lib.hs
```

Run QuickCheck:

```
# Inside of a REPL
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
