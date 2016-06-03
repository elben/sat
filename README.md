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

TODO!!! This is the interesting stuff:
Other problem to solve via SAT:

- SuDoku
  - http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html
- Clique
  - http://blog.computationalcomplexity.org/2006/12/reductions-to-sat.html
  - "We can get similarly nice reductions for many other NP-complete problems
  like 3-COLORING and HAMILTONIAN CYCLE. But there is no general procedure for
  producing simple formula, especially if there are calculations involved like
  SUBSET SUM."
- Subset sum
  - http://people.clarkson.edu/~alexis/PCMI/Notes/lectureB07.pdf
  - http://cs.mcgill.ca/~lyepre/pdf/assignment2-solutions/subsetSumNPCompleteness.pdf
  - http://users.cms.caltech.edu/~umans/cs21/lec22.pdf
- Knapsack, binpacking, etc
  - https://www.cs.princeton.edu/~rs/AlgsDS07/23Reductions.pdf
  - http://faculty.ycp.edu/~dbabcock/PastCourses/cs360/lectures/lecture27.html

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
