# Satisfiability (SAT) Solver

`sat` is a little program that builds files in the [DIMACS CNF
format](http://www.satcompetition.org/2009/format-benchmarks2009.html) used in
SAT solvers that accepts this format, like
[clasp](http://www.cs.uni-potsdam.de/clasp/) and [MiniSat](http://minisat.se/).

DIMACS requires your statement to be in CNF (conjunctive normal form). `sat`
currently cannot convert arbitrary statements to CNF.

```bash
brew install clasp

cd sat/

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

# References

Inspired by Felienne Hermans' [Quarto](https://github.com/Felienne/Quarto
) talk at LambdaConf 2016.

[clasp](http://www.cs.uni-potsdam.de/clasp/)
