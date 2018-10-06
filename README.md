# dca
Dynamic Channel Allocation through Reinforcement Learning
implementation in Haskell with Accelerate for numeric computing.

How to build:
```
stack build --ghc-options -O2
```

How to run:
```
stack exec dca-exe -- --llvm --log_iter 1
```
(if you don't have Accelerate.LLVM.Native then skip the flag to use the Interpreter.)
