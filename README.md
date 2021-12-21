# Parallel-N-Puzzle

To Build
```sh
stack build --executable-profiling --library-profiling --ghc-options="-threaded -rtsopts -eventlog " --no-system-ghc
```

To Run
```sh
stack exec .stack-work/install/x86.../.../8.6.5/bin/N-Puzzle -- input_4x4.txt +RTS -N4 -ls
```
