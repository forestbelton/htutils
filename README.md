htutils
=====

## Description
htutils is a collection of tools for use with the  [tenyr](http://tenyr.info/) architecture. This project aims to provide:
 * An assembler (htas)
 * A linker (htld)
 * A simulator (htsim)

## htsim
### Build process
htsim can be built via the command `make htsim`. The [Monad Transformer Library](http://www.haskell.org/haskellwiki/Monad_Transformer_Library) is required to build htsim, as it uses the State monad extensively.
### Usage
Running htsim is fairly self-explanatory. It takes as an argument the tenyr executable to be simulated.
```
$ htsim ~/tenyr/ex/fib_iter.texe
```

## htas
### Build process
### Usage

## htld
### Build process
### Usage
