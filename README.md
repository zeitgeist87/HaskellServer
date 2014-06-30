HaskellServer
=============

A small webserver written in Haskell with only Network.Socket as a dependency.
It is mainly a toy project to explore Haskell as a programming language.
I put more emphasis on beautiful coding style, than on performance optimizations.

## Compilation

You have to install the network package with cabal:

```
cabal update
cabal install network
```

The server can be easily compiled with the following command:

```
ghc Server.hs
```

## Usage

```
./Server -h
Usage: Server [OPTION...]
  -v       --version    show version number
  -n NAME  --host=NAME  hostname to bind to
  -p INT   --port=INT   port number
  -d DIR   --dir=DIR    path to public directory
  -h       --help       display this help
```
