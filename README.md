# lambda
This is a small lambda calculus interpreter.

## Usage
For best results you should run it using Nix, but using plain cabal should work in most cases as well!

To run the interpreter just do
```
cabal run lambda
```

### Syntax
A lambda calculus term can be one of:
  - a variable
  - application of two terms
  - lambda abstracion

This interpreter can handle lambda-terms and also supports declarations (assigning a name to a lambda-term).
Following example should showcase all features:
```
> i = \x.x
> s = \x.\y.\z.x z (y z)
> k = \x.\y.x
> s k k x
s k k x
(\x.\y.\z.x z (y z)) k k x
(\v$0.\v$1.k v$1 (v$0 v$1)) k x
(\v$0.k v$0 (k v$0)) x
k x (k x)
(\x.\y.x) x (k x)
(\v$0.x) (k x)
x
```

Behind the scenes the interpreter just exhaustively applies beta- and eta-reductions.

### Prelude
You can supply a prelude (a collection of functions that are loaded when the interpreter is started) to the interpreter in a file `prelude.lmd`.
