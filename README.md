# Automata [![Build Status](https://travis-ci.org/jakubriha/automata.svg?branch=master)](https://travis-ci.org/jakubriha/automata)
Implementation of an efficient functional library for finite automata (FA). This library is intended to be used primarily in the field of the formal verification. Available operations on finite automata correspond to this usage. The library provides the following operations, which are located in the module `Operations.Regular`:

* Membership testing (function `run`),
* Union (function `union`),
* Intersection (function `intersect`),
* Determinization (function `determinize`),
* Complement (function `complement`),
* Emptiness testing (function `isEmpty`),
* Inclusion testing (function `isSubsetOf`),
* Universality testing (function `isUniversal`).

Most of these operations also have their counterparts (in the module `Operations.WithExternalSymbols`), which allow to specify symbols set to work with. For example, the function `isEmpty :: Set sym -> Fa sym sta -> Bool` from this module has the first parameter, `Set sym`, for this reason.

Function `union` only supports FAs that both have the *same* type of a state. To overcome that, you can use a function `union` (in the module `Operations.Product`), which performs union of two FAs in which each has a different type of a state.

Finally, the library also provides two operations implemented using the [antichain-based approach](http://link.springer.com/chapter/10.1007/978-3-642-12002-2_14). These operations are located in the module `Operations.Antichain`:

* Inclusion testing (function `isSubsetOf`),
* Universality testing (function `isUniversal`).

## Experimenting with the library
This library uses [Stack](https://docs.haskellstack.org) as a development tool. It is necessary to install Stack in order to build the library. Stack docs contain the [installation instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install). After the installation of Stack, it is necessary to install the gtk2hs library by executing `stack install gtk2hs-buildtools`.

After the necessary setup, you can start the REPL by executing `stack ghci` in the root of the library. It will download all required dependencies and start GHCi with all library modules loaded. You can use the function `unsafeLoadFa :: FilePath -> Fa Symbol State` located in the module `Testing` to load an FA from a [Timbuk format](http://www.fit.vutbr.cz/research/groups/verifit/tools/libvata/#input) file. The following example shows how to load two FAs from files `{0, 1}.txt` and, after that, how to execute a binary operation on them in GHCi:

```
> let fstFa = unsafeLoadFa "tests/Examples/0.txt"
> let sndFa = unsafeLoadFa "tests/Examples/1.txt"
> fstFa `intersect` sndFa
```

The resulting FA of the intersection operation will be printed in the GHCi terminal in the Timbuk format. It is also possible to display the resulting FA graphically using the function `displayFa :: Fa sym sta -> IO ()` located in the module `Visualization`:

```
> displayFa (fstFa `intersect` sndFa)
```
