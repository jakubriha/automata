# Automata [![Build Status](https://travis-ci.org/jakubriha/automata.svg?branch=master)](https://travis-ci.org/jakubriha/automata)
Implementation of an efficient functional library for finite automata (FA). This library is intended to be used primarily in the field of the formal verification. Available operations on finite automata correspond to this usage. The library provides the following operations:

* Membership testing (located in `module Operations.run`),
* Union (located in module `Operations.union`),
* Intersection (located in module `Operations.intersect`),
* Determinization (located in module `Operations.determinize`),

The library will also provide the following operations in the future:

* Complement (located in module `Operations.complement`),
* Emptiness testing (located in module `Operations.empty`),
* Inclusion testing (located in module `Operations.inclusion`),
* Universality testing (located in module `Operations.universality`).

## Experimenting with the library
This library uses [Stack](https://docs.haskellstack.org) as a development tool. It is necessary to install Stack in order to build the library. Stack docs contain the [installation instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install).

After the installation of Stack, you can start the REPL by executing `stack ghci` in the root of the library. This will download all required dependencies and start GHCi with all library modules loaded.

The function `Testing.test2Fa` allows us to load two FAs from a text file and execute a binary operation on them. Consider the following expression executed in GHCi:

```Testing.test2Fa "test-suite/AutomataExamples/0.txt" "test-suite/AutomataExamples/1.txt" Operations.intersect```

It loads two FAs (located in `test-suite/AutomataExamples/{0,1}.txt`), performs intersection on them, and prints the resulting FA on the standard output.

