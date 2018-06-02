# calculi

A WIP collection of various calculi implementations and their semantics.

Not all calculi will have lexers and parsers, some may only exist as a Haskell ADT.


## Lambda Assignment

Minimum-viable lambda calculus with assignment interpreter.

To run an example

    $ make
    $ stack exec LambdaAssignment examples/LambdaAssignment/mutation.la

## Running the tests

To run the unit tests:

    $ make test


## Viewing the semantics

To build and view the semantics for a given calculi

    $ cd docs/LambdaAssignment
    $ make view

