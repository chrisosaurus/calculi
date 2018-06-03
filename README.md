# calculi

A WIP collection of various calculi implementations and their semantics.

Not all calculi will have lexers and parsers, some may only exist as a Haskell ADT.


## Lambda Untyped

Untyped lambda calculus.

To run an example

    $ make
    $ cat example/LambdaUntyped/if.lu
    ((\id.
        (if (id true) unit false))
     (\x.x))
    $ stack exec LambdaUntyped examples/LambdaUntyped/if.lu
    "Unit"


## Lambda Assignment

Untyped lambda calculus with assignment.

To run an example

    $ make
    $ cat examples/LambdaAssignment/mutation.la
    ((\x.((\ignored.(read x)) (write x true))) (new false))
    $ stack exec LambdaAssignment examples/LambdaAssignment/mutation.la
    "ExpTrue"


## Running the tests

To run the unit tests:

    $ make test


## Viewing the semantics

To build and view the semantics for a given calculi

    $ cd docs/LambdaAssignment
    $ make view

