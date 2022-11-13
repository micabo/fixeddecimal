# Fixed Decimals

To the best of my knowledge R does not have a package that implements (exact) decimal arithmetic.
This package aims at providing this functionality.
Currently, the implementation is very basic and does not cover much of what you'd want to do with the data type.

The decimal representation is built on the data type `bigz` provided by the `gmp` package.
