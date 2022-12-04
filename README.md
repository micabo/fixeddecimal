# Fixed Decimals (`fixeddecimal`)

To the best of my knowledge R does not have a package that implements (exact) decimal arithmetic.
This package aims at providing some of this functionality for _simple_ tasks.
I have found exact decimals useful for some simple/short calculations.
For example, calculations in a report where a reviewer might check a calculation with a few decimal digits by hand and will expect a certain result (which cannot be guaranteed with binary floating point numbers).

The decimal representation in this package is built on the data type `bigz` provided by the `gmp` package.

All operations (e.g. `+`, `-`, `*`, `/`, `mean`) on decimals x and y will require the decimals to have the same number of fractional decimal digits and will retain the number of decimals digits of the input variables.
Operations `*`, `/` and `mean` will round the result (away from zero).
Multiple operations of this type will lead to multiple rounding (try to use `prod`).

## Implemented

- Arithmetic operations (`+`, `-`, `*`, `/`)
- `sum`, `prod`, `mean`
- `min`, `max`, `abs`
- relational operators (`==`, `<`, ...)
- `round.decimal`
- Utilities (`c.decimal`, `print.decimal` etc.)
