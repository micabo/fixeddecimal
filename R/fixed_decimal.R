# implement a data type for fixed decimal digits
# based on the bigz data type from package gmp

options(fixeddecimal.rounding = "away-from-zero")

# options for rounding
# - away-from-zero
# - ...

# constructors -----------------------------------------------------------------

new_decimal <- function(x, ndecimals) {
  stopifnot(is.bigz(x))
  ndecimals <- as.integer(ndecimals)
  stopifnot(ndecimals >= 0)
  structure(x, ndecimals = ndecimals, class = c("decimal", class(x)))
}


#' Fixed-decimal vector
#'
#' Creates fixed-decimal numbers given as strings, e.g. '1.23'.
#' The string with the most decimal digits defines the decimal places for all elements.
#'
#' @param x A character vector specifying the decimals
#'
#' @return A decimal number
#' @export
#'
#' @examples
#' x <- decimal(c("1.23", "5.56"))
decimal <- function(x) {
  if (!is.character(x)) {
    stop("Argument must be of type character: e.g. '1.382983'")
  }

  digits <- str_split_fixed(x, "\\.", 2)
  integer_digits <- digits[, 1]
  fractional_digits <- digits[, 2]

  max_ndecimals <- max(str_length(fractional_digits))

  fractional_digits <- str_pad(
    fractional_digits,
    max_ndecimals,
    side = "right",
    pad = "0"
  )

  x_int_str <- str_c(integer_digits, fractional_digits)
  x_int_str <- str_replace(x_int_str, "^0*", "")
  x_int_str <- ifelse(x_int_str == "", 0, x_int_str)
  x_int_str[is.na(x)] <- NA
  x <- as.bigz(x_int_str)

  structure(x, ndecimals = max_ndecimals, class = c("decimal", class(x)))
}


#' Fixed-decimal vector
#'
#' Convert a variable of type character to (fixed-point) decimal (same as 'decimal')
#'
#' @param x A character vector specifying the decimals
#'
#' @return A decimal number
#' @export
#'
#' @examples
#' x <- as.decimal(c("1.23", "5.56"))
as.decimal <- decimal


# accessor ---------------------------------------------------------------------


#' Get number of decimal digits
#'
#' @param x A vector of type 'decimal'
#'
#' @return An integer >= 0
#' @export
#'
#' @examples
#' x <- decimal("1.23")
#' ndecimals(x)
ndecimals <- function(x) {
  attr(x, "ndecimals")
}


# predicates -------------------------------------------------------------------


#' Predicate: is x of type 'decimal'
#'
#' @param x A vector
#'
#' @return TRUE for a vector of type decimal
#' @export
#'
#' @examples
#' is.decimal(decimal("1.23"))
is.decimal <- function(x) {
  inherits(x, "decimal")
}


#' @export
is.na.decimal <- function(x) {
  NextMethod()
}



# conversion -------------------------------------------------------------------

#' @export
as.double.decimal <- function(x, ...) {
  NextMethod() / 10^ndecimals(x)
}


#' @export
as.character.decimal <- function(x, ...) {
  fmt <- paste0("%.", ndecimals(x), "f")
  sprintf(fmt, as.double(x))
  # Could also do a purely string-based approach -> put the "." at the right position
}


# utility functions ------------------------------------------------------------

#' @export
length.decimal <- function(x) {
  NextMethod()
}


#' @export
c.decimal <- function(...) {
  argv <- list(...)

  num_is_decimal <- vapply(argv, is.decimal, logical(1))
  if (!isTRUE(all(num_is_decimal))) stop("Cannot concatenate different types")

  decimal_places <- vapply(argv, ndecimals, integer(1))
  decimal_places_equal <- min(decimal_places) == max(decimal_places)
  if (!all(num_is_decimal) || !decimal_places_equal) {
    stop("Cannot concatenate decimals with different number of decimal places")
  }

  new_decimal(NextMethod(), decimal_places[[1]])
}


#' @export
print.decimal <- function(x, ...) {
  cat(
    paste0("Fixed Decimal (", ndecimals(x), " decimal digits)\n"),
    paste(as.character(x), collapse = " ")
  )
}


# rounding ---------------------------------------------------------------------

round_to_10_afz <- function(x) {
  # Round to 10ths, away-from-zero
  # Note: could not be implemented with ifelse (does work with bigz)
  stopifnot(is.bigz(x))
  z <- abs(x)
  remainder <- z %% 10
  z <- z - remainder + 10 * (remainder >= 5)
  z * sign(x)
}


round_to_10_rtz <- function(x) {
  # Round to 10ths, round-to-zero
  stopifnot(is.bigz(x))
  z <- abs(x)
  remainder <- z %% 10
  z <- z - remainder + 10 * (remainder > 5)
  z * sign(x)
}


round_with_strategy <- function(x, strategy) {
  stopifnot(is.bigz(x))
  switch(strategy,
    "away-from-zero" = round_to_10_afz(x) %/% 10,
    "round-to-zero" = round_to_10_rtz(x) %/% 10,
    stop("Rounding strategy '", strategy, "' unknown")
  )
}


#' Round decimal value
#'
#' Round decimal to `digits` decimal digits according to strategy
#'
#' @param x A vector of type decimal
#' @param digits An integer >= 0, number of decimals to round to
#'
#' @return A rounded decimal z with ndecimals(z) == digits
#' @export
#'
#' @examples
#' round(decimal("1.25"), 1)
round.decimal <- function(x, digits = 0L, strategy = getOption("fixeddecimal.rounding")) {
  digits <- as.integer(digits)
  if (digits == ndecimals(x)) {
    x
  } else if (digits < ndecimals(x)) {
    ndiff <- ndecimals(x) - digits
    z <- as.bigz(x) %/% 10^(ndiff - 1)
    z <- round_with_strategy(z, strategy)
    new_decimal(z, digits)
  } else {
    stop(
      "Argument 'digits' larger than decimal places of decimal.\n",
      "Cannot increase precision by rounding"
    )
  }
}


# override group generics ------------------------------------------------------

#' @export
Math.decimal <- function(x, ...) {
  stop("Math functions not fully implemented for decimal")
}


#' @export
Ops.decimal <- function(e1, e2) {
  stop("Ops functions not fully implemented for decimal")
}


#' @export
Summary.decimal <- function(..., na.rm) {
  stop("Summary functions not fully implemented for decimal")
}


# relational operators functions -----------------------------------------------

#' @export
`<.decimal` <- function(x, y) {
  # naive implementation, could also round the number with more decimal places
  # and compare after that
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`>.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`<=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`>=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`==.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`!=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  NextMethod()
}


# arithmetic functions ---------------------------------------------------------

#' @export
`+.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  new_decimal(NextMethod(), ndecimals(x))
}


#' @export
`-.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  new_decimal(NextMethod(), ndecimals(x))
}


#' @export
`*.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  z <- new_decimal(NextMethod(), ndecimals(x) * 2L)
  round(z, ndecimals(x))
}


#' @export
`/.decimal` <- function(x, y, strategy = getOption("fixeddecimal.rounding")) {
  stopifnot(is.decimal(x) && is.decimal(y))
  stopifnot(ndecimals(x) == ndecimals(y))
  z <- as.bigz(x) * 10^(ndecimals(x) + 1) / as.bigz(y)
  z <- round_with_strategy(as.bigz(z), strategy)
  new_decimal(as.bigz(z), ndecimals(x))
}


#' @export
abs.decimal <- function(x) {
  new_decimal(NextMethod(), ndecimals(x))
}


#' @export
min.decimal <- function(..., na.rm = FALSE) {
  new_decimal(NextMethod(), ndecimals(...))
}


#' @export
max.decimal <- function(..., na.rm = FALSE) {
  new_decimal(NextMethod(), ndecimals(...))
}


#' @export
sum.decimal <- function(..., na.rm = FALSE) {
  argv <- c(...)
  decimal_places <- ndecimals(argv)

  if (!na.rm && any(is.na(argv))) {
    new_decimal(as.bigz(NA), ndecimals = decimal_places)
  } else {
    new_decimal(NextMethod(), ndecimals = decimal_places)
  }
}


#' @export
prod.decimal <- function(..., na.rm = FALSE) {
  argv <- c(...)
  decimal_places <- ndecimals(argv)

  if (!na.rm && any(is.na(argv))) {
    new_decimal(as.bigz(NA), ndecimals = decimal_places)
  } else {
    z <- new_decimal(NextMethod(), ndecimals = decimal_places * length(argv))
    round(z, decimal_places)
  }
}


#' @export
mean.decimal <- function(x, ..., na.rm = FALSE, strategy = getOption("fixeddecimal.rounding")) {
  if (!na.rm && any(is.na(x))) {
    new_decimal(as.bigz(NA), ndecimals = ndecimals(x))
  } else {
    # Note: The calculation is implemented in the bigz world.
    # Could also be implemented via sum.decimal, /.decimal, round.decimal,
    # but this uses fewer conversions
    x_mean <- sum(as.bigz(x), na.rm = TRUE) * 10
    x_mean <- x_mean / (length(x) - sum(is.na(x)))
    x_mean <- round_with_strategy(as.bigz(x_mean), strategy)
    new_decimal(x_mean, ndecimals = ndecimals(x))
  }
}


#' @export
as.data.frame.decimal <- as.data.frame.vector
