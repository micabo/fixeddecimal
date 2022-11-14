# implement a data type for fixed decimal digits
# based on the bigz data type from package gmp

# constructors -----------------------------------------------------------------

new_decimal <- function(x, ndecimals) {
  stopifnot(is.bigz(x) && is.integer(ndecimals) && ndecimals >= 0)
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
  args <- list(...)
  num_is_decimal <- vapply(args, is.decimal, logical(1))
  decimal_places <- vapply(args, ndecimals, integer(1))
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


round_to_10ths <- function(x) {
  stopifnot(is.bigz(x))
  remainder <- x %% 10
  if (remainder < 5) {
    x - remainder
  } else {
    x + 10 - remainder
  }
}


#' Round ('round half away from zero') decimal to `digits` decimal digits
#'
#' @param x A vector of type decimal
#' @param digits An integer >= 0, number of decimals to round to
#'
#' @return A rounded decimal z with ndecimals(z) == digits
#' @export
#'
#' @examples
#' round(decimal("1.25"), 1)
round.decimal <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  if (digits == ndecimals(x)) {
    x
  } else if (digits < ndecimals(x)) {
    ndiff <- ndecimals(x) - digits
    z <- as.bigz(x) %/% 10^(ndiff-1)
    z <- round_to_10ths(z) %/% 10
    new_decimal(z, digits)
  } else {
    stop("Argument 'digits' larger than decimal places of decimal.\n",
         "Cannot increase precision by rounding")
  }
}


# override group generics ------------------------------------------------------

#' @export
Math.decimal <- function(x, ...) {
  stop("Math not implemented for decimal")
}


#' @export
Ops.decimal <- function(e1, e2) {
  stop("Ops not implemented for decimal")
}


#' @export
Summary.decimal <- function(..., na.rm) {
  stop("Summary not implemented for decimal")
}

# arithmetic functions ---------------------------------------------------------

#' @export
`<.decimal` <- function(x, y) {
  # naive implementation, could also round the number with more decimal places
  # and compare after that
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`>.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`<=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`>=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`==.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`!=.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
}


#' @export
`+.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  new_decimal(NextMethod(), ndecimals(x))
}


#' @export
`-.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  new_decimal(NextMethod(), ndecimals(x))
}


#' @export
`*.decimal` <- function(x, y) {
  # naive implementation...
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  new_decimal(NextMethod(), ndecimals(x) * 2L)
}


#' @export
`/.decimal` <- function(x, y) {
  # pseudo implementation
  decimal(as.character(as.double(x) / as.double(y)))
}


#' @export
`^.decimal` <- function(x, y) {
  # pseudo implementation
  decimal(as.character(as.double(x)^as.double(y)))
}


#' @export
`sqrt.decimal` <- function(x) {
  # pseudo implementation
  decimal(as.character(sqrt(as.double(x))))
}


#' @export
`abs.decimal` <- function(x) {
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
sum.decimal <- function(x, ..., na.rm = FALSE) {
  if (!na.rm && any(is.na(x))) {
    new_decimal(as.bigz(NA), ndecimals = ndecimals(x))
  } else {
    new_decimal(NextMethod(), ndecimals = ndecimals(x))
  }
}


#' @export
mean.decimal <- function(x, ..., na.rm = FALSE) {
  if (!na.rm && any(is.na(x))) {
    new_decimal(as.bigz(NA), ndecimals = ndecimals(x))
  } else {
    # TODO
    # Re-write using NextMethod or use round.decimal -> need to implement division
    x_mean <- sum(x, na.rm = TRUE)
    x_mean <- as.bigz(x_mean) * 10 / (length(x) - sum(is.na(x)))
    x_mean <- round_to_10ths(as.bigz(x_mean)) %/% 10
    new_decimal(x_mean, ndecimals = ndecimals(x))
  }
}
