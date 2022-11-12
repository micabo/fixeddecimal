# implement a data type for fixed decimal digits

# constructors -----------------------------------------------------------------

new_decimal <- function(x, ndecimals) {
  stopifnot(is.bigz(x) && is.integer(ndecimals) && ndecimals >= 0)
  structure(x, ndecimals = ndecimals, class = c("decimal", class(x)))
}


#' @export
as.decimal <- function(x) {
  x <- as.character(x)

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
  x <- as.bigz(x_int_str)

  structure(x, ndecimals = max_ndecimals, class = c("decimal", class(x)))
}


#' @export
decimal <- as.decimal


# accessor ---------------------------------------------------------------------

#' @export
ndecimals <- function(x) {
  attr(x, "ndecimals")
}


# predicates -------------------------------------------------------------------

#' @export
is.decimal <- function(x) {
  inherits(x, "decimal")
}


#' @export
is.na.decimal <- function(x) {
  NextMethod()
  #is.na(as.bigz(x))
}



# conversion -------------------------------------------------------------------

#' @export
as.double.decimal <- function(x) {
  NextMethod() / 10^ndecimals(x)
  #as.double(asNumeric(as.bigz(x) / 10^ndecimals(x)))
}


#' @export
as.character.decimal <- function(x) {
  fmt <- paste0("%.", ndecimals(x), "f")
  sprintf(fmt, as.double(x))
  # Could also do a purely string-based approach -> put the "." at the right position
}


# utility functions ------------------------------------------------------------

#' @export
length.decimal <- function(x) {
  NextMethod()
  # length(as.bigz(x))
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

  new_decimal(NextMethod(), ndecimals(args[[1]]))
  # as.decimal(unlist(lapply(args, as.character)))
}


#' @export
print.decimal <- function(x) {
  cat(
    paste0("Fixed Decimal (", ndecimals(x), " decimal digits)\n"),
    paste(as.character(x), collapse = " ")
  )
}


# mathematical functions -------------------------------------------------------

#' @export
`==.decimal` <- function(x, y) {
  stopifnot(is.decimal(x) && is.decimal(y) && ndecimals(x) == ndecimals(y))
  NextMethod()
  # as.bigz(x) == as.bigz(y)
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


# TODO:
# *.decimal etc.


.round_to_10ths <- function(x) {
  stopifnot(is.bigz(x))
  remainder <- x %% 10
  if (remainder < 5) {
    x - remainder
  } else {
    x + 10 - remainder
  }
}


#' @export
round.decimal <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  if (digits == ndecimals(x)) {
    x
  } else if (digits < ndecimals(x)) {
    # TODO
    ndiff <- ndecimals(x) - digits
    z <- as.bigz(x) %/% 10^(ndiff-1)
    z <- .round_to_10ths(z) %/% 10
    new_decimal(z, digits)
  } else {
    stop("Cannot increase precision by rounding")
  }
}


#' @export
sum.decimal <- function(x, na.rm = FALSE) {
  # TODO: test that all decimals have the same number of decimal digits
  if (!na.rm && any(is.na(x))) {
    as.decimal(NA)
  } else {
    new_decimal(NextMethod(), ndecimals = ndecimals(x))
  }
}


#' @export
mean.decimal <- function(x, na.rm = FALSE) {
  # TODO: test that all decimals have the same number of decimal digits
  if (!na.rm && any(is.na(x))) {
    as.decimal(NA)
  } else {
    # TODO
    # Re-write using NextMethod or use sum and round.decimal
    x_mean <- sum(as.bigz(x), na.rm = TRUE)
    x_mean <- x_mean * 10 / (length(x) - sum(is.na(x)))
    x_mean <- .round_to_10ths(as.bigz(x_mean)) %/% 10
    new_decimal(x_mean, ndecimals = ndecimals(x))
  }
}


# TODO
# - var and sd are not s3 generic -> how to implement?
#' @export
var.decimal <- function(x, na.rm = FALSE) {}

#' @export
sd.decimal <- function(x, na.rm = FALSE) {}
