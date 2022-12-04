test_that("basic constructor: new_decimal", {
  expect_error(new_decimal(123, 2)) # first arg must be bigz
  expect_error(new_decimal(as.bigz(123), -1)) # second arg must be >= 0

  # test conversion back to bigz
  x <- as.bigz(new_decimal(as.bigz(123), 2L))
  expect_true(x == as.bigz(123))
})


test_that("accesor: ndecimals", {
  x <- new_decimal(as.bigz(12345), 3L)
  expect_equal(ndecimals(x), 3)
})


test_that("predicate: is.decimal", {
  x <- new_decimal(as.bigz(12345), 3L)
  expect_true(is.decimal(x))
  expect_false(is.decimal(1))
})


test_that("predicate: is.na.decimal", {
  x <- new_decimal(as.bigz(NA), 100L)
  expect_true(is.na(x))
})


test_that("constructor: decimal", {
  x <- decimal(c("1.23", NA, "000.0"))
  x_bigz <- as.bigz(x)
  expect_true(x_bigz[[1]] == 123)
  expect_true(is.na(x_bigz[[2]]))
  expect_true(x_bigz[[3]] == 0)
})


test_that("as.character.decimal", {
  x <- new_decimal(as.bigz(12345), 3L)
  expect_equal(as.character(x), "12.345")
})


test_that("length.decimal", {
  x <- decimal(c("1.23", NA, "000.0"))
  expect_equal(length(x), 3)
})


test_that("c.decimal", {
  x <- decimal(c("1.23", NA, "000.0"))
  expect_error(c(x, 3))
  expect_error(c(x, decimal("123.456789")))

  y <- decimal("1.59")
  expect_length(c(x, y), 4)
})


test_that("relational operators", {
  x <- decimal("1.23")
  y <- decimal("1.45")
  z <- decimal("1.23")

  expect_true(x < y)
  expect_true(x <= y)
  expect_true(x <= z)
  expect_true(y > x)
  expect_true(y >= x)
  expect_true(z >= x)
  expect_true(x == z)
  expect_true(x != y)
})


test_that("round.decimal", {
  x <- decimal("1.44")
  y <- decimal("1.45")
  z <- decimal("1.5")
  expect_false(round(x, 1) == z)
  expect_true(round(z, 1) == z)
})


test_that("addition and subtraction", {
  x <- decimal("15.46")
  y <- decimal("7.13")
  z <- decimal("100000000.00")

  expect_true(x + y == decimal("22.59"))
  expect_true(x - y == decimal("8.33"))
  expect_true(x + z == decimal("100000015.46"))

  expect_error(decimal("1.23") + decimal("0.001"))
})


test_that("multiplication is correct", {
  x <- decimal("38.49")
  y <- decimal("3.00")
  z <- decimal("12.83")

  expect_true(x * y == decimal("115.47"))
  expect_true(x * z == decimal("493.83"))
  expect_true(y * z == x)
})


test_that("division is correct", {
  x <- decimal("38.49")
  y <- decimal("3.00")
  z <- decimal("12.83")

  expect_true(x / y == z)
  expect_true(x / z == y)
  expect_true(y / z == decimal("0.23"))
})


test_that("sum.decimal", {
  x <- decimal(c("1.00", "1.01", "1.02", "1.03"))
  expect_true(sum(x) == decimal("4.06"))
})


test_that("prod.decimal", {
  x <- decimal(c("1.00", "1.01", "1.02", "1.03"))
  expect_true(prod(x) == decimal("1.06"))
})


test_that("mean.decimal", {
  x <- decimal(c("1.01", "1.08", "1.05"))
  expect_true(mean(x) == decimal("1.05"))
})
