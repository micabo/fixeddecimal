test_that("basic constructor and accessors are correct", {
  x <- new_decimal(as.bigz(123), 2L)
  y <- decimal("1.23")

  expect_equal(ndecimals(x), 2)
  expect_equal(ndecimals(y), 2)
  # also tests == operator
  expect_true(x == y)
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


test_that("Noodeling", {
  x <- decimal(c("18389229.12", "238828329.2024343", "1823892.0129389139289"))
  mean(x) + new_decimal(as.bigz(19239128), 13L) - decimal("3892839.1928382920192")
})
