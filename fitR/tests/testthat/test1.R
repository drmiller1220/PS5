context("Adding squares correctly")
test_that("squares add correctly", {
  expect_that(addSquares(2,3),
              equals(new("Squares", square=(13), x = 2, y = 3)))
  expect_that(addSquares(2,2),
              equals(new("Squares", square=(8), x = 2, y = 2)))
  expect_that(addSquares(4,2),
              equals(new("Squares", square=(20), x = 4, y = 2)))
})

context("Adding squares correct classes")
test_that("squares add correctly", {
  expect_is(addSquares(6,9), "Squares")
  expect_is(addSquares(3,7), "Squares")
  expect_is(addSquares(4,2), "Squares")
})

context("Adding squares should throw errors for non-numeric inputs")
test_that("squares add correctly", {
  expect_error(addSquares(Q))
  expect_error(addSquares("p"))
  expect_error(addSquares("test"))
})

context("subtracting squares correctly")
test_that("squares subtract correctly", {
  expect_that(subtractSquares(2,3),
              equals(new("Squares", square=(-5), x = 2, y = 3)))
  expect_that(subtractSquares(2,2),
              equals(new("Squares", square=(0), x = 2, y = 2)))
  expect_that(subtractSquares(4,2),
              equals(new("Squares", square=(12), x = 4, y = 2)))
})

context("subtracting squares correct classes")
test_that("squares subtract correctly", {
  expect_is(subtractSquares(6,9), "Squares")
  expect_is(subtractSquares(3,7), "Squares")
  expect_is(subtractSquares(4,2), "Squares")
})

context("subtracting squares should throw errors for non-numeric inputs")
test_that("squares subtract correctly", {
  expect_error(subtractSquares(Q))
  expect_error(subtractSquares("p"))
  expect_error(subtractSquares("test"))
})

context("multiplying squares correctly")
test_that("squares multiply correctly", {
  expect_that(multiplySquares(2,3),
              equals(new("Squares", square=(36), x = 2, y = 3)))
  expect_that(multiplySquares(2,2),
              equals(new("Squares", square=(16), x = 2, y = 2)))
  expect_that(multiplySquares(4,2),
              equals(new("Squares", square=(64), x = 4, y = 2)))
})

context("multiplying squares correct classes")
test_that("squares multiply correctly", {
  expect_is(multiplySquares(6,9), "Squares")
  expect_is(multiplySquares(3,7), "Squares")
  expect_is(multiplySquares(4,2), "Squares")
})

context("multiplying squares should throw errors for non-numeric inputs")
test_that("squares multiply correctly", {
  expect_error(multiplySquares(Q))
  expect_error(multiplySquares("p"))
  expect_error(multiplySquares("test"))
})

context("dividing squares correctly")
test_that("squares divide correctly", {
  expect_that(divideSquares(2,3),
              equals(new("Squares", square=(36), x = 2, y = 3)))
  expect_that(divideSquares(2,2),
              equals(new("Squares", square=(16), x = 2, y = 2)))
  expect_that(divideSquares(4,2),
              equals(new("Squares", square=(64), x = 4, y = 2)))
})

context("dividing squares correct classes")
test_that("squares divide correctly", {
  expect_is(divideSquares(6,9), "Squares")
  expect_is(divideSquares(3,7), "Squares")
  expect_is(divideSquares(4,2), "Squares")
})

context("dividing squares should throw errors for non-numeric inputs")
test_that("squares divide correctly", {
  expect_error(divideSquares(Q))
  expect_error(divideSquares("p"))
  expect_error(divideSquares("test"))
})

context("squaring squares correctly")
test_that("squares square correctly", {
  expect_that(squareSquares(2,3),
              equals(new("Squares", square=(36), x = 2, y = 3)))
  expect_that(squareSquares(2,2),
              equals(new("Squares", square=(16), x = 2, y = 2)))
  expect_that(squareSquares(4,2),
              equals(new("Squares", square=(64), x = 4, y = 2)))
})

context("squaring squares correct classes")
test_that("squares square correctly", {
  expect_is(squareSquares(6,9), "Squares")
  expect_is(squareSquares(3,7), "Squares")
  expect_is(squareSquares(4,2), "Squares")
})

context("squaring squares should throw errors for non-numeric inputs")
test_that("squares square correctly", {
  expect_error(squareSquares(Q))
  expect_error(squareSquares("p"))
  expect_error(squareSquares("test"))
})