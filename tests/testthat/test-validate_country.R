test_that("validate_country returns a character string", {
  result <- validate_country("China")

  expect_true(is.character(result))
})

test_that("validate_country returns the correctly capitalized country name", {
  result <- validate_country("china")

  expect_equal(result, "China")
})

test_that("validate_country ignores capitalization", {
  result <- validate_country("CHINA")

  expect_equal(result, "China")
})

test_that("validate_country rejects invalid countries", {
  expect_error(
    validate_country("NotACountry")
  )
})

test_that("validate_country rejects non-character input", {
  expect_error(
    validate_country(123)
  )
})

test_that("validate_country rejects vectors of country names", {
  expect_error(
    validate_country(
      c("China", "India")
    )
  )
})

test_that("validate_country rejects data missing a country column", {
  bad_data <- tibble::tibble(
    x = 1:5
  )

  expect_error(
    validate_country(
      country_name = "China",
      data = bad_data
    )
  )
})
