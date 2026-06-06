test_that("compare_country_plastic_types returns an htest object", {
  result <- suppressWarnings(compare_country_plastic_types(
    country_one = "China",
    country_two = "Canada"
  )
  )

  expect_true("htest" %in% class(result))
})

test_that("compare_country_plastic_types returns expected output names", {
  result <- suppressWarnings(compare_country_plastic_types(
    country_one = "China",
    country_two = "Canada"
  )
  )

  expect_true(
    all(
      c(
        "statistic",
        "parameter",
        "p.value",
        "method",
        "data.name",
        "observed",
        "expected",
        "residuals",
        "stdres"
      ) %in% names(result)
    )
  )
})

test_that("compare_country_plastic_types ignores country capitalization", {
  result <- suppressWarnings(compare_country_plastic_types(
    country_one = "china",
    country_two = "CANADA"
  )
  )
  expect_true("htest" %in% class(result))
})

test_that("compare_country_plastic_types rejects same countries", {
  expect_error(
    compare_country_plastic_types(
      country_one = "China",
      country_two = "china"
    )
  )
})

test_that("compare_country_plastic_types rejects invalid country_one", {
  expect_error(
    compare_country_plastic_types(
      country_one = "NotACountry",
      country_two = "China"
    )
  )
})

test_that("compare_country_plastic_types rejects invalid country_two", {
  expect_error(
    compare_country_plastic_types(
      country_one = "China",
      country_two = "NotACountry"
    )
  )
})

test_that("compare_country_plastic_types rejects data missing required columns", {
  bad_data <- tibble::tibble(
    country = c("A", "B")
  )

  expect_error(
    compare_country_plastic_types(
      data = bad_data,
      country_one = "A",
      country_two = "B"
    )
  )
})
