test_that("summarize_country_totals returns a tibble", {
  result <- summarize_country_totals()

  expect_true(is.data.frame(result))
})

test_that("summarize_country_totals returns expected columns", {
  result <- summarize_country_totals()

  expect_true(
    all(
      c(
        "country",
        "total_plastic"
      ) %in% names(result)
    )
  )
})

test_that("summarize_country_totals removes EMPTY countries", {
  result <- summarize_country_totals()

  expect_false(
    "EMPTY" %in% result$country
  )
})

test_that("summarize_country_totals returns nonnegative totals", {
  result <- summarize_country_totals()

  expect_true(
    all(result$total_plastic >= 0)
  )
})

test_that("summarize_country_totals rejects data missing required columns", {
  bad_data <- tibble::tibble(
    country = c("A", "B")
  )

  expect_error(
    summarize_country_totals(
      data = bad_data
    )
  )
})
