test_that("load_data returns a data frame", {
  data <- load_data()

  expect_true(is.data.frame(data))
  expect_true(nrow(data) > 0)
})

test_that("load_data contains expected columns", {
  data <- load_data()

  expect_true(
    all(
      c("country", "year", "parent_company", "grand_total") %in%
        names(data)
    )
  )
})
