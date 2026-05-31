test_that("clean_plastics_data returns a data frame", {
  result <- clean_plastics_data()

  expect_true(is.data.frame(result))
})

test_that("clean_plastics_data removes Grand Total rows", {
  result <- clean_plastics_data()

  expect_true(!("Grand Total" %in% result$parent_company))
})

test_that("clean_plastics_data removes EMPTY countries when requested", {
  result <- clean_plastics_data(remove_empty_country = TRUE)

  expect_true(!("EMPTY" %in% result$country))
})

test_that("clean_plastics_data catches missing required columns", {
  bad_data <- data.frame(
    year = c(2019, 2020),
    grand_total = c(10, 20)
  )

  expect_error(
    clean_plastics_data(data = bad_data)
  )
})

test_that("clean_plastics_data catches invalid remove_empty_country", {
  expect_error(
    clean_plastics_data(remove_empty_country = "yes")
  )

  expect_error(
    clean_plastics_data(remove_empty_country = c(TRUE, FALSE))
  )
})
