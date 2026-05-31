test_that("summarize_plastics returns a tibble", {
  result <- summarize_plastics()

  expect_true(is.data.frame(result))
})

test_that("summarize_plastics returns expected columns for both", {
  result <- summarize_plastics(type = "both")

  expect_true(
    all(
      c(
        "plastic_type",
        "prop_2020",
        "prop_change",
        "yoy_change"
      ) %in% names(result)
    )
  )
})

test_that("summarize_plastics returns expected columns for prop", {
  result <- summarize_plastics(type = "prop")

  expect_true(
    all(
      c("plastic_type", "prop_change") %in%
        names(result)
    )
  )
})

test_that("summarize_plastics returns expected columns for yoy", {
  result <- summarize_plastics(type = "yoy")

  expect_true(
    all(
      c("plastic_type", "yoy_change") %in%
        names(result)
    )
  )
})

test_that("summarize_plastics rejects invalid type", {
  expect_error(
    summarize_plastics(type = "bad")
  )
})
