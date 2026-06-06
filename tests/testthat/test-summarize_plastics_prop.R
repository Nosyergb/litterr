test_that("summarize_plastics_prop returns a tibble", {
  result <- summarize_plastics_prop()

  expect_true(is.data.frame(result))
})

test_that("summarize_plastics_prop returns expected columns for both", {
  result <- summarize_plastics_prop(type = "both")

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

test_that("summarize_plastics_prop returns expected columns for prop", {
  result <- summarize_plastics_prop(type = "prop")

  expect_true(
    all(
      c("plastic_type", "prop_change") %in%
        names(result)
    )
  )
})

test_that("summarize_plastics_prop returns expected columns for yoy", {
  result <- summarize_plastics_prop(type = "yoy")

  expect_true(
    all(
      c("plastic_type", "yoy_change") %in%
        names(result)
    )
  )
})

test_that("summarize_plastics_prop rejects invalid type", {
  expect_error(
    summarize_plastics_prop(type = "bad")
  )
})
