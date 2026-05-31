test_that("style_gt_journal returns a gt table", {

  table <- summarize_plastics() |>
    gt::gt()

  styled <- style_gt_journal(
    table = table,
    title = "Plastic Summary"
  )

  expect_true("gt_tbl" %in% class(styled))
})

test_that("style_gt_journal accepts color columns", {

  table <- summarize_plastics() |>
    gt::gt()

  styled <- style_gt_journal(
    table = table,
    title = "Plastic Summary",
    color_col = c("prop_change", "yoy_change")
  )

  expect_true("gt_tbl" %in% class(styled))
})

test_that("style_gt_journal rejects invalid title", {

  table <- summarize_plastics() |>
    gt::gt()

  expect_error(
    style_gt_journal(
      table = table,
      title = c("A", "B")
    )
  )
})

test_that("style_gt_journal rejects invalid subtitle", {

  table <- summarize_plastics() |>
    gt::gt()

  expect_error(
    style_gt_journal(
      table = table,
      title = "Title",
      subtitle = c("A", "B")
    )
  )
})
