test_that("style_gt_journal returns a gt table", {
  table <- summarize_plastics_prop()

  styled <- style_gt_journal(
    table = table,
    title = "Plastic Summary"
  )

  expect_true("gt_tbl" %in% class(styled))
})

test_that("style_gt_journal accepts color columns", {
  table <- summarize_plastics_prop()

  styled <- style_gt_journal(
    table = table,
    title = "Plastic Summary",
    color_cols = c("prop_change", "yoy_change")
  )

  expect_true("gt_tbl" %in% class(styled))
})

test_that("style_gt_journal accepts percent columns", {
  table <- summarize_plastics_prop()

  styled <- style_gt_journal(
    table = table,
    title = "Plastic Summary",
    percent_cols = c("prop_2020", "prop_change", "yoy_change")
  )

  expect_true("gt_tbl" %in% class(styled))
})

test_that("style_gt_journal rejects invalid title", {
  table <- summarize_plastics_prop()

  expect_error(
    style_gt_journal(
      table = table,
      title = c("A", "B")
    )
  )
})

test_that("style_gt_journal rejects invalid subtitle", {
  table <- summarize_plastics_prop()

  expect_error(
    style_gt_journal(
      table = table,
      title = "Title",
      subtitle = c("A", "B")
    )
  )
})
