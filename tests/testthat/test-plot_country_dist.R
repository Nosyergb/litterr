test_that("plot_country_dist returns a ggplot", {
  plot <- plot_country_dist()

  expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_country_dist accepts custom arguments", {
  plot <- plot_country_dist(
    label_n = 3,
    label_largest_increase = TRUE,
    point_color = "steelblue",
    point_size = 4,
    point_alpha = 0.5
  )

  expect_true(ggplot2::is_ggplot(plot))
})

test_that("plot_country_dist rejects invalid label_n", {
  expect_error(
    plot_country_dist(label_n = -1)
  )
})

test_that("plot_country_dist rejects invalid point_alpha", {
  expect_error(
    plot_country_dist(point_alpha = 2)
  )
})

test_that("plot_country_dist rejects invalid point_size", {
  expect_error(
    plot_country_dist(point_size = -1)
  )
})
