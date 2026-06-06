test_that("animate_country_plastics runs successfully", {
  expect_no_error(
    animate_country_plastics(
      country_name = "China",
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics accepts custom arguments", {
  expect_no_error(
    animate_country_plastics(
      country_name = "China",
      colors = rainbow(7),
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics ignores country capitalization", {
  expect_no_error(
    animate_country_plastics(
      country_name = "china",
      nframes = 5,
      fps = 2
    )
  )

  expect_no_error(
    animate_country_plastics(
      country_name = "CHINA",
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects invalid country names", {
  expect_error(
    animate_country_plastics(
      country_name = "NotACountry",
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects non-character country_name", {
  expect_error(
    animate_country_plastics(
      country_name = 123,
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects colors vectors of wrong length", {
  expect_error(
    animate_country_plastics(
      country_name = "China",
      colors = c("red", "blue"),
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects non-character colors", {
  expect_error(
    animate_country_plastics(
      country_name = "China",
      colors = 1:7,
      nframes = 5,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects invalid nframes", {
  expect_error(
    animate_country_plastics(
      country_name = "China",
      nframes = -1,
      fps = 2
    )
  )
})

test_that("animate_country_plastics rejects invalid fps", {
  expect_error(
    animate_country_plastics(
      country_name = "China",
      nframes = 5,
      fps = -1
    )
  )
})
