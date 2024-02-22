test_that("spi_normalisation_m() works with official data", {
  expect_no_error(spi_normalisation_m("regional"))
  expect_snapshot(spi_normalisation_m("regional"))
  expect_snapshot(spi_normalisation_m("national"))
})

test_that("spi_normalisation_m() works with custom data", {
  expect_no_error(spi_normalisation_m("regional"))
  normalisation_data <- data.frame(indicators = c("Indicator 1", "Indicator 2", "Indicator 3"),
                                   inverted = c(TRUE, FALSE, TRUE),
                                   utopian = c(0,100,24),
                                   dystopian = c(100,0,72))
  raw_data <- data.frame(regions = c("Region A", "Region B"),
                         indicator1 = c(81, 76),
                         indicator2 = c(97, 96),
                         indicator3 = c(36, 24))
  expect_equal(spi_normalisation_m("regional", normalisation_data, raw_data),
                    data.frame(regions = c("Region A", "Region B"),
                                                    indicator1 = c(19, 24),
                                                    indicator2 = c(97, 96),
                                                    indicator3 = c(75, 100)))
})
