test_that("spi_indicator_sum() works", {
  data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
                     "Indicator1" = c(8, 61, 92),
                     "Indicator2" = c(99, 5, 30),
                     "Indicator3" = c(90, 31, 71))

  expect_equal(spi_indicator_sum("regional", data, "Component1"),
               data.frame("Regions" = c("Region A", "Region B", "Region C"),
                          "Indicator1" = c(8, 61, 92),
                          "Indicator2" = c(99, 5, 30),
                          "Indicator3" = c(90, 31, 71),
                          "Component1" = c(65.666, 32.333, 64.333)),
               tolerance = 0.001)
})
