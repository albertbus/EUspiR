test_that("weighted_spi() works", {
  data <- data.frame("NUTS_ID" = c("AT11", "AT12", "AT13", "AT21", "AT22", "AT31", "AT32", "AT33", "AT34"),
                     "Component1" = c(70, 65, 50, 48, 78, 80, 39, 67, 78))
  nat_data <- data.frame("NUTS_ID" = "AT", "Component1" = 60, "Component2" = 69, "Component3" = 80)
  expect_no_error(weighted_spi("AT", data, nat_data))
  expect_equal(weighted_spi("AT", data, nat_data),
               data.frame("NUTS_ID" = c("AT11", "AT12", "AT13", "AT21", "AT22", "AT31", "AT32", "AT33", "AT34"),
                          "Component1" = c(65.66744, 60.66744, 45.66744, 43.66744, 73.66744, 75.66744, 34.66744, 62.66744, 73.66744)),
               tolerance = 0.001)
})
