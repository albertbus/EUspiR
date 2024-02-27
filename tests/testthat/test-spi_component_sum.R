# test_that("spi_component_sum() works with default b", {
#   data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
#                      "Component1" = c(24, 51, 88),
#                      "Component2" = c(90, 29, 73),
#                      "Component3" = c(85, 39, 62))
#   expect_equal(spi_component_sum(data, "Dimension1"),
#                data.frame("Regions" = c("Region A", "Region B", "Region C"),
#                           "Component1" = c(24, 51, 88),
#                           "Component2" = c(90, 29, 73),
#                           "Component3" = c(85, 39, 62),
#                           "Dimension1" = c(61.912, 39.152, 73.953)),
#                tolerance = 0.001)
# })
#
# test_that("spi_component_sum() works with custom b", {
#   data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
#                      "Component1" = c(35, 14, 60),
#                      "Component2" = c(41, 76, 62),
#                      "Component3" = c(88, 96, 97))
#   expect_equal(spi_component_sum(data, "Dimension1", b = 0.8),
#                data.frame("Regions" = c("Region A", "Region B", "Region C"),
#                           "Component1" = c(35, 14, 60),
#                           "Component2" = c(41, 76, 62),
#                           "Component3" = c(88, 96, 97),
#                           "Dimension1" = c(53.710, 59.420, 72.624)),
#                tolerance = 0.001)
# })
#
# test_that("spi_component_sum() is sensible to b", {
#   data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
#                      "Component 1" = c(35, 14, 60),
#                      "Component 2" = c(41, 76, 62),
#                      "Component 3" = c(88, 96, 97))
#   expect_failure(expect_equal(spi_component_sum(data, "Dimension1"),
#                               spi_component_sum(data, "Dimension1", b = 0.8)))
# })
