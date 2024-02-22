test_that("spi_rawdata_down() regional data downloads", {
  expect_no_error(spi_rawdata_down("REGIONAL"))
  expect_snapshot(spi_rawdata_down("REGIONAL"))
})

test_that("spi_rawdata_down() national data downloads", {
  expect_no_error(spi_rawdata_down("NATIONAL"))
  expect_snapshot(spi_rawdata_down("NATIONAL"))
})
