test_that("spi_rawdata_down() regional data downloads", {
  expect_no_error(spi_rawdata_down("regional"))
  expect_snapshot(spi_rawdata_down("regional"))
})

test_that("spi_rawdata_down() national data downloads", {
  expect_no_error(spi_rawdata_down("national"))
  expect_snapshot(spi_rawdata_down("national"))
})

test_that("spi_rawdata_down() normalisation data downloads", {
  expect_no_error(spi_rawdata_down("normalisation"))
  expect_snapshot(spi_rawdata_down("normalisation"))
})
