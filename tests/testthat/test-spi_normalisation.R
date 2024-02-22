test_that("spi_normalisation() works with non inverted indicators", {
  expect_equal(spi_normalisation(0,0,100,FALSE), 0)
  expect_equal(spi_normalisation(100,0,100,FALSE), 100)
  expect_equal(spi_normalisation(80,20.5,95.6,FALSE), 79.227696)
})

test_that("spi_normalisation() works with non inverted indicators", {
  expect_equal(spi_normalisation(100,0,100,TRUE), 100)
  expect_equal(spi_normalisation(0,0,100,TRUE), 0)
  expect_equal(spi_normalisation(70,25,90,FALSE), 69.2307692)
})
