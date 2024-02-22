#' Component or dimension aggregation within EU-SPI (2020)
#' @description
#' Aggregate components or dimensions following the EU-SPI (2020) methodology.
#'
#' @param dataSet Data frame of the components or dimensions to aggregate. The values
#' should be numerical.
#' @param dimensionName Name of the dimension or index that is to be retrieved
#' as the result of the aggregation.
#' @param b β value, a constant that can be adjusted to change the level of
#' compensability of the index. From β = 1 for the arithemtic mean to β = 0 for
#' the geometric mean. The default value is β = 0.5.
#'
#' @examples
#' data <- data.frame("Regions" <- c("Region A", "Region B", "Region C"),
#'                    "Component1" <- sample(1:100, 3),
#'                    "Component2" <- sample(1:100, 3),
#'                    "Component3" <- sample(1:100, 3))
#' spi_component_sum(data, "Dimension1")
#'
#' @export

#This function aggregates components following the EU-SPI (2020) methodology
#This function aggregates dimensions following the EU-SPI (2020) methodology
spi_component_sum <- function(dataSet,dimensionName,b=0.5){
  #Handle given data in case of potential errors
  stopifnot(is.data.frame(dataSet))
  stopifnot(is.character(dimensionName))
  stopifnot(is.numeric(b))

  #First step: elevate the values to b
  elevate_to_b <- dataSet[which(sapply(dataSet, is.numeric))[1]:ncol(dataSet)]^b

  #Second step: Sum by row (region)
  sum_row <- rowSums(elevate_to_b, na.rm = TRUE)

  #Third step: 1 divided by the number of indicators
  one_indicator_n <- 1/(rowSums(!is.na(dataSet)) - sum(sapply(dataSet,is.character)))

  #Fourth step: sum_row * one_indicator_n
  fourth_step <- sum_row*one_indicator_n

  #Fifth step: elevate the values to (1/b)
  dataSet[dimensionName] <- fourth_step^(1/b)

  #Retrieve the aggregated dimension
  return(dataSet)
}
