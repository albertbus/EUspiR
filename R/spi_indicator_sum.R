#' Aggregation of indicators within the EU-SPI (2020)
#' @description
#' Aggregate a set of indicators as seen in the EU-SPI (2020) methodology
#'
#' @param indicators Data frame with the values of each indicator (columns) for each
#' region (rows)
#' @param componentName Name of the component that is to be obtained as the final
#' result of the aggregation
#'
#' @examples
#' data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
#'                    "Indicator 1" = sample(1:100,3),
#'                    "Indicator 2" = sample(1:100,3),
#'                    "Indicator 3" = sample(1:100,3))
#' spi_indicator_sum(data,"Component 1")
#'
#' @export

spi_indicator_sum <- function(indicators, componentName){
  #Handle the data given in case of potential errors
  stopifnot(is.character(componentName))
  stopifnot(is.data.frame(indicators))

  #Apply the aggregation formula
  component <- indicators
  component[componentName] <- rowMeans(component[(which(sapply(component,is.numeric))[1]):ncol(component)],na.rm=TRUE)
  return(component)
}
