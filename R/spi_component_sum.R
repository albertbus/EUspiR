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
#' @param type Placeholder
#'
#' @examples
#' #data <- data.frame("Regions" <- c("Region A", "Region B", "Region C"),
#' #                   "Component1" <- sample(1:100, 3),
#' #                   "Component2" <- sample(1:100, 3),
#' #                   "Component3" <- sample(1:100, 3))
#' #spi_component_sum(data, "Dimension1")
#'
#' @export

#This function aggregates components following the EU-SPI (2020) methodology
#This function aggregates dimensions following the EU-SPI (2020) methodology
spi_component_sum <- function(type, dataSet = NULL, dimensionName = "all", b=0.5, component_index = NULL, append = TRUE){
  #Handle given data in case of potential errors
  stopifnot(is.character(type))
  if (!is.null(dataSet)) {stopifnot(is.data.frame(dataSet))}
  stopifnot(is.character(dimensionName))
  stopifnot(is.numeric(b))

  #Retrieve data if necessary
  if (is.null(dataSet)) {
    if (type == "regional dimension") {dataSet <- weighted_spi("all", append = FALSE)}
    if (type == "regional spi") {dataSet <- spi_component_sum("regional dimension", append = FALSE)}
    if (type == "national dimension") {dataSet <- spi_indicator_sum("national", append = FALSE)}
    if (type == "national spi") {dataSet <- spi_component_sum("national dimension", append = FALSE)}
  }

  #Set the dimension(s) name
  if (dimensionName == "all") {
    if (type == "regional dimension" | type == "national dimension") {
      dimensionName <- c("Basic Human Needs",
                         "Foundations of Well-Being",
                         "Opportunity")
    }
    if (type == "regional spi" | type == "national spi") {dimensionName <- "EU-SPI"}
  }

  #Set the dimension(s) index
  if (is.null(component_index)) {
    if (type == "regional dimension") {component_index <- list(c(4:7),c(8:11),c(12:15))}
    if (type == "regional spi") {component_index <- list(c(4:6))}
    if (type == "national dimension") {component_index <- list(c(3:6),c(7:10),c(11:14))}
    if (type == "national spi") {component_index <- list(c(3:5))}
  }

  result <- dataSet[sapply(dataSet, is.character)]

  for(n in 1:length(component_index)) {
    #First step: Retrieve data for the aggregation
    agg_data <- cbind(dataSet[which(sapply(dataSet, is.character))],
                      dataSet[component_index[[n]]])

    #Second step: elevate the values to b
    elevate_to_b <- agg_data[which(sapply(agg_data, is.numeric))[1]:ncol(agg_data)]^b

    #Third step: Sum by row (region)
    sum_row <- rowSums(elevate_to_b, na.rm = TRUE)

    #Fourth step: 1 divided by the number of indicators
    one_indicator_n <- 1/(rowSums(!is.na(agg_data)) - sum(sapply(agg_data, is.character)))

    #Fifth step: sum_row * one_indicator_n
    fifth_step <- sum_row*one_indicator_n

    #Sixth step: elevate the values to (1/b)
    agg_data[dimensionName[n]] <- fifth_step^(1/b)

    #Return the dimension(s)
    agg_data <- agg_data[sapply(agg_data, is.numeric)]
    if(append == TRUE){result <- cbind(result, agg_data)}
    else{result <- cbind(result, agg_data[ncol(agg_data)])}
  }

  #Retrieve the aggregated dimension
  return(result)
}
