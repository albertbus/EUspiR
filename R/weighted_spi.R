#' Calculation of the weighted components within EU-SPI (2020)
#' @description
#' Calculate the weighted value of a given component with the population data of
#' the NUTS-2 regions, according to the EU-SPI (2020) methodology.
#' @param unweighted_values Data frame with the values to weight of the chosen
#' component. See details for more information about the format of the data frame.
#' @param country NUTS-0 code of the country to weight. For several countries,
#' country should be a vector of the NUTS-0 codes to be used. It can also take the value
#' "all" so that it is applied to each EU country at once.
#' @param nationalValue Data frame with the national values to weight for the chosen
#' component. See details for more information about the format of the data frame.
#'
#' @details
#' The parameters unweighted_values and nationalValue dataframe should have the following
#' format:
#' - First column(s) should be of type character. There is no rules on how many
#' columns should the data frame have of this type but the function expects at
#' least one to be named "NUTS_ID" and have the NUTS-2 or NUT-S0 code for each
#' region on each row.
#' - The following columns should be of type numeric. Each column should have one
#' component to be weighted and each row should represent one region or country.
#'
#' @examples
#' data <- data.frame("NUTS_ID" = c("AT11", "AT12", "AT13", "AT21",
#'                                  "AT22", "AT31", "AT32", "AT33", "AT34"),
#'                    "Component1" = sample(1:100,9),
#'                    "Component2" = sample(1:100,9),
#'                    "Component3" = sample(1:100,9))
#' national_val <- data.frame("NUTS_ID" = "AT",
#'                            "Component1" = 52,
#'                            "Component2" = 49,
#'                            "Component3" = 55)
#' weighted_spi(data, "AT", national_val)
#'
#' @importFrom eurostat get_eurostat
#' @importFrom stats aggregate weighted.mean
#'
#' @export

weighted_spi <- function(country, unweighted_values = NULL, nationalValue = NULL){
  #Handling of given data in case of potential errors
  if (!is.null(unweighted_values)) {stopifnot(is.data.frame(unweighted_values))}
  if (!is.null(nationalValue)) {stopifnot(is.data.frame(nationalValue))}
  stopifnot(is.character(country))
  country <- tolower(country)
  if (country != "all") {stopifnot(nchar(country[1]) == 2)
    country <- toupper(country)}

  #Retrieving the data if needed
  if (is.null(unweighted_values)) {unweighted_values <- spi_indicator_sum("regional")}
  if (is.null(nationalValue)) {unweighted_values <- spi_indicator_sum("national")}

  #Creation of the data base of population and countries
  population <- eurostat::get_eurostat(id = "tgs00096", filters = list(time = c(2017:2019)))[c(5,7)]
  if (country == "all") {
    population <- subset(population, !grepl("AL|CH|HR02|HR05|HR06|IS|XX|ME|MK|LI|NO|RS", population$geo))
    population <- subset(population, !grepl("AL|CH|HR02|HR05|HR06|IS|XX|ME|MK|LI|NO|RS", population$geo))
    population <- population[order(population$geo),][1:720,]
    }
  else {
    country <- paste(country, collapse = "|")
    population <- subset(population, grepl(paste0("^", country), population$geo))
    }
  population <- aggregate(values ~ geo, data = population, mean)
  countries <- unique(substr(population$geo, start = 1, stop = 2))

  # Application of the formula to each component
  weighted_data <- unweighted_values[which(sapply(unweighted_values, is.character))]
  for (component in (which(sapply(unweighted_values, is.numeric)))) {
    #A pplication of the formula to each corresponding country
    result <- c()
    for (c in countries) {
      weights <- population[grepl(paste0("^", c), population$geo),]
      unweighted <- unweighted_values[grepl(paste0("^", c), unweighted_values$NUTS_ID), component]
      national <- nationalValue[grepl(c, nationalValue$NUTS_ID), component]
      #Application of the formula to each corresponding region
      reg_result <- c()
      for (r in 1:nrow(weights)) {
        w_mean <- weighted.mean(unweighted, weights$values, na.rm = TRUE)
        reg_result <- c(reg_result, (national + unweighted[r] - w_mean))
      }
      result <- c(result, reg_result)
    }
    weighted_data <- cbind(weighted_data, result)
  }
  colnames(weighted_data) <- colnames(unweighted_values)
  return(weighted_data)
}


grepl("AT|ES", test$Country)
test <- c("AT","ES","BE")
test2 <- paste(test, collapse = "|")
