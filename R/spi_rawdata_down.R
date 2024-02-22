#' Download EU-SPI (2020) Official Data
#' @description
#' Download the official raw and nomralisation data used for the creation of the
#' Eu-SPI (2020) as presented by the European Commision in the offical web page.
#'
#' @references Source of the indicator data: https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx
#' @references Source of the normalisation data: https://ec.europa.eu/regional_policy/sources/work/202006_spi_en.pdf
#'
#' @param type Variable that accepts the following values:
#' - "regional", to download the regional raw indicator data;
#' - "national", to download the national raw indicator data;
#' - "normalisation", to download the normalisation data.
#'
#' @returns For the raw data, a data frame with the values for each indicator and
#' each NUTS-2 region or NUTS-0 country. For the normalisation data, a data frame
#' with the utopian and dystopian values and wether the indicator is inverted for
#' each indicator of the EU-SPI (2020).
#'
#' @importFrom utils download.file read.csv
#'
#' @export

spi_rawdata_down <- function(type) {
  # Handling the given data for potential errors
  stopifnot(is.character(type))
  type <- tolower(type)

  # Downloading regional data
  if (type == "regional") {
    # Downloading the needed data from the European Commision
    url <- "https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx"
    download.file(url, destfile = paste0(tempdir(), "spi2020_raw_data.xlsx"), mode = "wb")

    # First regional sheet (Basic Regional Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Basic_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[4:ncol(data)] <- lapply(data[1:240, 4:ncol(data)], as.numeric)
    spi2020_regional_raw <- data

    # Second regional sheet (Foundations Regional Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Foundations_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[4:ncol(data)] <- lapply(data[1:240, 4:ncol(data)], as.numeric)
    spi2020_regional_raw <- cbind(spi2020_regional_raw, data[4:ncol(data)])

    # Third regional sheet (Opportunity Regional Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Opportunity_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[4:ncol(data)] <- lapply(data[1:240, 4:ncol(data)], as.numeric)
    spi2020_regional_raw <- cbind(spi2020_regional_raw, data[4:ncol(data)])

    # Returning the resulting data frame
    file.remove(paste0(tempdir(), "spi2020_raw_Data.xlsx"))
    return(spi2020_regional_raw)
  }

  # Downloading national data
  if (type == "national") {
    # Downloading the needed data from the European Commision
    url <- "https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx"
    download.file(url, destfile = paste0(tempdir(), "spi2020_raw_data.xlsx"), mode = "wb")

    # First national sheet (Basic National Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Basic_national_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[3:ncol(data)] <- lapply(data[1:27, 3:ncol(data)], as.numeric)
    spi2020_national_raw <- data

    # Second national sheet (Basic National Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Foundations_national_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[3:ncol(data)] <- lapply(data[1:27, 3:ncol(data)], as.numeric)
    spi2020_national_raw <- cbind(spi2020_national_raw, data[3:ncol(data)])

    # Third national sheet (Opportunity National Data)
    data <- readxl::read_excel(paste0(tempdir(), "spi2020_raw_data.xlsx"), sheet = "Opportunity_national_data", .name_repair = "minimal")
    colnames(data) <- data[4, ]
    data <- data[5:nrow(data), ]
    data[3:ncol(data)] <- lapply(data[1:27, 3:ncol(data)], as.numeric)
    spi2020_national_raw <- cbind(spi2020_national_raw, data[3:ncol(data)])

    # Returning the resulting data frame
    file.remove(paste0(tempdir(), "spi2020_raw_Data.xlsx"))
    return(spi2020_national_raw)
  }

  # Downloading normalisation data
  if (type == "normalisation") {
    # Download the needed data from GitHub
    file_url <- "https://raw.githubusercontent.com/albertbus/EUspiR/master/data/spi_normalisation_data.csv"
    download.file(file_url, paste0(tempdir(), "norm_data.csv"))

    # Return the csv file in R
    data <- read.csv(paste0(tempdir(), "norm_data.csv"))
    file.remove(paste0(tempdir(), "norm_data.csv"))
    return(data)
  }
}
