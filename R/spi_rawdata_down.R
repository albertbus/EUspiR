#' Download the EU-SPI (2020) Raw Data
#' @description
#' This downloads the official raw data used for the creation of the EU-SPI (2020)
#' as presented in the European Commission page. It returns a data frame with the
#' values for each indicator and each region at NUTS-2 level or country at NUTS-0.
#' Additionaly, it returns the official normalisation data.
#'
#' @references Source of the indicator data: https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx
#' @references Source of the normalisation data: https://ec.europa.eu/regional_policy/sources/work/202006_spi_en.pdf
#'
#' @param type Variable that accepts the values "REGIONAL" to download the regional
#' data or "NATIONAL to download the national data. It can also take the value
#' "NORM" to provide the normalisation data from the GitHub of the EUspiR
#' package, made from the European Commision data.
#'
#' @returns A data frame with the chosen data type
#' @importFrom utils download.file read.csv
#' @export

spi_rawdata_down <- function(type) {
  #Module 1: Installation of the required packages
  #requireNamespace("readxl", quietly = TRUE)
  #if (!requireNamespace("readxl",quietly = TRUE)) {install.packages("readxl")}

  #Module 2: Regional Database
  if (type == "REGIONAL") {
    #0: Downloading the needed data from the European Commision
    url <- "https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx"
    download.file(url, destfile = paste0(tempdir(),"spi2020_raw_data.xlsx"), mode = "wb")
    #1: First regional sheet (Basic Regional Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Basic_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[4:ncol(data)] <- lapply(data[1:240,4:ncol(data)],as.numeric)
    spi2020_regional_raw <- data
    #2: Second regional sheet (Foundations Regional Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Foundations_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[4:ncol(data)] <- lapply(data[1:240,4:ncol(data)],as.numeric)
    spi2020_regional_raw <- cbind(spi2020_regional_raw, data[4:ncol(data)])
    #3: Third regional sheet (Opportunity Regional Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Opportunity_regional_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[4:ncol(data)] <- lapply(data[1:240,4:ncol(data)],as.numeric)
    spi2020_regional_raw <- cbind(spi2020_regional_raw, data[4:ncol(data)])
    return(spi2020_regional_raw)
  }
  #Module 3: National Database
  if (type == "NATIONAL") {
    #0: Downloading the needed data from the European Commision
    url <- "https://ec.europa.eu/regional_policy/sources/work/spi2020_raw_data.xlsx"
    download.file(url, destfile = paste0(tempdir(),"spi2020_raw_data.xlsx"), mode = "wb")
    #1: First national sheet (Basic National Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Basic_national_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[3:ncol(data)] <- lapply(data[1:27,3:ncol(data)],as.numeric)
    spi2020_national_raw <- data
    #2: Second national sheet (Basic National Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Foundations_national_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[3:ncol(data)] <- lapply(data[1:27,3:ncol(data)],as.numeric)
    spi2020_national_raw <- cbind(spi2020_national_raw,data[3:ncol(data)])
    #3: Third national sheet (Opportunity National Data)
    data <- readxl::read_excel(paste0(tempdir(),"spi2020_raw_data.xlsx"), sheet = "Opportunity_national_data", .name_repair = "minimal")
    colnames(data) <- data[4,]
    data <- data[5:nrow(data),]
    data[3:ncol(data)] <- lapply(data[1:27,3:ncol(data)],as.numeric)
    spi2020_national_raw <- cbind(spi2020_national_raw,data[3:ncol(data)])
    return(spi2020_national_raw)
  }
  if (type == "NORM") {
    #0: Download the needed data from GitHub
    file_url <- "https://raw.githubusercontent.com/albertbus/EUspiR/master/data/spi_normalisation_data.csv"
    download.file(file_url, paste0(tempdir(),"norm_data"))
    #1: Return the csv file in R
    data <- read.csv(paste0(tempdir(),"norm_data"))
    return(data)
  }
}
