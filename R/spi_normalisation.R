#' Normalisation function within EU-SPI (2020)
#' @description
#' Normalise the data accordin to the methodology seen in the EU-SPI(2020). It
#' only works with single values, not matrices of data.
#'
#' @param val Value that needs to be normalised
#' @param dis_val Dystopic value of the indicator
#' @param uto_val Utopic value of the indicator
#' @param inverted Logical value. Does the indicator have a positive relation
#' with the index (inverted = FALSE) or a negative relation with the index
#' (inverted = TRUE)
#'
#' @examples
#' spi_normalisation(20,0,100,TRUE)
#' spi_normalisation(10,100,0,FALSE)
#'
#' @returns Normalised numeric value.
#'
#' @export

#This function normalises a value following the EU-SPI (2020) methodology
spi_normalisation <- function(val,dis_val,uto_val,inverted){
  if(inverted==TRUE) return(((-100*(val-uto_val))/(dis_val-uto_val))+100)
  if(inverted==FALSE) return((100*(val-dis_val))/(uto_val-dis_val))
}
