#' Regional Map Creation at the NUTS-2 level
#' @description
#' Create a map for a specific region. Helper function for the europe_map_creation()
#' function inside the EUspiR package.
#'
#' @param map_data Data from which the map should be created.
#' @param type Variable that can take the values of "NAT" if it is the National
#' or continental level or the name of the extra region to create such as
#' "Martinique" or "Canary Islands".
#' @param global_map Optional parameter, only used when type = "NAT", it is used
#' as the background mute global map
#' @param labs_plot Vector of labels to be used in the scale of the map
#' @param var_name Vector of values to be used as the scale of the map
#'
#' @keywords internal

region_map_creation <- function(map_data, type = "NAT", global_map, pal, labs_plot, var_name) {
  bbox <- sf::st_bbox(map_data)
  if (type == "NAT") {
    map_result <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = global_map,
                       color = "white", fill = "lightgrey", size = 1) +
      ggplot2::geom_sf(data = map_data,
                       color = "white", size = 0.2, ggplot2::aes(fill = map_data$values_cut))
  }
  else {
    map_result <- ggplot2::ggplot(data = map_data) +
      ggplot2::geom_sf(color = "white", size = 0.2, ggplot2::aes(fill = map_data$values_cut)) +
      ggplot2::labs(caption = type) +
      ggplot2::theme(legend.position = "none",
                     text = ggplot2::element_text(family = "serif"),
                     plot.caption = ggplot2::element_text(hjust = 0.5, size = 5))
    }
  map_result <- map_result +
    ggplot2::scale_fill_manual(
      name = var_name,
      labels = labs_plot,
      drop = FALSE,
      values = pal) +
    ggplot2::coord_sf(xlim = c(bbox$xmin,bbox$xmax),
                      ylim = c(bbox$ymin,bbox$ymax),
                      label_axes = "")
  return(map_result)
}
