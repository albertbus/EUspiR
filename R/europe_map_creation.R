#' European Maps at the NUTS-2 Level
#' @description
#' Create European maps at the NUTS-2 level. Adjust to visualise only continental
#' europe, adding the extra regions, or only one country.
#'
#' @param map_data Data frame with two columns. The first should have the NUTS-2
#' codes for the regions to be visualised, the second should have the values for
#' each region
#' @param type A variable that takes the values "CONTINENTAL" to visualise only
#' continental europe, "ALL" to visualise all territories including extraregions,
#' and a NUTS-0 country code to visualise a map of that country
#' @param title_t Text to be displayed as the title of the graph
#' @param subtitle_t Text to be displayed as the subtitle of the graph
#' @param caption_t Text to be displayed as the caption of the graph
#' @param var_name Text to be displayed as the title of the legend
#' @param col_pal Name of the palette to be used when making the graph, acording
#' to the HCL colour palettes available in the grDevices package
#'
#' @importFrom grDevices hcl.colors
#' @importFrom stats quantile
#'
#' @export

europe_map_creation <- function(map_data,type="ALL",title_t="",subtitle_t="",caption_t="",var_name="Legend",col_pal="ag_GrnYl"){
  # Handle the given data for potential errors
  stopifnot(!is.null(map_data))
  stopifnot(is.character(type))
  type <- tolower(type)
  if (type != "all" & type != "continental") {type <- toupper(type)}
  showtext::showtext_begin()

  # Creation of the european NUTS-2 map with GISCO data
  base_eu <- giscoR::gisco_get_nuts(
    year = "2016",
    resolution = "10",
    nuts_level = c("2")
  )

  # Creation of the global mute map
  global_map <- rnaturalearth::ne_countries(scale=10)

  # Regions to exclude (extraterritorial or non EU members) for the continental mute map
  exclude_values <- c("ES70", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30",
                      "AL", "CH", "IS", "LI", "ME", "MK", "NO", "RS", "TR", "UK")

  # Subset of the map data without the excluded values
  continental_eu_map <- subset(base_eu, !(base_eu$NUTS_ID %in% exclude_values | base_eu$CNTR_CODE %in% exclude_values))

  #Definition of the extraterritorial map data
  canary_map <- subset(base_eu, base_eu$NUTS_ID == "ES70") #Canary
  guadeloupe_map <- subset(base_eu, base_eu$NUTS_ID == "FRY1") #Guadeloupe
  martinique_map <- subset(base_eu, base_eu$NUTS_ID == "FRY2") #Martinique
  guyane_map <- subset(base_eu, base_eu$NUTS_ID == "FRY3") #French Guyane
  reunion_map <- subset(base_eu, base_eu$NUTS_ID == "FRY4") #La Réunion
  mayotte_map <- subset(base_eu, base_eu$NUTS_ID == "FRY5") #Mayotte
  acores_map <- subset(base_eu, base_eu$NUTS_ID == "PT20") #Les Açores
  madeira_map <- subset(base_eu, base_eu$NUTS_ID == "PT30") #Madeira

  # Preparation of the given data
  colnames(map_data) <- c("NUTS_ID","value")
  if (type != "all" & type != "continental") {
    map_data <- map_data[grepl(paste0("^",type), map_data$NUTS_ID),]
  }

  # Preparation of the legend and colour palette of the map
  deciles <- c(0,quantile(map_data$value, probs = seq(0, 1, by = 0.1)))
  map_data$values_cut <- cut(map_data$value,deciles,dig.lab=7)
  pal <- hcl.colors(length(deciles)-1, col_pal)
  labs_plot <- prettyNum(deciles[-1], digits = 4)
  labs_plot <- as.character(labs_plot)

  # Creation of the continental map
  continental_eu_map <- merge(continental_eu_map,map_data,by="NUTS_ID",all=FALSE)
  map_europe <- region_map_creation(map_data = continental_eu_map, global_map = global_map,
                                    pal = pal, labs_plot = labs_plot, var_name = var_name)

  # Creation of the extraterritorial Spanish map
  if (type == "ES" | type == "all") {
    canary_map <- merge(canary_map,map_data,by="NUTS_ID",all=FALSE)
    map_canary <- region_map_creation(map_data = canary_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Canary Islands")
    }

  # Creation of the extraterritorial French map
  if (type == "FR" | type == "all") {
    guadeloupe_map <- merge(guadeloupe_map,map_data,by="NUTS_ID",all=FALSE)
    martinique_map <- merge(martinique_map,map_data,by="NUTS_ID",all=FALSE)
    guyane_map <- merge(guyane_map,map_data,by="NUTS_ID",all=FALSE)
    reunion_map <- merge(reunion_map,map_data,by="NUTS_ID",all=FALSE)
    mayotte_map <- merge(mayotte_map,map_data,by="NUTS_ID",all=FALSE)
    #1: Guadeloupe
    map_guadeloupe <- region_map_creation(map_data = guadeloupe_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Guadeloupe")

    #2: Martinique
    map_martinique <- region_map_creation(map_data = martinique_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Martinique")
    #3: Guyane
    map_guyane <- region_map_creation(map_data = guyane_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Guyane")
    #4: La Réunion
    map_reunion <- region_map_creation(map_data = reunion_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "La R\u00e9union")
    #5: Mayotte
    map_mayotte <- region_map_creation(map_data = mayotte_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Mayotte")
    #6: Extraterritorial France
    extra_france_map <- cowplot::plot_grid(map_guadeloupe,map_martinique,map_guyane,map_reunion,map_mayotte)
  }

  # Creation of the extraterritorial Portuguese map
  if (type == "PT" | type == "all") {
    acores_map <- merge(acores_map,map_data,by="NUTS_ID",all=FALSE)
    madeira_map <- merge(madeira_map,map_data,by="NUTS_ID",all=FALSE)
    #1: Açores
    map_acores <- region_map_creation(map_data = acores_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "A\u00e7ores")
    #2: Madeira
    map_madeira <- region_map_creation(map_data = madeira_map, pal = pal, labs_plot = labs_plot,
                                      var_name = var_name, type = "Madeira")
    #6: Extraterritorial Portugal
    extra_portugal_map <- cowplot::plot_grid(map_acores,map_madeira,ncol=1)
  }

  # Joining the maps
  map <- map_europe
  if (type == "ES") {
    design <- "
      1222
      1222"
    map <- patchwork::wrap_plots(map_canary, map_europe, design = design)
  }
  if (type == "FR") {
    design <- "
      12666
      34666
      5#666"
    map <- patchwork::wrap_plots(map_guadeloupe, map_martinique, map_guyane,
                                 map_reunion, map_mayotte, map_europe, design = design)
  }
  if (type == "PT") {
    design <- "
      1333
      2333
      2333"
    map <- patchwork::wrap_plots(map_acores, map_madeira, map_europe, design = design)
  }
  if (type == "ALL") {
    design <- "
      127999999
      347999999
      566999999
      888999999
      888999999
    "
    map <- patchwork::wrap_plots(map_guadeloupe, map_martinique, map_guyane,
                                 map_reunion, map_mayotte, map_acores,
                                 map_madeira, map_canary, map_europe, design = design)
  }
  map <- map + patchwork::plot_annotation(title = title_t,
                                          subtitle = subtitle_t,
                                          caption = caption_t,
                                          theme = ggplot2::theme(
                                            text = ggplot2::element_text(family = "serif"),
                                            plot.title = ggplot2::element_text(hjust=0.5,face="bold"),
                                            plot.subtitle = ggplot2::element_text(hjust=0.5,face="italic")
                                          ))
  showtext::showtext_end()
  return(map)
}
