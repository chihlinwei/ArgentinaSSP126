#' Global distribution of large seamounts
#'
#' Global distribution of large seamounts from Kim and Wessel (2011)
#'
#' @details
#' This dataset has compiled 24643  separate large seamounts in the world ocean.
#' @docType data
#' @keywords datasets
#' @format A SpatialPointsDataFrame
#' @source \url{https://doi.org/10.1111/j.1365-246X.2011.05076.x}
#' @references Kim, S.-S., Wessel, P., 2011. New global seamount census from altimetry-derived gravity data.
#' Geophysical Journal International 186, 615–631.
#' @name seamount
#' @examples
#' # VME-associated seamounts
#' # Seamount submmits between 200 and 2500 m water depth
#' library(plotKML)
#' seamount@data$Summit <- seamount@data$Height+seamount@data$Depth
#' plotKML(subset(seamount, Summit>=-2500 & Summit<=-200)[, "Summit"],
#'         folder.name="seamount", colour_scale = jet.colors(100), raster_name="seamount.png")
NULL
