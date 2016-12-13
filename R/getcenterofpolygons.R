#' Get the Center of a Bunch of Polygons
#'
#' @param polygons a SpatialPolygonsDataFrame
#'
#' @return a two column data frame with the longitude and latitude of the center point of polygons
#' @export
#'
getcenterofpolygons  <- function(polygons) {

  blob  <- rgeos::gUnaryUnion(polygons)
  center  <- blob@polygons[[1]]@labpt

  data_frame(lon = center[1], lat = center[2])

}

