
#' People with a Given Distance
#'
#' @description Given a data frame of locations with lon and lat columns and a destination, filter the data frame to return only results that are within maxdist of the destination.
#'
#' @param locations a data frame that includes lat and lon columns
#' @param center a center location. Can be a character string or a two column data frame.
#' @param maxdist maximum distance in miles (converted to Euclidean distance by rough calculation)
#' @param key Google API key (see https://developers.google.com/maps/documentation/geocoding/get-api-key)
#'
#' @return a data frame with mapdist columns appended
#' @export
#'
peoplenearbydist  <- function(locations, center, maxdist, key) {

  maxdisteuclidean  <- maxdist/60


  if(class(center) == 'character') {

    centercoords  <- googleway::google_geocode(center, key = key) %>%
      purrr::pluck("results", "geometry", "location")

  } else {
    centercoords  <- center
  }


  locations %>%
    dplyr::mutate(mapdist = distcalc(lon = lon, lat = lat, y = centercoords) ) %>%
    dplyr::filter(mapdist <= maxdisteuclidean)

}


