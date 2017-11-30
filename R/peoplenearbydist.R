
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

    centercoords  <- google_geocode(center, key = key) %>%
      pluck("results", "geometry", "location")

  } else {
    centercoords  <- center
  }


  locations %>%
    mutate(mapdist = distcalc(lon = lon, lat = lat, y = centercoords) ) %>%
    filter(mapdist <= maxdisteuclidean)

}


