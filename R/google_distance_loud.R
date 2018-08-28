#' Get Driving Distance to/from a place
#'
#' @description a version of googleway::google_distance that messages where you're at. Used in countieswithindrivingdistance(), not exported.
#'
#' @param x a one row, two column data_frame with lat and lon columns in that order
#' @param y a indicator of what county you're getting distance to/from
#' @param thisdest the destination
#' @param thiskey a Google Maps API key
#'
#' @return
#'
google_distance_loud  <- function(x, y, thisdest = placeofinterest$placename, thiskey = key ) {
  message("Getting distance to county ", y)

  googleway::google_distance(
      origins = x
    ,  destinations = thisdest
    , key = thiskey
 )
}
