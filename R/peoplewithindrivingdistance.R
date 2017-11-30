

#' People Within Driving Distance
#'
#' @param locations a data frame of locations including  lon and lat columns
#' @param center a center location. Can be a character string or a two column data frame.
#' @param maxtime  a maximum driving time in minutes
#' @param key Google API key (see https://developers.google.com/maps/documentation/geocoding/get-api-key)
#'
#' @return a filtered data frame, with only rows where location is with maxtime minutes of center
#' @export
#'
#' @examples
peoplewithindrivingdistance  <- function(locations, center, maxtime, key) {


  if(class(center) == 'character') {

    centercoords  <- center

  } else {

    centercoords  <- unlist(center)

  }

  locations %>%
    nest(lat, lon, .key = "latlon") %>%
    mutate(
      vectordata  = map(latlon,     ~unlist(.x) )
      , directions  = map(vectordata, ~google_directions(.x, centercoords, key = key) )
      , drivingtime = map(directions, ~pluck(.x, "routes", "legs", 1, "duration", "value") )
    ) %>%
    unnest(drivingtime) %>%
    mutate(drivingtime = drivingtime/60) %>%
    filter(drivingtime <= maxtime)


}







