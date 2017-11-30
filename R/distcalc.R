

#' Calculate Euclidian Distance
#'
#' @description calculate Euclidean distance, a rough proxy for as-the-crow flies distance between vectors of latitude and longitudes and a single destination. Best used with mutate().
#'
#' @param lon a vector of longitudues
#' @param lat a vector of latitudes
#' @param y a single destinatoin
#'
#' @return a numeric vector of distances
#' @export
#'
distcalc  <- function(lon, lat, y) {
  map_dbl(seq_along(lat), ~ matrix(c(lat[.x],lon[.x], y), nrow = 2, ncol=2, byrow = T) %>% dist %>% as.numeric   )
}

