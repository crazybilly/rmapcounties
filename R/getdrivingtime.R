#' Get Driving Time from Google Query
#'
#' @param results  the results of a `googleway::google_distance()` query
#'
#' @return a vector of driving times with class `lubridate::duration`. When the google query returns no results, driving time is returned as NA.
#' @export
#'
getdrivingtime  <- function(results) {

  r2  <- googleway::distance_elements(results) %>%
    purrr::map(2)

  hasresults  <- purrr::map_lgl(r2, ~length(.x) > 1)

  r2 %>%
    purrr::map2_chr(hasresults, ~
      ifelse(.y, .x[[1]] ,NA)
    ) %>%
    as_duration()


}
