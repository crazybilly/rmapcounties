#' Safe Version of as.duration
#'
#' @description a version of lubridate::as.duration that returns NA on errors instead of throwing an error
#'
#' @export

as_duration <- purrr::possibly(lubridate::as.duration, otherwise = NA)
