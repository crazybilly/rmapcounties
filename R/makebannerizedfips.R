#' Bannerize FIPS Codes
#'
#' @description modify the default list of fips county codes to add columns for joining with Banner and Census Bureau. Uses the fips_codes data from the tigris package.
#'
#' @return a data frame of all the FIPS codes in the US.
#' @export
#'
makebannerizedfips  <- function() {

  dplyr::mutate(
    tigris::fips_codes
      , bannercode = paste0(state,county_code)
      , GEOID = paste0(state_code, county_code)
    )
}
