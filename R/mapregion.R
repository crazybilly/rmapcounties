
#' Map a Geo Region
#'
#' @param regionfile either a data frame or the file location of a csv with a column named "bannercode" of Banner-style FIPS codes
#' @param countymaps a spatialpolygonsdataframe. If NA, data is downloaded and read from the US Census
#' @param fipscodes  a data frame of fips codes for all counties with bannerized codes added. If NA, data from the tigris package is modified to work
#'
#' @return a spatialPolygonsDataFrame of the region. As a side effect, generates a leaflet map of the counties
#' @import leaflet
#' @importFrom muadc read.tidy
#' @export
#'
mapregion  <- function(region, countymaps = NA, fipscodes = NA) {


  if(any(grepl("data.frame", class(region)) ) ) {
    myregion  <- region
  } else {
    myregion  <- read.tidy(region)
  }

  myregion %<>%
    mutate(inregion = "#00FF00")



  # get maps ----------------------------------------------------------------

  if( is.na(countymaps)) {
    us.map  <- getcountymaps()
  } else {
    us.map  <- countymaps
  }

  # get proper fips code ----------------------------------------------------

  if( is.na(fipscodes)) {
    bannerizedfips   <- makebannerizedfips()
  } else {
    bannerizedfips  <- fipscodes
  }


  mapdata  <- bannerizedfips %>%
    left_join(myregion, by='bannercode') %>%
    mutate(
        inregion  = fillna(inregion, fill = '#CCCCCC')
      , popuptext = paste(county,bannercode,sep = '<br>')
    )


  mapdata_full  <- left_join(us.map@data, mapdata, by=c("GEOID"))

  us.map2  <- us.map
  us.map2@data  <- mapdata_full



  # map the counties --------------------------------------------------------

  leaflet(us.map2) %>%
    addTiles() %>%
    addPolygons(
        fillColor = ~inregion
      , weight = .3
      , opacity = 1
      , popup = ~popuptext
    )


  # invisible(us.map2)


}

