
#' Map a Geo Region
#'
#' @param regionfile the location of a csv with one column: bannercode
#' @param countymaps a spatialpolygonsdataframe. If NA, data is downloaded and read from the US Census
#' @param fipscodes  a data frame of fips codes for all counties with bannerized codes added. If NA, data from the tigris package is modified to work
#'
#' @return a spatialPolygonsDataFrame of the region. As a side effect, generates a leaflet map of the counties
#' @export
#'
mapregion  <- function(regionfile, countymaps = NA, fipscodes = NA) {

  filename  <- regionfile

  # 1 column data frame
  #    bannercode = all codes in region, with style IL123
  myregion <- read.tidy(filename) %>%
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
      , weight = .7
      , opacity = .7
      , popup = ~popuptext
    )


  return(us.map2)


}

