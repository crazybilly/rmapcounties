#' Compare an Existing Region to a List of Drivable Counties
#'
#' @description
#'
#' @param regiondf an existing data frame or a file URL
#' @param drivingdistance an object of class drivingdistance or a numeric distance
#' @param countymaps a spatialpolygonsdataframe. If NA, data is downloaded and read from the US Census
#' @param fipscodes  a data frame of fips codes for all counties with bannerized codes added. If NA, data from the tigris package is modified to work
#' @param output a character vector describing what output you want. Available options include, "nearbydf", "mapdata", "outsidecountiesdf" and "map"
#'
#' @return if more than one output is selected, a list containing values of each. If only one output is selected, the object itself is return, without being wrapped in a list. Outputs include:
#' \itemize{
#'  \item{"nearbydf"}{A data frame of nearby counties and their distance to centerlocation}
#'  \item{"mapdata"}{A SpaitalPolygonsDataFrame of the nearby counties appropriate for use in a leaflet map}
#'  \item{"outsidecountiesdf"}{A data frame of counties that are within driving range but outside the regiondf}
#'  \item{"map"}{A pre-generated leaflet map}
#'  }
#' @export
#'
#' @examples
drivablecountiesvsregion  <- function(
  regiondf
  , drivingdistance
  , countymaps = NA
  , fipscodes  = NA
  , output= c('nearbydf', 'mapdata', 'outsidecountiesdf', 'map' )
) {

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



  # get region --------------------------------------------------------------

  if(grepl("data.frame", class(regiondf) ) ) {
    myregion  <- regiondf
  } else {
    myregion  <- read.tidy(regiondf)
  }

  myregion2  <- myregion %>%
    left_join(bannerizedfips, by = 'bannercode') %>%
    mutate( edgecolor = '#00FF00', inregion = T )



  # get driving distance ----------------------------------------------------

  if(grepl("drivingdistance",class(drivingdistance)) ) {
    nearbycounties  <- drivingdistance[["nearbycounties"]]
  } else {

    centerofregion <- us.map[us.map@data$GEOID %in% myregion2$GEOID] %>%
      getcenterofpolygons() %>%
      revgeocode

    nearbycounties <- countieswithindrivingdistance(
      centerlocation = centerofregion
      , drivingtime = drivingdistance
      , considerationradius = drivingtime * 2
      , countymaps = us.maps
      , fipscodes = bannerizedfips
      , output = c("nearbydf")
    )
  }




  # compare region to range -------------------------------------------------

  inrange  <- nearbycounties %>%
    filter(withinrange) %>%
    mutate(fillcolor = pal(minutes))



  mapdata3  <- us.map@data %>%
    left_join(myregion2 %>% select(GEOID, edgecolor, inregion), by = 'GEOID')  %>%
    left_join(inrange  %>% select(GEOID, minutes, withinrange,  fillcolor), by = 'GEOID')  %>%
    left_join(bannerizedfips %>% select(GEOID, bannercode), by = 'GEOID') %>%
    mutate(
      edgecolor = fillna(edgecolor, fill = '#AAAAAA')
      , popuptext = paste0(NAME,"<br>",bannercode)
      , distancecatg = cut(rank(minutes,tie.method = 'first'), 3, labels = F)
      , inregion = fillna(inregion, F)
      , withinrange = fillna(withinrange, F)
    )



  comparisonindex  <- !is.na(mapdata3$fillcolor) | mapdata3$edgecolor == "#00FF00"

  us.map3  <- us.map
  us.map3@data  <- mapdata3
  us.map3  <- us.map3[comparisonindex,]


  # map ---------------------------------------------------------------------

  comparisonmap  <- leaflet(us.map3) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~fillcolor
      , color = ~edgecolor
      , weight = 2.5
      , opacity = .7
      , popup = ~popuptext
    )

  comparisonmap


  # what counties are outside region ----------------------------------------
  outsideregion  <- mapdata3 %>%
    filter(
      !inregion
      , withinrange
      , distancecatg <= 2
    ) %>%
    arrange(-desc(NAME))

  # output ------------------------------------------------------------------


  alloutput  <- list(
    nearbydf = nearbycounties
    , mapdata = us.map3
    , outsidecountiesdf = outsideregion
    , map = comparisonmap
  )

  class(alloutput)  <- c("list","drivingdistance")


  if(grepl("all",output) ) {
    return(alloutput)
  } else if(length(output) > 1) {
    return(alloutput[output])
  } else {
    return(alloutput[[output]])
  }



}
