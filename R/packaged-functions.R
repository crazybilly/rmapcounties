
# libraries ---------------------------------------------------------------

library(tidyverse)
library(muadc)
library(leaflet)

# Get County Maps ---------------------------------------------------------

#' Get County Map
#'
#' @description Create a SpatialPolygonsDataFrame of all the counties in the continental US
#'
#' @param filelocation a character string describing where to get the shape files. By default, assumes the files should be downloaed from the US Census Bureau at https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html. Local file addresses can also be used.
#' @param dest the local destination where the files should be downloaded to.
#' @param local a logical value indicating whether filelocation is a local address or not.
#'
#' @return
#' @export
getcountymaps  <- function(
    filelocation = "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_20m.zip"
  , dest = "data"
  , local = F
  ) {


  # create dir if it doesn't exist ------------------------------------------

  if( !dir.exists(dest) & !local ) {
    dir.create(dest)
  }

  # download file if we didn't feed in a local location ---------------------

  if( !local ) {

    destfile = paste0(dest,"/tiger.zip")

    download.file(filelocation, destfile = destfile )
    unzip(destfile, exdir = 'data/tiger')

    shplocation  <- sub(".zip","",destfile)

  } else {
    shplocation  <- filelocation

  }


  # read in the shape files ------------------------------------------------

  message(paste("destfile = ", destfile,"\n"))
  message(paste("shplocation = ", shplocation,"\n"))
  # Download county shape file from Tiger.
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  us.map <- rgdal::readOGR(dsn =path.expand(shplocation), layer = "cb_2015_us_county_20m", stringsAsFactors = FALSE)

  # Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
  #  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
  us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                          "64", "68", "70", "74"),]
  # Make sure other outling islands are removed.
  us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                          "95", "79"),]

}






# bannerize FIPS code -----------------------------------------------------


#' Bannerize FIPS Codes
#'
#' @description modify the default list of fips county codes to add columns for joining with Banner and Census Bureau. Uses the fips_codes data from the tigris package.
#'
#' @return a data frame
#' @export
makebannerizedfips  <- function() {

  tigris::fips_codes %>%
  mutate(
      bannercode = paste0(state,county_code)
    , GEOID = paste0(state_code, county_code)
  )
}



# map a banner georegion --------------------------------------------------


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




# counties within driving distance of a location ----------------------

#' Counties within Driving Distance
#'
#' @description generate a list of counties within a particular driving distance of a location
#'
#' @param centerlocation a character string of a location acting as the center from which you want to measure
#' @param drivingtime numeric value of time, in minutes, that you want to drive
#' @param considerationradius numeric value of distance, in miles, that you want to consider. Limits the number of geocoded counties. Recommended to be much larger than drivingtime, perhaps 50% larger, to ensure that all counties are considered.
#' @param countymaps a spatialpolygonsdataframe. If NA, data is downloaded and read from the US Census
#' @param fipscodes  a data frame of fips codes for all counties with bannerized codes added. If NA, data from the tigris package is modified to work
#' @param output a character vector describing what output you want. Available options include, "nearbydf", "mapdata", "hobsonslist" and "map"
#'
#' @return if more than one output is selected, a list containing values of each. If only one output is selected, the object itself is return, without being wrapped in a list. Outputs include:
#' \itemize{
#'  \item{"nearbydf"}{A data frame of nearby counties and their distance to centerlocation}
#'  \item{"mapdata"}{A SpaitalPolygonsDataFrame of the nearby counties appropriate for use in a leaflet map}
#'  \item{"hobsonslist"}{A character vector ready to paste into a Hobsons filter }
#'  \item{"map"}{A pre-generated leaflet map}
#'  }
#'
#' @export
#'
countieswithindrivingdistance <- function(
    centerlocation
  , drivingtime = 90
  , considerationradius = drivingtime * 1.5
  , countymaps = NA
  , fipscodes = NA
  , output = c("nearbydf","mapdata","hobsonslist","map")
  ){



  placeofinterest  <- centerlocation

  placeofinterest <- ggmap::geocode(placeofinterest) %>%
    mutate(placename = placeofinterest )


  # this probably ought to be bigger than you think it should be
  #   It just keeps us from geocoding all the counties in the US, but you'd rather test too
  #   many than not enough
  generalcutoffinmiles  <- considerationradius


  drivingcutoff  <- lubridate::dminutes(drivingtime)


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


  # get county centers ------------------------------------------------------


  countycenters  <- us.map@polygons %>%
    lapply( function(x){

      center  <- x@labpt
      data_frame(
        lon = center[1]
        , lat = center[2]
      )

    }) %>%
    bind_rows()  %>%
    bind_cols(us.map@data)


  # narrow down distances ---------------------------------------------------

  distance  <- sp::spDistsN1(
      as.matrix(countycenters[,1:2])
    , as.numeric(placeofinterest %>% select(lon, lat))
    , longlat = T
  ) %>%
    magrittr::multiply_by(.6) # to convert km to miles

  nearbycounties  <- countycenters %>%
    magrittr::inset("distance", value = distance) %>%
    filter(distance <= generalcutoffinmiles)

  revcodedcounties  <- nearbycounties %>%
    select(lon, lat) %>%
    apply(1, function(x) {
      ggmap::revgeocode(as.numeric(x))
    })

  drivingdistance  <- revcodedcounties %>%
    ggmap::mapdist( to = placeofinterest$placename, mode = 'driving' )


  nearbycounties %<>%
    bind_cols(drivingdistance) %>%
    mutate(
      drivingtime = lubridate::dminutes(minutes)
      , withinrange = drivingtime <= drivingcutoff
    ) %>%
    left_join(bannerizedfips %>% select(GEOID, bannercode), by='GEOID')



  # output for Hobsons filter writing ---------------------------------------


  hobsonslist  <- nearbycounties %>%
    filter(withinrange)

  hobsonslist  <- paste(hobsonslist$bannercode, collapse = "~")



  # map stuff ---------------------------------------------------------------

  countyindex  <- us.map@data$GEOID %in% (nearbycounties %>% filter(withinrange) %>% magrittr::extract2("GEOID") )

  nearbymap  <- us.map[countyindex, ]

  nearbymap@data  <- nearbymap@data %>%
    left_join(
      nearbycounties %>%
        select(GEOID, minutes)
      , by = 'GEOID'
    )


  pal <- colorBin("Blues"
                  , nearbycounties %>% filter(withinrange) %>% magrittr::extract2("minutes")
                  , 3, pretty = FALSE)


  countymap  <- leaflet(nearbymap) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(minutes)
      , weight = .5
      , opacity = .7
    )


# create output -----------------------------------------------------------

  alloutput  <- list(
        nearby = nearbycounties
      , mapdata = nearbymap
      , hobsonslist = hobsonslist
      , map = countymap
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


# get center of bunch of polygons -----------------------------------------

#' Get the Center of a Bunch of Polygons
#'
#' @param polygons a SpatialPolygonsDataFrame
#'
#' @return a two column data frame with the longitude and latitude of the center point of polygons
#' @export
#'
getcenterofpolygons  <- function(polygons) {

  blob  <- rgeos::gUnaryUnion(polygons)
  center  <- blob@polygons[[1]]@labpt

  data_frame(lon = center[1], lat = center[2])

}




# compare a region of counties to the drivable counties -------------------


#' Title
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
      , distancecatg = cut(minutes, 3, labels = F)
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
