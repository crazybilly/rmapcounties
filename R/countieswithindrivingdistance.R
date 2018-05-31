#' Counties within Driving Distance
#'
#' @description generate a list of counties within a particular driving distance of a location
#'
#' @param centerlocation a character string of a location acting as the center from which you want to measure
#' @param drivingtime numeric value of time, in minutes, that you want to drive
#' @param considerationradius numeric value of distance, in miles, that you want to consider. Limits the number of geocoded counties. Recommended to be much larger than drivingtime, perhaps 50 percent larger, to ensure that all counties are considered.
#' @param countymaps a spatialpolygonsdataframe. If NA, data is downloaded and read from the US Census via getcountymaps().
#' @param fipscodes  a data frame of fips codes for all counties with bannerized codes added. If NA, data from the tigris package is modified to work
#' @param output a character vector describing what output you want. Available options include, "nearbydf", "mapdata", "hobsonslist" and "map"
#' @param key a Google Maps user API key
#'
#' @details this function is not vectorized--you cannot pass in vector of centerlocations (or more usefully, a vector of centerlocations and a second vector of driving distances). Use lapply() for this.
#'
#' @return if more than one output is selected, a list containing values of each. If only one output is selected, the object itself is returned, without being wrapped in a list. Outputs include:
#' \itemize{
#'  \item{"nearbydf"}{ a data frame of nearby counties and their distance to centerlocation}
#'  \item{"mapdata"}{ a SpaitalPolygonsDataFrame of the nearby counties appropriate for use in a leaflet map}
#'  \item{"hobsonslist"}{ a character vector ready to paste into a Hobsons filter }
#'  \item{"map"}{ a pre-generated leaflet map}
#'  }
#' @export
#'
countieswithindrivingdistance <- function(
  centerlocation
  , drivingtime = 90
  , considerationradius = drivingtime * 1.5
  , countymaps = NA
  , fipscodes = NA
  , output = c("all","nearbydf","mapdata","hobsonslist","map")
  , key = Sys.getenv('GOOGLE_MAPS_KEY')
){



  placeofinterest  <- centerlocation

  message(paste("\nGeocoding", centerlocation, "---------------------------\n\n"))

  placeofinterest <- googleway::google_geocode(placeofinterest, key = key) %>%
    googleway::geocode_coordinates() %>%
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
    , as.numeric(placeofinterest %>% select(lng, lat))
    , longlat = T
  ) %>%
    magrittr::multiply_by(.6) # to convert km to miles

  nearbycounties  <- countycenters %>%
    magrittr::inset("distance", value = distance) %>%
    filter(distance <= generalcutoffinmiles)



# Allow user to abort if there's a ton of nearby counties -----------------

  userabort  <- readline(paste("There are", nrow(nearbycounties), "nearby counties. What do you want to do? [A]bort or [C]ontinue? [C] ") ) %>%
      tolower()

  if(userabort == 'a') {

      stop(return(nearbycounties))

  } else {



    message(paste("\nFinding driving distance from county centers to", centerlocation, "---------------------------\n\n"))

    # it'd be nice to add some messaging here so can get a sense of where you're at
    drivingtimetodest  <- purrr::pmap_chr(nearbycounties, ~
        googleway::google_distance(
            origins = c(..2, ..1)
          , destinations = placeofinterest$placename
          , key = key
        ) %>%
        pluck("rows", "elements", 1, "duration", "text" )
      )  %>%
      as.duration()

    nearbycounties$drivingtime  <- drivingtimetodest

    nearbycounties %<>%
      mutate(
          withinrange = drivingtime <= drivingcutoff
        , minutes = as.numeric(drivingtime)/60
      ) %>%
      left_join(bannerizedfips %>% select(GEOID, bannercode), by='GEOID')



    # revcodedcounties  <- nearbycounties %>%
    #   left_join(bannerizedfips, by = c('STATEFP' = 'state_code', 'COUNTYFP' = 'county_code')) %>%
    #   mutate(countyname = paste(NAME, "County,", state)) %>%
    #   magrittr::extract2('countyname')
    #
    #
    # drivingdistance  <- revcodedcounties %>%
    #   ggmap::mapdist( to = placeofinterest$placename, mode = 'driving' )


    # nearbycounties %<>%
    #   bind_cols(drivingdistance) %>%
    #   mutate(
    #     drivingtime = lubridate::dminutes(minutes)
    #     , withinrange = drivingtime <= drivingcutoff
    #   ) %>%
    #   left_join(bannerizedfips %>% select(GEOID, bannercode), by='GEOID')



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


    nearbyminutes  <- nearbycounties %>% filter(withinrange) %>% magrittr::extract2("minutes")

    if(length(nearbyminutes) == 1) {
      nearbyminutes  <- c(nearbyminutes, nearbyminutes * 2)
    }

    pal <- leaflet::colorBin("Blues"
                             , nearbyminutes
                             , 3, pretty = FALSE)


    countymap  <- leaflet::leaflet(nearbymap) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(
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

}
