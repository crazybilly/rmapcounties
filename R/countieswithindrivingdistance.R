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
  , countymaps = NULL
  , fipscodes = NULL
  , output = c("all","nearbydf","mapdata","hobsonslist","map")
  , key = Sys.getenv('GOOGLE_MAPS_KEY')
){



  placeofinterest  <- centerlocation

  message(paste("\nGeocoding", centerlocation, "---------------------------\n\n"))

  placeofinterest <- googleway::google_geocode(placeofinterest, key = key) %>%
    googleway::geocode_coordinates() %>%
    dplyr::mutate(placename = placeofinterest )


  # this probably ought to be bigger than you think it should be
  #   It just keeps us from geocoding all the counties in the US, but you'd rather test too
  #   many than not enough
  generalcutoffinmiles  <- considerationradius


  drivingcutoff  <- lubridate::dminutes(drivingtime)


  # get maps ----------------------------------------------------------------

  if( is.null(countymaps)) {
    us.map  <- getcountymaps()
  } else {
    us.map  <- countymaps
  }

  # get proper fips code ----------------------------------------------------

  if( is.null(fipscodes)) {
    bannerizedfips   <- makebannerizedfips()
  } else {
    bannerizedfips  <- fipscodes
  }


  # get county centers ------------------------------------------------------


  countycenters  <- us.map@polygons %>%
    lapply( function(x){

      center  <- x@labpt
      tibble::data_frame(
        lon = center[1]
        , lat = center[2]
      )

    }) %>%
    dplyr::bind_rows()  %>%
    dplyr::bind_cols(us.map@data)


  # narrow down distances ---------------------------------------------------

  distance  <- sp::spDistsN1(
      as.matrix(countycenters[,1:2])
    , as.numeric(placeofinterest %>% dplyr::select(lng, lat))
    , longlat = T
  ) %>%
    magrittr::multiply_by(.6) # to convert km to miles

  nearbycounties  <- countycenters %>%
    magrittr::inset("distance", value = distance) %>%
    dplyr::filter(distance <= generalcutoffinmiles)



# Allow user to abort if there's a ton of nearby counties -----------------

  userabort  <- readline(paste("There are", nrow(nearbycounties), "nearby counties. What do you want to do? [A]bort or [C]ontinue? [C] ") ) %>%
      tolower()

  if(userabort == 'a') {

      stop(return(nearbycounties))

  } else {



    message(paste("\nFinding driving distance from county centers to", centerlocation, "---------------------------\n\n"))

    # it'd be nice to add some messaging here so can get a sense of where you're at
    drivingtimetodest  <- nearbycounties %>%
      dplyr::select(lat, lon) %>%
      dplyr::mutate(rowid = dplyr::row_number() ) %>%
      tidyr::nest(-rowid) %>%
      dplyr::mutate(
        geoinfo = purrr::map2(data, rowid, ~
            google_distance_loud(.x, .y, thisdest = placeofinterest$placename, thiskey = key )
        )
        , drivingtime = purrr::map(geoinfo, getdrivingtime)
      )

    nearbycounties$drivingtime  <- drivingtimetodest$drivingtime %>%
      unlist() %>%
      as_duration()

    nearbycounties %<>%
      dplyr::mutate(
          withinrange = drivingtime <= drivingcutoff
        , minutes = as.numeric(drivingtime)/60
      ) %>%
      dplyr::left_join(bannerizedfips %>% dplyr::select(GEOID, bannercode), by='GEOID')


    # output for Hobsons filter writing ---------------------------------------


    hobsonslist  <- nearbycounties %>%
      dplyr::filter(withinrange)

    hobsonslist  <- paste(hobsonslist$bannercode, collapse = "~")



    # map stuff ---------------------------------------------------------------

    countyindex  <- us.map@data$GEOID %in% (nearbycounties %>% dplyr::filter(withinrange) %>% magrittr::extract2("GEOID") )

    nearbymap  <- us.map[countyindex, ]

    nearbymap@data  <- nearbymap@data %>%
      dplyr::left_join(
        nearbycounties %>%
          dplyr::select(GEOID, minutes)
        , by = 'GEOID'
      )


    nearbyminutes  <- nearbycounties %>% dplyr::filter(withinrange) %>% magrittr::extract2("minutes")

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
