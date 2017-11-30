#' Get County Map
#'
#' @description Create a SpatialPolygonsDataFrame of all the counties in the continental US
#'
#' @param filelocation a character string describing where to get the shape files. By default, assumes the files should be downloaed from the US Census Bureau at https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html. Local file addresses can also be used.
#' @param dest the local destination where the files should be downloaded to.
#' @param local a logical value indicating whether filelocation is a local address or not.
#' @param continentalonly a logial value indicating whether the resulting map data should only contain the continental US or the entire US
#'
#' @return a SpatialPolygonsDataFrame with polygons for each county within the United States.
#'
#' @import curl
#' @export
getcountymaps  <- function(
  filelocation = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
  , dest = "data"
  , local = F
  , continentalonly = T
) {



  # create dir if it doesn't exist ------------------------------------------

  if( !dir.exists(dest) & !local ) {
    dir.create(dest)
  }

  # download file if we didn't feed in a local location ---------------------

  if( !local ) {

    destfile = paste0(dest,"/tiger.zip")
    exdir    = paste0(dest,"/tiger")

    curl::curl_download(filelocation, destfile = destfile )

    unzip(destfile, exdir = exdir)

    shplocation  <- sub(".zip","",destfile)

  } else {
    shplocation  <- filelocation
    destfile  <- filelocation

  }


  # read in the shape files ------------------------------------------------

  message(paste("destfile = ", destfile,"\n"))
  message(paste("shplocation = ", shplocation,"\n"))
  # Download county shape file from Tiger.
  # https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
  us.map <- rgdal::readOGR(dsn =path.expand(shplocation), layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)

  if(continentalonly) {
    # Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
    #  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
    us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                            "64", "68", "70", "74"),]
    # Make sure other outling islands are removed.
    us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                            "95", "79"),]
  }

}



