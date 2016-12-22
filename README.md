rmapcounties
===================================
map counties using leaflet
-----------------------------------

This R package provides some tools to make it easier to map US counties. Tools include:

  - `getcountymaps()` - downloads data from the US census necessary to build a leaflet map
  - `countieswithindrivingdistance()` - a function to find all counties within a particular driving distance from a location
  - `drivablecountesvsregion()` - a function to compare counties within driving distance to a preestablished region
  - `mapregion()` - a function to map an existing Banner-style georegion

Several of the functions are designed to work with Ellucian's Banner Advancement and the way Banner codes counties, namely "IL123."

These funtions are designed primarily in support of Millikin University's Alumni & Development Center and are licensed under the GPLv3 or later.
