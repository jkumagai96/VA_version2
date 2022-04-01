# Valuation Atlas Interactive Map

library(tidyverse)
library(sf)
library(rgeos)
# library(rmapshaper)
# https://gis.stackexchange.com/questions/236340/simplifying-and-plotting-polygons-in-leaflet-package-in-r

countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp")

lonlatpr <- "+proj=lonlat +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"


# countries <- st_simplify(st_wrap_dateline(st_transform(read_sf("Data/gadm36_levels_shp/gadm36_0.shp")), robin_crs), preserveTopology = F, dTolerance=1000) %>%     
#   rename(ISO_Alpha_3 = GID_0)
# 
# countries <- st_simplify(st_transform(read_sf("Data/gadm36_levels_shp/gadm36_0.shp"), robin_crs), preserveTopology = F, dTolerance=1000) %>%     
#   rename(ISO_Alpha_3 = GID_0)


countries <- st_transform(countries, lonlatpr)

countries1 <- st_simplify(countries, dTolerance=1000, preserveTopology = T)
# countries2 <- st_simplify(countries, dTolerance=1500, preserveTopology = T)
# countries3 <- st_simplify(countries, dTolerance=2000, preserveTopology = T) # leaves 9 geometries empty

# countries3 <- gSimplify(countries, tol=0.01, topologyPreserve = F) 

# countries3 <- ms_simplify(countries, keep=0.2)

# countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp")   
  # rename(ISO_Alpha_3 = GID_0)

#try caching it?

#write to file
setwd("Data")
if (dir.exists("cntrs_smpl")==F) {dir.create("cntrs_smpl")}
setwd("cntrs_smpl")
write_sf(countries1, "countries1.shp")
# write_sf(countries2, "countries2.shp")
# write_sf(countries3, "countries3.shp")

setwd("../../")

# wrld <- ggplot(countries, group=geometry) + geom_polygon(data=countries, fill = "white", colour = "black")
# wrld + 
#   
# plot(countries) + coord_map(xlim=c(-180,180))
# library(ggplot2)
# ggplot(countries)


# countries <- read_sf("Data/cntrs_smpl/countries.shp")
# 
# countries <- st_make_valid(countries)
# 
# 
# plot(countries)
