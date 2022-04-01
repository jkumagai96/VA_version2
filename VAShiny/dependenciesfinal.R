

# Valuation Atlas Interactive Map

library(tidyverse)
library(cowplot)
library(sf)


data <- readxl::read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

# Declare Functions
countFunction <- function(data, x) {
  data %>%
    mutate(x = strsplit(as.character(x), ", ")) %>%
    unnest(x) %>%
    count(x, sort = TRUE) %>%
    drop_na() %>%
    mutate(n_log = log(n))
}

# Necessary datasets for the upcoming maps
harmonized_data <- read.csv("Outputs/Corpus/harmonized_data.csv")
indicators <- read.csv("Outputs/indicators_compiled.csv")
column_names <- readxl::read_excel("Data/names_of_columns.xlsx")
countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp") %>%
  rename(ISO_Alpha_3 = GID_0)

# Combine and project data
colnames(indicators)[2:25] <- column_names$Short_Name

df <- left_join(harmonized_data, indicators, by = "ISO_Alpha_3")
df <- left_join(countries, df, by = "ISO_Alpha_3")

# Project data
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
poly <- st_transform(df, robin_crs)

#make vectors 
ISO <- poly[[1]]
countrynames <- poly[[2]]

ISO[grep("Austria", countrynames)]

rm(df,data,countries)

# #cut 
# nameskeep <- "DEU|FRA|ESP|PRT|ITA|LIE"
# nameskeep <- "DEU|FRA|ITA|LIE|CHE|AUT"
# 
# # Amend dataset for testing purposes
# poly1 <- poly
# poly <- poly[grep(nameskeep, poly$ISO_Alpha_3),] # test with these (or other) countries for faster loading times
# #add prt and ita?



# Relevant columns for valuation atlas
poly <- poly[1:7]

# Change names of columns into what they actually are (may need to be updated when data is changed)
oldnames <- names(poly)
newnames <- c("ISO Alpha 3", "Country Name", "geometry", "Density of Studies", "Density of Studies (log)", "Density of Organizations", "Density of Organizations (log)")
names(poly) <- newnames

poly$`Ratio of Studies and Organizations` <- poly$`Density of Studies`/poly$`Density of Organizations`


# Create color palettes for different datasets
pal <- list()

#Colors for Density of Studies
# low = "#F7FCB9",
# high = "#006837",
# pal$DoF <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)
pal[["Density of Studies"]] <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)

#Colors for Density of Organizations
# low = "#DEEBF7",
# high = "#08519C",
# pal$DoO <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)
pal[["Density of Organizations"]] <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)

# Colors for ratio (Density of Studies/Density of Organizations)
# low = "#F7FCB9",
# high = "#FB8C00",
# pal$RSO <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)
pal[["Ratio of Studies and Organizations"]] <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)


# Load Shapefiles for updated borders-----
# grey_areas <- read_sf("Data/Data_Final/grey_areas.shp") # area grey, no outlines
# grey_areas <- st_transform(st_wrap_dateline(grey_areas), robin_crs)
# 
# solid_borders <- read_sf("Data/Data_Final/solid_borders.shp") # solid lines, color X
# solid_borders <- st_transform(st_wrap_dateline(solid_borders), robin_crs)
# 
# dashed_borders <- read_sf("Data/Data_Final/dashed_borders.shp") # dashed lines, color X
# dashed_borders <- st_transform(st_wrap_dateline(dashed_borders), robin_crs)
# 
# dotted_borders <- read_sf("Data/Data_Final/dotted_borders.shp") # dotted lines, color X
# dotted_borders <- st_transform(st_wrap_dateline(dotted_borders), robin_crs)
# 
# major_lakes <- read_sf("Data/Data_Final/Major_Lakes.shp") # dotted lines, color X
# major_lakes <- st_transform(st_wrap_dateline(major_lakes), robin_crs)



#tmap functions needed for map----

library(tmap)
library(leaflet)

#make poly nogeom
# polynogeom <- poly
# polynogeom$geometry <- NULL



# Add fill layer to nz shape
tmap_options(basemaps = 
               # "Esri.WorldGrayCanvas" # grey map
               # "Esri.WorldImagery"    # satellite images
               NULL                 # none
)
# map <- tm_shape(poly) + tm_polygons(col=names(poly)[6], palette=pal, id="Country Name", popup.vars=grep("log|geometry|Country Name",names(poly), invert=T, value=T))
# tmap_mode("view")
# map



