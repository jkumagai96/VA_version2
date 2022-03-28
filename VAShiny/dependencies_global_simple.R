

# Valuation Atlas Interactive Map

library(tidyverse)
# library(cowplot)
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

# Load simplified data 
# 1 for 1000 m // 2 for 1500 m // 3 for 2000 m
countries <- read_sf("Data/cntrs_smpl/countries3.shp") %>%     # load simplified data
              rename(ISO_Alpha_3 = GID_0)

# countries <- rename(countries, ISO_Alpha_3 = GID_0)

# Combine and project data
colnames(indicators)[2:25] <- column_names$Short_Name

df <- left_join(harmonized_data, indicators, by = "ISO_Alpha_3")
df <- left_join(countries, df, by = "ISO_Alpha_3")


# Amend dataset for testing purposes
# Relevant columns for valuation atlas
df <- df[1:7]


# robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
# poly <- st_transform(df, robin_crs) # old
poly <- df

#filter empty geometries
poly <- poly %>% filter(!st_is_empty(.))

# rm(df,data,countries)



# Change names of columns into what they actually are (may need to be updated when data is changed)
# oldnames <- names(poly)
indicatornames <- c("ISO Alpha 3", "Country Name", "geometry", "Density of Studies", "Density of Studies (log)", "Density of Organizations", "Density of Organizations (log)") #, "Ratio of Studies and Organizations"
names(poly) <- indicatornames
# indicatornames <- c("ISO Alpha 3", "Country Name", "Density of Studies", "Density of Studies (log)", "Density of Organizations", "Density of Organizations (log)")

#Create Ratio
poly$`Ratio of Studies and Organizations` <- poly$`Density of Studies`/poly$`Density of Organizations`


# Replace NA with missing
# for(i in 4:length(poly)){
#   poly[[i]] <- poly[[i]] %>% as.character() %>% replace_na("Missing")
# 
# }

# poly <- as_Spatial(poly)

# Create Choice names and values
# choiceNames <- indicatornames[4:length(indicatornames)]
choiceNames <- names(poly)[4:length(names(poly))]
choiceValues <- names(poly)[4:length(names(poly))]
# names(poly) <- indicatornames # or just change them back like this?   # beware of error message





# Create color palettes for different datasets
pal <- list()

#Colors for Density of Studies
# low = "#F7FCB9",
# high = "#006837",
# pal$DoF <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)
pal[["Density of Studies"]] <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)
pal[["Density of Studies (log)"]] <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)

#Colors for Density of Organizations
# low = "#DEEBF7",
# high = "#08519C",
# pal$DoO <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)
pal[["Density of Organizations"]] <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)
pal[["Density of Organizations (log)"]] <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)

# Colors for ratio (Density of Studies/Density of Organizations)
# low = "#F7FCB9",
# high = "#FB8C00",
# pal$RSO <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)
pal[["Ratio of Studies and Organizations"]] <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)


names(pal) <- choiceValues


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
# maybe need to s

# adjust tmap options
tmap_options(basemaps = 
               "Esri.WorldGrayCanvas" # grey map
               # "Esri.WorldImagery"    # satellite images
               # NULL                 # none
               # "Stamen.Watercolor"
             ,
              check.and.fix=T)


# map <- tm_shape(poly) + tm_polygons(col=names(poly)[6], palette=pal, id="Country Name", popup.vars=grep("log|geometry|Country Name",names(poly), invert=T, value=T))
# tmap_mode("view")
# map

#speed up loading time
# https://gis.stackexchange.com/questions/62292/how-to-speed-up-the-plotting-of-polygons-in-r
