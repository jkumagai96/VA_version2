# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 21st 2021
# Create four maps with the CORPUS dataset, indicators, and variables for before 1990, 1990-1999, 2000 - 2009, 2010 to now
# Values Assessment 

##### Load Packages #####
library(tidyverse)
library(sf)
library(readxl)
library(cowplot)

##### Load Data #####
data <- read.csv("Outputs/Corpus/harmonized_data.csv")
indicators <- read.csv("Outputs/indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")
countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp") %>% 
  rename(ISO_Alpha_3 = GID_0)

##### Combine and project data #####
# add names onto indicators dataset
colnames(indicators)[2:25] <- column_names$Short_Name

df <- left_join(data, indicators, by = "ISO_Alpha_3")
df <- left_join(countries, df, by = "ISO_Alpha_3") # Three records from data removed from this process, which only 1 had data (Netherlands Antilles)

# Project data 
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
poly <- st_transform(df, robin_crs)

##### Graph all ######
grid <- st_graticule(lat = seq(-90, 90, by = 30), # the graticules 
                     lon = seq(-180, 180, by = 60)) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
  st_geometry 

plot1 <- ggplot(poly) + # Names 1 all studies 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = Names1, colour = NULL)) +
  annotate("text", x = -18000000, y = 0, label = "0°", size = 3) +
  annotate("text", x = -18000000, y = 3200000, label = "30° N", size = 3) +
  annotate("text", x = -15500000, y = 6200000, label = "60° N", size = 3) +
  annotate("text", x = -18000000, y = -3200000, label = "30° S", size = 3) +
  annotate("text", x = -15500000, y = -6200000, label = "60° S", size = 3) +
  annotate("text", x = 0, y = 9500000, label = "0°", size = 3) +
  annotate("text", x = -3000000, y = 9500000, label = "60°W", size = 3) +
  annotate("text", x = 3000000, y = 9500000, label = "60°E", size = 3) +
  annotate("text", x = -8000000, y = 9500000, label = "180°W", size = 3) +
  annotate("text", x = 8000000, y = 9500000, label = "180°E", size = 3) +
  scale_colour_manual(values = NA) +              
  guides(colour = guide_legend("No data", override.aes = list(colour = "grey", fill = "grey")))+
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(
    low = "#267311",
    high = "#ECF649",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 5, 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  labs(fill = "Density of Studies") 

# Names 1 Log
plot1_log <- ggplot(poly) + # Names 2 log all studies
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = Names1_log, colour = NULL)) +
  annotate("text", x = -18000000, y = 0, label = "0°", size = 3) +
  annotate("text", x = -18000000, y = 3200000, label = "30° N", size = 3) +
  annotate("text", x = -15500000, y = 6200000, label = "60° N", size = 3) +
  annotate("text", x = -18000000, y = -3200000, label = "30° S", size = 3) +
  annotate("text", x = -15500000, y = -6200000, label = "60° S", size = 3) +
  annotate("text", x = 0, y = 9500000, label = "0°", size = 3) +
  annotate("text", x = -3000000, y = 9500000, label = "60°W", size = 3) +
  annotate("text", x = 3000000, y = 9500000, label = "60°E", size = 3) +
  annotate("text", x = -8000000, y = 9500000, label = "180°W", size = 3) +
  annotate("text", x = 8000000, y = 9500000, label = "180°E", size = 3) +
  scale_fill_gradient(
    low = "#267311",
    high = "#ECF649",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 5, 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  labs(fill = "Density of Studies (log)") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Export 
png("Outputs/Maps/Names1_percountry.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plot1, plot1_log, labels = "auto", ncol = 1)
dev.off()

# Names 2
plot2 <- ggplot(poly) + # Names 2 all studies
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = Names2, colour = NULL)) +
  annotate("text", x = -18000000, y = 0, label = "0°", size = 3) +
  annotate("text", x = -18000000, y = 3200000, label = "30° N", size = 3) +
  annotate("text", x = -15500000, y = 6200000, label = "60° N", size = 3) +
  annotate("text", x = -18000000, y = -3200000, label = "30° S", size = 3) +
  annotate("text", x = -15500000, y = -6200000, label = "60° S", size = 3) +
  annotate("text", x = 0, y = 9500000, label = "0°", size = 3) +
  annotate("text", x = -3000000, y = 9500000, label = "60°W", size = 3) +
  annotate("text", x = 3000000, y = 9500000, label = "60°E", size = 3) +
  annotate("text", x = -8000000, y = 9500000, label = "180°W", size = 3) +
  annotate("text", x = 8000000, y = 9500000, label = "180°E", size = 3) +
  scale_fill_gradient(
    low = "#08519C",
    high = "#DEEBF7",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 5, 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  labs(fill = "Density of Institutions") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Names 2 Log
plot2_log <- ggplot(poly) + # Names 2 log all studies
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = Names2_log, colour = NULL)) +
  annotate("text", x = -18000000, y = 0, label = "0°", size = 3) +
  annotate("text", x = -18000000, y = 3200000, label = "30° N", size = 3) +
  annotate("text", x = -15500000, y = 6200000, label = "60° N", size = 3) +
  annotate("text", x = -18000000, y = -3200000, label = "30° S", size = 3) +
  annotate("text", x = -15500000, y = -6200000, label = "60° S", size = 3) +
  annotate("text", x = 0, y = 9500000, label = "0°", size = 3) +
  annotate("text", x = -3000000, y = 9500000, label = "60°W", size = 3) +
  annotate("text", x = 3000000, y = 9500000, label = "60°E", size = 3) +
  annotate("text", x = -8000000, y = 9500000, label = "180°W", size = 3) +
  annotate("text", x = 8000000, y = 9500000, label = "180°E", size = 3) +
  scale_fill_gradient(
    low = "#08519C",
    high = "#DEEBF7",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 5, 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  labs(fill = "Density of Institutions (log)") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Export 
png("Outputs/Maps/Names2_percountry.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plot2, plot2_log, labels = "auto", ncol = 1)
dev.off()

##### Graph by region #####


##### Graph by year #####

# Before 1990 

# 1990 - 1999

# 2000 - 2009

# 2010 - 2020