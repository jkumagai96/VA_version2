# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 22nd 2021
# Create maps for countries with different temporal sets of the corpus
# Values Assessment 

##### Load Packages #####
library(tidyverse)
library(sf)
library(readxl)
library(cowplot)

#### Declare Functions ####
countFunction <- function(data, x) {
  data %>% 
    mutate(x = strsplit(as.character(x), ", ")) %>% 
    unnest(x) %>% 
    count(x, sort = TRUE) %>% 
    drop_na() 
}

##### Load Data #####
countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp") %>% 
  rename(ISO_Alpha_3 = GID_0)

corpus <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

############################################# NAMES 1
##### Group data temporally #####
b1990 <- corpus %>% 
  filter(PY < 1990) %>% 
  countFunction(.$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)
  
b2000 <- corpus %>% 
  filter(PY >= 1990 & PY < 2000) %>% 
  countFunction(.$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

b2010 <- corpus %>% 
  filter(PY >= 2000 & PY < 2010) %>% 
  countFunction(.$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

b2020 <- corpus %>% 
  filter(PY >= 2010) %>% 
  countFunction(.$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)


##### Combine and project data #####
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

poly_1990 <- left_join(countries, b1990, by = "ISO_Alpha_3")  %>% 
  st_transform(robin_crs)

poly_2000 <- left_join(countries, b2000, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

poly_2010 <- left_join(countries, b2010, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

poly_2020 <- left_join(countries, b2020, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

##### Plot Names 1 #####
grid <- st_graticule(lat = seq(-90, 90, by = 30), # the graticules 
                     lon = seq(-180, 180, by = 60)) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
  st_geometry 

plot1 <- ggplot(poly_1990) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 5000),
    low = "#F7FCB9",
    high = "#006837",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(1000,2000,3000,4000,5000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Studies", title = "1980 - 1990") 

plot2 <- ggplot(poly_2000) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 5000),
    low = "#F7FCB9",
    high = "#006837",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(1000,2000,3000,4000,5000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Studies", title = "1990 - 2000") 

plot3 <- ggplot(poly_2010) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 5000),
    low = "#F7FCB9",
    high = "#006837",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(1000,2000,3000,4000,5000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Studies", title = "2000 - 2010") 


plot4 <- ggplot(poly_2020) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 5000),
    low = "#F7FCB9",
    high = "#006837",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(1000,2000,3000,4000,5000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Studies", title = "2010 - 2020")

png("Outputs/Maps/Names1_fourpanels.png", width = 10, height = 8, units = "in", res = 600)
plot_grid(plot1, plot2, plot3, plot4, labels = "auto", ncol = 2)
dev.off()


############################################# Timeline graph
names1_timeline <- corpus %>% 
  mutate(n = 1) %>% 
  group_by(PY) %>% 
  summarise(year_count = sum(n)) %>% 
  filter(PY != 2020)

timeline_plot <- ggplot(names1_timeline, aes(x = PY, y = year_count)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "Year", y = "Count of valuation studies")

svg("Outputs/Corpus/Timeline.svg")
timeline_plot
dev.off()


############################################# NAMES 2
##### Group data temporally #####
b1990 <- corpus %>% 
  filter(PY < 1990) %>% 
  countFunction(.$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

b2000 <- corpus %>% 
  filter(PY >= 1990 & PY < 2000) %>% 
  countFunction(.$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

b2010 <- corpus %>% 
  filter(PY >= 2000 & PY < 2010) %>% 
  countFunction(.$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

b2020 <- corpus %>% 
  filter(PY >= 2010) %>% 
  countFunction(.$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)


##### Combine and project data #####
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

poly_1990 <- left_join(countries, b1990, by = "ISO_Alpha_3")  %>% 
  st_transform(robin_crs)

poly_2000 <- left_join(countries, b2000, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

poly_2010 <- left_join(countries, b2010, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

poly_2020 <- left_join(countries, b2020, by = "ISO_Alpha_3") %>% 
  st_transform(robin_crs)

##### Plot Names 1 #####
grid <- st_graticule(lat = seq(-90, 90, by = 30), # the graticules 
                     lon = seq(-180, 180, by = 60)) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
  st_geometry 

plot1 <- ggplot(poly_1990) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 17091),
    low = "#DEEBF7",
    high = "#08519C",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(4000,8000,12000,16000),  
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Institutions", title = "1980 - 1990") 

plot2 <- ggplot(poly_2000) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 17091),
    low = "#DEEBF7",
    high = "#08519C",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(4000,8000,12000,16000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Institutions", title = "1990 - 2000") 

plot3 <- ggplot(poly_2010) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 17091),
    low = "#DEEBF7",
    high = "#08519C",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(4000,8000,12000,16000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Institutions", title = "2000 - 2010") 


plot4 <- ggplot(poly_2020) + # Names 1 all studies 
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
  scale_fill_gradient(
    limits = c(0, 17091),
    low = "#DEEBF7",
    high = "#08519C",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    breaks = c(4000,8000,12000,16000), 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(fill = "Density of Institutions", title = "2010 - 2020")

png("Outputs/Maps/Names2_fourpanels.png", width = 10, height = 8, units = "in", res = 600)
plot_grid(plot1, plot2, plot3, plot4, labels = "auto", ncol = 2)
dev.off()
