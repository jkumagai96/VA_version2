# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 22nd 2021
# Create maps for density of studies / institutions 
# Values Assessment 

##### Load Packages #####
library(tidyverse)
library(sf)
library(readxl)
library(cowplot)

##### Declare Functions ######
countFunction <- function(data, x) {
  data %>% 
    mutate(x = strsplit(as.character(x), ", ")) %>% 
    unnest(x) %>% 
    count(x, sort = TRUE) %>% 
    drop_na() %>% 
    mutate(n_log = log(n))
}

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

st_drop_geometry(df) %>% select(Names1) %>% sum(na.rm = T)
st_drop_geometry(df) %>% select(Names2) %>% sum(na.rm = T)

# Project data 
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
poly <- st_transform(df, robin_crs)

##### Map full corpus ######
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
    low = "#F7FCB9",
    high = "#006837",
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
    low = "#F7FCB9",
    high = "#006837",
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
    low = "#DEEBF7",
    high = "#08519C",
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
    low = "#DEEBF7",
    high = "#08519C",
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


##### Map corpus => 2010 ######
# Load and clean data
data_2010 <- read.csv("Outputs/Corpus_2010/harmonized_data.csv")
df_2010 <- left_join(countries, data_2010, by = "ISO_Alpha_3") # Three records from data removed from this process, which only 1 had data (Netherlands Antilles)

st_drop_geometry(df_2010) %>% select(Names1) %>% sum(na.rm = T)

# Project data 
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
poly_2010 <- st_transform(df_2010, robin_crs)

# Plotting Names 1 
plotA <- ggplot(poly_2010) + # Names 1 all studies 
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
    low = "#F7FCB9",
    high = "#006837",
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

# Plotting Names 1 log 
plotA_log <- ggplot(poly_2010) + # Names 2 log all studies
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
    low = "#F7FCB9",
    high = "#006837",
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
png("Outputs/Maps/Names1_percountry_2010.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plotA, plotA_log, labels = "auto", ncol = 1)
dev.off()

# Plotting Names 2 
plotB <- ggplot(poly_2010) + # Names 2 all studies
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
    low = "#DEEBF7",
    high = "#08519C",
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

# Plotting Names 2 log
plotB_log <- ggplot(poly_2010) + # Names 2 log all studies
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
    low = "#DEEBF7",
    high = "#08519C",
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
png("Outputs/Maps/Names2_percountry_2010.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plotB, plotB_log, labels = "auto", ncol = 1)
dev.off()


###### Mapping corpus < 2010 #####

# Create dataset 
corpus_b2010 <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1) %>% 
  filter(PY < 2010)

n1 <- corpus_b2010 %>% 
  countFunction(x = corpus_b2010$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

n2 <- corpus_b2010 %>% 
  countFunction(x = corpus_b2010$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

df_b2010 <- full_join(n1, n2, by = "ISO_Alpha_3")

# Process dataset
t <- left_join(countries, df_b2010, by = "ISO_Alpha_3") # Three records from data removed from this process, which only 1 had data (Netherlands Antilles)

# Project data 
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
poly_b2010 <- st_transform(t, robin_crs)

st_drop_geometry(poly_b2010) %>% select(Names1) %>% sum(na.rm = T)

# Plotting Names 1 
plotC <- ggplot(poly_b2010) + # Names 1 all studies before 2010
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
    low = "#F7FCB9",
    high = "#006837",
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

# Plotting Names 1 log 
plotC_log <- ggplot(poly_b2010) + # Names 2 log all studies before 2010
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
    low = "#F7FCB9",
    high = "#006837",
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
png("Outputs/Maps/Names1_percountry_before2010.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plotC, plotC_log, labels = "auto", ncol = 1)
dev.off()

# Plotting Names 2 
plotD <- ggplot(poly_b2010) + # Names 2 all studies before 2010
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
    low = "#DEEBF7",
    high = "#08519C",
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

# Plotting Names 2 log
plotD_log <- ggplot(poly_b2010) + # Names 2 log all studies before 2010
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
    low = "#DEEBF7",
    high = "#08519C",
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
png("Outputs/Maps/Names2_percountry_before2010.png", width = 8, height = 8, units = "in", res = 600)
plot_grid(plotD, plotD_log, labels = "auto", ncol = 1)
dev.off()


##### Prepare data by region #####
# Download data
inborutils::download_zenodo("10.5281/zenodo.3928281", path = "Data/ipbes_regions_subregions/", quiet = FALSE)
unzip("Data/ipbes_regions_subregions/ipbes_regions_subregions_shape_1.1.zip", exdir = "Data/ipbes_regions_subregions")

# Load needed data 
regions <- st_read("Data/ipbes_regions_subregions/IPBES_regions_Subregions2.shp")
corpus <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

# Dissolve data by ipbes region
#data_region <- regions %>% # this dissolves the data by region
#  mutate(n = 1) %>% 
#  dplyr::group_by(Region) %>% 
#  dplyr::summarise(count_n = sum(n)) %>% 
#  sf::st_cast() 
#st_write(data_region, "Data/ipbes_regions_subregions/region.shp")
data_region <- st_read("Data/ipbes_regions_subregions/region.shp")

# Count number of times per region (NAMES 1)
names1_region_counts <- corpus %>% 
  mutate(x = strsplit(as.character(Region_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE) %>% 
  rename("Region" = "x")

n1_region_poly <- left_join(data_region, names1_region_counts) %>% 
  st_wrap_dateline() %>% 
  st_transform(robin_crs)

# Count number of times per region (NAMES 2)
names2_region_counts <- corpus %>%
  mutate(x = strsplit(as.character(Region_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE) %>% 
  rename("Region" = "x")

n2_region_poly <- left_join(data_region, names2_region_counts, by = "Region") %>% 
  st_wrap_dateline() %>% 
  st_transform(robin_crs)
  
##### Prepare data by subregion #####  
# Dissolve data by ipbes subregion
#data_subregion <- regions %>% # this dissolves the data by subregion
#  mutate(n = 1) %>% 
#  dplyr::group_by(Sub_Region) %>% 
#  dplyr::summarise(count_n = sum(n)) %>% 
#  sf::st_cast()
#st_write(data_subregion, "Data/ipbes_regions_subregions/subregion.shp")
data_subregion <- st_read("Data/ipbes_regions_subregions/subregion.shp")

# Count number of times per sub region (NAMES 1)
names1_subregion_counts <- corpus %>% 
  mutate(x = strsplit(as.character(Subregion_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE) %>% 
  rename("Sub_Region" = "x")

fix <- names1_subregion_counts %>% # Within the data western and central europe are separated, I added them together to match the regions within the polygon data
  filter(Sub_Region == "Western Europe" | Sub_Region == "Central Europe") 
fix <- sum(fix$n)

names1_subregion_counts <- names1_subregion_counts %>% 
  add_row(tibble_row(Sub_Region = "Central and Western Europe", n = fix))

# final join
n1_subregion_poly <- left_join(data_subregion, names1_subregion_counts, by = "Sub_Region") %>%
  st_wrap_dateline() %>% 
  st_transform(robin_crs)

# Count number of times per subregion (NAMES 2)
names2_subregion_counts <- corpus %>%
  mutate(x = strsplit(as.character(Subregion_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE) %>% 
  rename("Sub_Region" = "x")

# Within the data western and central europe are separated, I added them together to match the regions within the polygon data
fix <- names2_subregion_counts %>% 
  filter(Sub_Region == "Western Europe" | Sub_Region == "Central Europe")
fix <- sum(fix$n)

names2_subregion_counts <- names2_subregion_counts %>% 
  add_row(tibble_row(Sub_Region = "Central and Western Europe", n = fix))

# final join
n2_subregion_poly <- left_join(data_subregion, names2_subregion_counts, by = "Sub_Region") %>% 
  st_wrap_dateline() %>% 
  st_transform(robin_crs)

##### Graph data by region and sub-region ######

# Subregion names 1
plot3 <- ggplot(n1_subregion_poly) + 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = n, colour = NULL)) +
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
    low = "#F7FCB9",
    high = "#006837",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 4, 
    guide = guide_colorbar(title.position = "top",
                           title.hjust = .5,
                           barwidth = 10, 
                           barheight = 0.5
    )) +
  labs(fill = "Density of Studies") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Export 
png("Outputs/Maps/Names1_persubregion.png", width = 8, height = 5, units = "in", res = 600)
plot3
dev.off()

# Subregion names 2
plot4 <- ggplot(n2_subregion_poly) + 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = n, colour = NULL)) +
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
    low = "#DEEBF7",
    high = "#08519C",
    space = "Lab",
    na.value = "grey",
    aesthetics = "fill",
    n.breaks = 4, 
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

# Export 
png("Outputs/Maps/Names2_persubregion.png", width = 8, height = 5, units = "in", res = 600)
plot4
dev.off()

# Plot regions by names 1
n1_region_poly$n <- as.factor(n1_region_poly$n)

plot5 <- ggplot(n1_region_poly) + 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = n, colour = NULL)) +
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
  scale_fill_manual(values = c("#F7FCB9", "#ADDD8E", "#238443", "#006837", "grey")) +
  labs(fill = "Density of Studies") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Export 
png("Outputs/Maps/Names1_perregion.png", width = 8, height = 5, units = "in", res = 600)
plot5
dev.off()

# Plot regions by names 2
n2_region_poly$n <- as.factor(n2_region_poly$n)

plot6 <- ggplot(n2_region_poly) + 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = n, colour = NULL)) +
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
  scale_fill_manual(values = c("#DEEBF7", "#9ECAE1", "#4292C6", "#08519C", "grey")) +
  labs(fill = "Density of Institutions") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

# Export 
png("Outputs/Maps/Names2_perregion.png", width = 8, height = 5, units = "in", res = 600)
plot6
dev.off()
