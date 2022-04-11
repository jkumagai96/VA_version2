### Summarized by subregion 

#### Density of studies 

##### Prepare data by subregion #####  
# Dissolve data by ipbes subregion
#data_subregion <- regions %>% # this dissolves the data by subregion
#  mutate(n = 1) %>% 
#  dplyr::group_by(Sub_Region) %>% 
#  dplyr::summarise(count_n = sum(n)) %>% 
#  sf::st_cast()
#st_write(data_subregion, "Data/ipbes_regions_subregions/subregion.shp", append = FALSE)
library(tidyverse)
library(sf)
data <- readxl::read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)
data_subregion <- st_read("Data/ipbes_regions_subregions/subregion.shp", quiet = T)
major_lakes <- read_sf("Data/Data_Final/Major_Lakes.shp") # dotted lines, color X
major_lakes <- st_transform(st_wrap_dateline(major_lakes), robin_crs)
grid <- st_graticule(lat = seq(-90, 90, by = 30), # the graticules 
                     lon = seq(-180, 180, by = 60)) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
  st_geometry 

# Count number of times per sub region (NAMES 1)
names1_subregion_counts <- data %>% 
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
n1_subregion_poly <- left_join(data_subregion, names1_subregion_counts, by = "Sub_Region") 

n1_subregion_poly_plot <- ggplot(n1_subregion_poly) + 
  geom_sf(data = grid, 
          colour = "gray60",
          linetype = "dashed") +
  geom_sf(aes(fill = n), colour = "gray40") +
  annotate("text", x = -18000000, y = 0, label = "0°", size = 3) +
  annotate("text", x = -18000000, y = 3200000, label = "30°N", size = 3) +
  annotate("text", x = -15500000, y = 6200000, label = "60°N", size = 3) +
  annotate("text", x = -18000000, y = -3200000, label = "30°S", size = 3) +
  annotate("text", x = -15500000, y = -6200000, label = "60°S", size = 3) +
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
  # geom_sf(data = grey_areas, fill = "grey", colour = NA) +
  geom_sf(data = major_lakes, fill = "white", colour = "gray40") +
  labs(fill = "Density of Studies") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "bottom")

n1_subregion_poly_plot

ggsave(file = "Outputs/Figures/Density_of_studies_subregion_2.pdf", plot = n1_subregion_poly_plot, width = 7, height = 5)

ggsave(file = "Outputs/Figures/Density_of_studies_subregion_big.pdf", plot = n1_subregion_poly_plot, width = 14, height = 10)

svg("Outputs/Figures/Density_of_studies_subregion.svg", width = 14, height = 10)
n1_subregion_poly_plot
dev.off()
