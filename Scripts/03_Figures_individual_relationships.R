# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 8th 2021
# Create Figures of individual relationships 
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(cowplot)
library(readxl)

#### Load Data ####
setwd("C:/Users/jkumagai/Documents/IPBES/R/VA_version2")

# Corpus 
data <- read.csv("Outputs/Corpus/harmonized_data.csv")                # full corpus
data_2010 <- read.csv("Outputs/Corpus_2010/harmonized_data.csv") %>%  # corpus from 2010
  select(ISO_Alpha_3, Names1, Names1_log, Names2, Names2_log) 

# indicators 
column_names <- read_excel("Data/names_of_columns.xlsx")
indicators <- read.csv("Outputs/Indicators_compiled.csv")

# Fix column names 
colnames(indicators) <- c("ISO_Alpha_3", column_names$Short_Name) # add names to columns 
colnames(data_2010)[2:5] <- paste(colnames(data_2010)[2:5], "> 2010")

#### Combine Data ####
all_data <- full_join(data, data_2010, by = "ISO_Alpha_3") %>% 
  select(ISO_Alpha_3:Names2_log, `Names1 > 2010`:`Names2_log > 2010`, hdi_2018:Pop_2018)

all_data <- full_join(all_data, indicators, by = "ISO_Alpha_3")

#### Set working directory ####
# Create folder to place figures 
#   dir.create("Outputs/Figures_Corpus_individual")
setwd("C:/Users/jkumagai/Documents/IPBES/R/VA_version2/Outputs/Figures_Corpus_individual")

#### Plot Function ####
Plot_function <- function(plot_data, yvar, yname) {
  plot1 <- plot_data %>% 
    ggplot(aes(x = Names1, y = yvar)) +
    geom_point() +
    labs(x = "Names 1", y = yname) +
    geom_smooth(method = "lm") +
    ggpmisc::stat_fit_glance(method = 'lm',
                             color = "red3",
                             aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                             label.x = "right", label.y = "bottom") +
    theme_bw()
  
  plot2 <- plot_data %>% 
    ggplot(aes(x = `Names1 > 2010`, y = yvar)) +
    geom_point() +
    labs(x = "Names 1 from 2010", y = yname) +
    geom_smooth(method = "lm") +
    ggpmisc::stat_fit_glance(method = 'lm',
                             color = "red3",
                             aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                             label.x = "right", label.y = "bottom") +
    theme_bw()
  
  plot3 <- plot_data %>% 
    ggplot(aes(x = Names2, y = yvar)) +
    geom_point() +
    labs(x = "Names 2", y = yname) +
    geom_smooth(method = "lm") +
    ggpmisc::stat_fit_glance(method = 'lm',
                             color = "red3",
                             aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                             label.x = "right", label.y = "bottom") +
    theme_bw()
  
  
  plot4 <- plot_data %>% 
    ggplot(aes(x = `Names2 > 2010`, y = yvar)) +
    geom_point() +
    labs(x = "Names 2 from 2010", y = yname) +
    geom_smooth(method = "lm") +
    ggpmisc::stat_fit_glance(method = 'lm',
                             color = "red3",
                             aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                             label.x = "right", label.y = "bottom") +
    theme_bw()
  
  
  
  
  plot_all <- plot_grid(plot1, plot2, plot3, plot4)
  return(plot_all)
  
}

#### Human Development Index ####
svg("HDI.svg")
Plot_function(all_data, all_data$hdi_2018, "Human Development Index (2018)")
dev.off()

#### Learning Outcomes ####
svg("LearningOutcomes.svg")
Plot_function(all_data, all_data$Learning_outcomes_2015, "Learning Oucomes (2015)")
dev.off()

#### GDP ####
svg("GDP.svg")
Plot_function(all_data, all_data$GDP_2019, "GDP (2019)")
dev.off()

#### CPI 2020 ####
svg("CPI.svg")
Plot_function(all_data, all_data$CPI_2020, "Corruption Perception Index (2020)")
dev.off()

#### Population ####
svg("POP.svg")
Plot_function(all_data, all_data$Pop_2018, "Population in Millions (2018)")
dev.off()

#### GDP per capita ####
d <- all_data %>% 
  mutate(GDP_per_capita = GDP_2019/Pop_2018) 

svg("GDP_per_capita.svg")
Plot_function(d, d$GDP_per_capita, "GDP per Capita")
dev.off()

#### Forest area under FSC certification ####
svg("FSC.svg")
Plot_function(all_data, all_data$`Forest Area under FSC certification`, "Forest area under FSC certification")
dev.off()

#### Biodiversity Habitat Index ####
svg("Biodiversity_habitat_index.svg")
Plot_function(all_data, all_data$`Biodiversity Habitat Index`, "Biodiversity Habitat Index")
dev.off()

#### Biodiversity Intactness Index ####
svg("Biodiversity_intactness_index.svg")
Plot_function(all_data, all_data$`Biodiversity Intactness Index`, "Biodiversity Intactness Index")
dev.off()

#### Biocapacity per capita ####
svg("Biocapacity_per_capita.svg")
Plot_function(all_data, all_data$`Biocapacity per capita`, "Biocapacity per Capita")
dev.off()

#### Ecological Footprint per capita ####
svg("Ecological_footprint.svg")
Plot_function(all_data, all_data$`Ecological Footprint per capita`, "Ecological Footprint per Capita")
dev.off()

#### Forest area ####
svg("Forest_area.svg")
Plot_function(all_data, all_data$`Forest area`, "Forest Area")
dev.off()

#### Water Footprint ####
svg("Water_footprint.svg")
Plot_function(all_data, all_data$`Water Footprint`, "Water Footprint")
dev.off()

#### Inland Fishery Production ####
svg("Inland_fishery_production.svg")
Plot_function(all_data, all_data$`Inland Fishery Production`, "Inland Fishery Production")
dev.off()

#### Marine Trophic Index ####
svg("Marine_tropic_index.svg")
Plot_function(all_data, all_data$`Marine Trophic Index (1950)`, "Marine Trophic Index in reference to 1950")
dev.off()

#### Nitrogen Fertilizers ####
svg("Nitrogen_fertilizers.svg")
Plot_function(all_data, all_data$`Nitrogen Fertilizers`, "Nitrogen Fertilizers")
dev.off()

#### Nitrogen Use Efficiency ####
svg("Nitrogen_use_efficiency.svg")
Plot_function(all_data, all_data$`Nitrogen Use Efficiency (%)`, "Nitrogen Use Efficiency (%)")
dev.off()

#### Percentage of area covered by protected areas ####
svg("Percentage_protected.svg")
Plot_function(all_data, all_data$`Percentage protected`, "Percentage of Area Covered by PAs")
dev.off()

#### Percentage of undernourished people ####
svg("Percentage_of_undernourished_people.svg")
Plot_function(all_data, all_data$`Percentage of undernourished people`, "Percentage of Undernourished People")
dev.off()

#### Local Breeds at risk of extinction ####
svg("Local_breeds.svg")
Plot_function(all_data, all_data$`Local Breeds at risk of extinction`, "Local Breeds At Risk of Extinction")
dev.off()

#### PA of Key Biodiversity Areas Coverage (%) ####
svg("PA_of_key_biodiversity_area_coverage.svg")
Plot_function(all_data, all_data$`PA of Key Biodiversity Areas Coverage (%)`, "Percentage of KBA covered by PAs")
dev.off()

#### Protected area management effectiveness ####
svg("PA_management_effectiveness.svg")
Plot_function(all_data, all_data$`Protected area management effectiveness`, "PA Assessed on Management Effectiveness (%)")
dev.off()

#### Protected Area Connectedness Index ####
svg("PA_connectedness.svg")
Plot_function(all_data, all_data$`Protected Area Connectedness Index`, "PA Connectedness Index")
dev.off()

#### Species Habitat Index ####
svg("Species_habitat_index.svg")
Plot_function(all_data, all_data$`Species Habitat Index`, "Species Habitat Index")
dev.off()

#### Species Protection Index ####
svg("species_protection_index.svg")
Plot_function(all_data, all_data$`Species Protection Index (%)`, "Species Protection Index (%)")
dev.off()

#### Species Status Information Index ####
svg("Species_status.svg")
Plot_function(all_data, all_data$`Species Status Information Index`, "Species Status Information Index")
dev.off()

#### Total Wood Removals (roundwood, m3) #####
svg("Total_wood_removals.svg")
Plot_function(all_data, all_data$`Total Wood Removals`, "Total wood removals (roundwood, m3")
dev.off()

#### Trends in forest extent (tree cover) ####
svg("Trends_in_forest_extent.svg")
Plot_function(all_data, all_data$`Trends in forest extent`, "Trends in forest extent")
dev.off()

#### Nitrogen Deposition Trends ####
svg("Nitrogen_deposition_trends.svg")
Plot_function(all_data, all_data$`Nitrogen Deposition Trends`, "Nitrogen Deposition Trends (kg N/ha/yr)")
dev.off()

#### Trends in Pesticides Use ####
svg("Trends_in_pesticides_use.svg")
Plot_function(all_data, all_data$`Trends in Pesticides Use`, "Trends in Pesticides Use")
dev.off()
