# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 3rd 2021
# Create Table w/ Pearson's Correlation Coefficients, significance level, and number of observations 
# for all indicators and variables 
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(corrplot)
library(Hmisc)
library(readxl)

#### Load Data ####
# Names 1 Data (Density of Studies)
Q2 <- read.csv("Outputs/Uptake_Q2ESvaluation/harmonized_data_Q2.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("ES/NCP" = Names1)

Q2_2010 <- read.csv("Outputs/Uptake_Q2ESvaluation_2010/harmonized_data_Q2.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("ES/NCP>2010" = Names1)

Q4 <- read.csv("Outputs/Uptake_Q4/harmonized_data_Q4.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("Doc_uptake" = Names1)

Q6 <- read.csv("Outputs/Uptake_Q6/harmonized_data_Q6.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("Informative" = Names1)

Q8 <- read.csv("Outputs/Uptake_Q8/harmonized_data_Q8.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("Decisive" = Names1)

Q13 <- read.csv("Outputs/Uptake_Q13/harmonized_data_13.csv") %>% 
  select(ISO_Alpha_3, Names1) %>% 
  rename("Technical" = Names1)

# Variables
area <- read.csv("Data/country_areas_UNEP_eckert.csv", sep = "\t") %>% 
  select(ISO3CD, Area_km2) %>% 
  rename("ISO_Alpha_3" = "ISO3CD")

variables <- read.csv("Outputs/variables_compiled.csv")
variables <- left_join(x = variables, y = area, by = "ISO_Alpha_3")
variables$GDP_per_cap <- variables$GDP_2019/variables$Pop_2018
variables$Pop_per_km2 <- variables$Pop_2018/variables$Area_km2
variables <- variables %>% select(-Pop_2018, -Area_km2)

# Add area of the countries and divide by population

# remove population

# Indicators
indicators <- read.csv("Outputs/Indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")
colnames(indicators) <- c("ISO_Alpha_3", column_names$Short_Name)

#### Join Data together ####
studies <- full_join(Q2, Q2_2010, by = "ISO_Alpha_3")
studies <- full_join(studies, Q4, by = "ISO_Alpha_3")
studies <- full_join(studies, Q6, by = "ISO_Alpha_3")
studies <- full_join(studies, Q8, by = "ISO_Alpha_3")
studies <- full_join(studies, Q13, by = "ISO_Alpha_3")

data <- full_join(studies, variables, by = "ISO_Alpha_3")
data <- full_join(data, indicators, by = "ISO_Alpha_3")

#### Clean Data ####
data$remove <- data %>% select(`ES/NCP`:Technical) %>% rowSums(na.rm=TRUE) # removing rows where there is no Names1 information
data <- data %>% filter(data$remove != 0) %>% select(-remove, -ISO_Alpha_3)

#### Correlation Analysis ####
# calculate correlations 
res1 <- rcorr(as.matrix(data), type = "pearson") # Only linear relationships 

# plot 
corrplot(res1$r, type="upper", #order="hclust", 
         p.mat = res1$P, sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

#### Formatting Data and Export ####

# Selecting the correlation coefficients 
data_r <- res1$r %>% 
  as.data.frame() %>% 
  select(1:6)

data_r <- data_r[-c(1:6), ]

# Selecting the significance levels 
data_p <- res1$P %>% 
  as.data.frame() %>% 
  select(1:6)

data_p <- data_p[-c(1:6), ]

# Selecting the number of observations that went into the calculation 
data_n <- res1$n %>% 
  as.data.frame() %>% 
  select(1:6)

data_n <- data_n[-c(1:6), ]

# plot rectangular correlation plot
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

write.csv(data_r, "Outputs/Pearson_correlation_table/correlation_coefficients.csv", row.names = T)
write.csv(data_p, "Outputs/Pearson_correlation_table/significance_values.csv", row.names = T)
write.csv(data_n, "Outputs/Pearson_correlation_table/number_of_observations.csv", row.names = T)

png("Outputs/Pearson_correlation_Table/correlation_figure.png", height = 8, width = 8, units = "in", res = 600)
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank
dev.off()
