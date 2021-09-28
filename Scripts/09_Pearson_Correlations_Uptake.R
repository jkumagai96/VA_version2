# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 27th 2021
# Pearson Correlations for Uptake Dataset
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(corrplot)
library(Hmisc)
library(readxl)

#### Declare Functions ####
countFunction <- function(data, x) {
  data %>% 
    mutate(x = strsplit(as.character(x), ", ")) %>% 
    unnest(x) %>% 
    count(x, sort = TRUE) %>% 
    drop_na()
}

#### Load Data ####
uptake <- read_excel("Data/Uptake_results_sample_characteristics.xlsx")
uptake_2010 <- uptake %>%  # left with 1,722 records
  filter(PY >= 2010)

variables <- read.csv("Outputs/variables_compiled.csv")
indicators <- read.csv("Outputs/Indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")
colnames(indicators) <- c("ISO_Alpha_3", column_names$Short_Name)

#### Count Countries ####
names1 <- uptake %>% 
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n)

names1_2010 <- uptake_2010 %>% 
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1_2010" = n)

doc_uptake <- uptake %>%  
  filter(Q4uptake == "Documented uptake") %>%  # left with 169 records
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Uptake" = n)

informative <- uptake %>% 
  filter(Q6informative == "YES") %>% # left with 614 records 
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Informative" = n)

decisive <- uptake %>% 
  filter(Q8decisive == "YES") %>% # left with 495 records 
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Decisive" = n)

technical <- uptake %>% # left with 87 records
  filter(Q13technical == "YES") %>% 
  countFunction(x = .$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Technical" = n)
  
#### Join to Indicators #####
data <- full_join(names1, names1_2010, by = "ISO_Alpha_3")
data <- full_join(data, doc_uptake, by = "ISO_Alpha_3")
data <- full_join(data, informative, by = "ISO_Alpha_3")
data <- full_join(data, decisive, by = "ISO_Alpha_3")
data <- full_join(data, technical, by = "ISO_Alpha_3")
data <- full_join(data, variables, by = "ISO_Alpha_3")
data <- full_join(data, indicators, by = "ISO_Alpha_3")

data$remove <- data %>% select(Names1:Technical) %>% rowSums(na.rm=TRUE) # removing rows where there is no Names1 information

data <- data %>% 
  filter(data$remove != 0) %>% 
  select(-remove, -ISO_Alpha_3) %>% 
  rename("Density of ES/NCP study location" = "Names1",
         "Density of ES/NCP study location post 2010" = "Names1_2010",
         "Documented Uptake" = "Uptake",
         "Human Development Index" = "hdi_2018",
         "Learning Outcomes" = "Learning_outcomes_2015",
         "Gross Domestic Product" = "GDP_2019", 
         "Corruption Perception Index" = "CPI_2020",
         "Population" = "Pop_2018",
         "Forest Area Under FSC Cetrification" = "Forest Area under FSC certification",
         "Biocapacity Per Capita" = "Biocapacity per capita",
         "Forest Area" = "Forest area",
         "Marine Trophic Index" = "Marine Trophic Index (1950)",
         "Percent Nitrogen Use Efficiency" = "Nitrogen Use Efficiency (%)",
         "Percentage Protected" = "Percentage protected",
         "Percentage of Undernourished People" = "Percentage of undernourished people",
         "Local Breeds at Risk of Extinction" = "Local Breeds at risk of extinction",
         "Percentage of Key Biodiversity Areas Protected" = "PA of Key Biodiversity Areas Coverage (%)",
         "Protected Area Management Effectiveness" = "Protected area management effectiveness",
         "Species Protection Index" = "Species Protection Index (%)",
         "Trends in Forest Extent" = "Trends in forest extent")

#### Pearson Correlation #### 
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
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black", tl.srt = 45, 
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

# Only significant relationships
data_r <- data_r[-c(2,6,7,9,14,16:21,23,24,25,27,28), ]
data_p <- data_p[-c(2,6,7,9,14,16:21,23,24,25,27,28), ]
data_n <- data_n[-c(2,6,7,9,14,16:21,23,24,25,27,28), ]

corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black", tl.srt = 45,
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

png("Outputs/Pearson_correlation_Table/correlation_figure_uptake_only_sig.png", height = 8, width = 10, units = "in", res = 600)
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black", tl.srt = 45,
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

dev.off()


