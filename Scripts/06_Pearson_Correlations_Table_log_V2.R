# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 7th 2021
# Create Table w/ Pearson's Correlation Coefficients, significance level, and number of 
#   observations for just corpus and indicators
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(corrplot)
library(Hmisc)
library(readxl)

#### Load Data ####
corpus <- read.csv("Outputs/Corpus/harmonized_data.csv") %>% 
  mutate(GDP_per_capita = GDP_2019/Pop_2018) %>% 
  select(-Names1, -Names2)

corpus_2010 <- read.csv("Outputs/Corpus_2010/harmonized_data.csv") %>% 
  select(ISO_Alpha_3, Names1_log, Names2_log)

indicators <- read.csv("Outputs/Indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")

#### Clean and Join Data together ####
colnames(indicators) <- c("ISO_Alpha_3", column_names$Short_Name)
colnames(corpus_2010)[2:3] <- paste(colnames(corpus)[2:3], "> 2010")

data <- full_join(corpus_2010, corpus, by = "ISO_Alpha_3")
data <- full_join(data, indicators, by = "ISO_Alpha_3")

data$remove <- data %>% select(`Names1_log > 2010`:Names2_log) %>% rowSums(na.rm=TRUE) # removing rows where there is no Names1 information
data <- data %>% 
  filter(data$remove != 0) %>% 
  select(-remove, -ISO_Alpha_3) %>% 
  select(Names1_log, `Names1_log > 2010`, Names2_log, `Names2_log > 2010`, hdi_2018:`Trends in Pesticides Use`) %>% 
  rename("Log Density of Studies" = "Names1_log",
         "Log Density of Studies post 2010" = "Names1_log > 2010",
         "Log Density of Institutions" = "Names2_log",
         "Log Density of Institutions post 2010" = "Names2_log > 2010")

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
  select(1:4)

data_r <- data_r[-c(1:4), ]

# Selecting the significance levels 
data_p <- res1$P %>% 
  as.data.frame() %>% 
  select(1:4)

data_p <- data_p[-c(1:4), ]

# Selecting the number of observations that went into the calculation 
data_n <- res1$n %>% 
  as.data.frame() %>% 
  select(1:4)

data_n <- data_n[-c(1:4), ]

# plot rectangular correlation plot
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank


# Export 
write.csv(data_r, "Outputs/Pearson_correlation_table/correlation_coefficients_log.csv", row.names = T)
write.csv(data_p, "Outputs/Pearson_correlation_table/significance_values_log.csv", row.names = T)
write.csv(data_n, "Outputs/Pearson_correlation_table/number_of_observations_log.csv", row.names = T)

png("Outputs/Pearson_correlation_Table/correlation_figure_log.png", height = 8, width = 8, units = "in", res = 600)
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank
dev.off()

# Only significant relationships
data_r <- data_r[-c(6,7,8,10,15,17,18,21,24), ]
data_p <- data_p[-c(6,7,8,10,15,17,18,21,24), ]
data_n <- data_n[-c(6,7,8,10,15,17,18,21,24), ]


corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

png("Outputs/Pearson_correlation_Table/correlation_figure_log_only_sig.png", height = 8, width = 8, units = "in", res = 600)
corrplot(as.matrix(data_r), is.corr = FALSE, cl.ratio = .6, tl.col="black",
         p.mat = as.matrix(data_p), sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

dev.off()
