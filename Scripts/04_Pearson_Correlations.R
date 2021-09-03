# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 3rd 2021
# Join together the data and create final harmonized datasets to be analyzed 
# Values Assessment 

#### Joy Kumagai
#### Date: September 3rd 2021
#### Compilation of Indicators and Subset data  (Valuation of ES/NCP)
#### Values Assessment - Atlas

#### Load Packages ####
library(tidyverse)
library(readxl)
library(corrplot)
library(Hmisc)
library(cowplot)

#### Load Data ####
setwd("~/IPBES/R/VA_version2")

corpus <- read.csv("Outputs/Corpus/harmonized_data.csv") %>% 
  select(ISO_Alpha_3, Names1, Names2)
uptakeQ2 <- read.csv("Outputs/Uptake_Q2ESvaluation/harmonized_data_Q2.csv") %>% 
  select(ISO_Alpha_3, Names1, Names2)

indicators <- read.csv("Outputs/Indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")

#### Clean Data #####
corpus_I <- left_join(x = corpus, y = indicators, by = "ISO_Alpha_3") %>% 
  select(-ISO_Alpha_3)

colnames(corpus_I) <- c("Density of Studies", 
                        "Density of Institutions", 
                        column_names$Short_Name)

#uptakeQ2
uptakeQ2_I <- left_join(x = uptakeQ2, y = indicators, by = "ISO_Alpha_3") %>% 
  select(-ISO_Alpha_3)

colnames(uptakeQ2_I) <- c("Density of Studies", 
                        "Density of Institutions", 
                        column_names$Short_Name)

####  Run Matrix of Pearson Correlations ####
res3 <- rcorr(as.matrix(corpus_I), type = "pearson") # Only linear relationships 

corrplot(res3$r, type="upper", #order="hclust", 
         p.mat = res3$P, sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

res4 <- rcorr(as.matrix(uptakeQ2_I), type = "pearson") # Only linear relationships 

corrplot(res4$r, type="upper", #order="hclust", 
         p.mat = res4$P, sig.level = 0.01, insig = "blank") # Insignificant correlation are blank

#### Export ####
png("Outputs/Corpus/pearson_filtered_plot.png", 
    units = "in", 
    width = 8, 
    height = 8, 
    pointsize=10,
    res = 600) 

corrplot(res3$r, type="upper", #order="hclust", 
         p.mat = res3$P, sig.level = 0.01, insig = "blank")

dev.off() 

png("Outputs/Uptake_Q2ESvaluation/pearson_filtered_plot.png", 
    units = "in", 
    width = 8, 
    height = 8, 
    pointsize=10,
    res = 600) 

corrplot(res4$r, type="upper", #order="hclust", 
         p.mat = res3$P, sig.level = 0.01, insig = "blank")

dev.off() 
