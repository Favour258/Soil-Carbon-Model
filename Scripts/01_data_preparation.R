# 01_data_preparation.R

# Load necessary packages
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(zoo)

# Read data
data_met <- read.csv("Measurements_meteo_hourly_201601_201712_gapfilled.csv")
data_soil <- read.csv("Measurements_soil_hourly_201601_201712_gapfilled.csv")
data_resp <- read.csv("Measurements_soilresp_hourly_201601_201712.csv")
data_fluxes <- read.csv("Measurements_fluxes_hourly_201601_201712_gapfilled.csv")

# Importing litter input data
input_fruit <- read_excel("Measuremets_CarbonComponents_Hainich.xlsx", sheet = "NPP - Fruit")
input_leaves <- read_excel("Measuremets_CarbonComponents_Hainich.xlsx", sheet = "NPP - Wood&Leaf")

# Clean the fruit dataset
input_fruit <- input_fruit[10:31, 1:2]
colnames(input_fruit)[colnames(input_fruit) == "Annual Fruit Production"] <- "Year"
colnames(input_fruit)[colnames(input_fruit) == "...2"] <- "NPP_F"
input_fruit$NPP_F[input_fruit$NPP_F == "nd"] <- NA
input_fruit$NPP_F <- as.numeric(input_fruit$NPP_F)
input_fruit$Year <- as.factor(input_fruit$Year)

# Clean the leaves dataset
input_leaves <- input_leaves[-1, 1:5]
colnames(input_leaves)[colnames(input_leaves) == "Total NPP"] <- "NPP"
colnames(input_leaves)[colnames(input_leaves) == "Total wood-NPP"] <- "wood_NPP"
colnames(input_leaves)[colnames(input_leaves) == "Fagus wood-NPP"] <- "fagus_wood_NPP"
colnames(input_leaves)[colnames(input_leaves) == "Leaf-NPP"] <- "leaf_NPP"
input_leaves$NPP <- as.numeric(input_leaves$NPP)
input_leaves$wood_NPP <- as.numeric(input_leaves$wood_NPP)
input_leaves$fagus_wood_NPP <- as.numeric(input_leaves$fagus_wood_NPP)
input_leaves$leaf_NPP <- as.numeric(input_leaves$leaf_NPP)
input_leaves$Year <- as.factor(input_leaves$Year)

# Output: Cleaned data ready for modeling
