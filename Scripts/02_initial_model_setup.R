# 02_initial_model_setup.R

# Load necessary packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)

# Read parameter data
par <- read_csv("parameters.csv")

# Extract k-values and a-values
k1 <- par$value[par$variable == "k1"]
k2 <- par$value[par$variable == "k2"]
# (Continue extracting other parameters as needed...)

# Set up moisture and temperature forcing factors
soil_filtered <- data_soil %>%
  mutate(
    TS_profile = (TS_2cm_degC*2/30 + TS_5cm_degC*3/30 + TS_15cm_degC*10/30 + TS_30cm_degC*15/30),
    SWC_profile = (2*SWC_8cm_.*0.08 + (SWC_8cm_. + SWC_16cm_.) * 0.16 + (SWC_16cm_. + SWC_32cm_.) * 0.3) / (2 * (0.08 + 0.16 + 0.3))
  ) %>%
  select(TIMESTAMP_END, TS_profile, SWC_profile)

# Assign time-format to first column
soil_filtered$TIMESTAMP_END <- as.POSIXct(soil_filtered$TIMESTAMP_END, format = "%Y-%m-%d %H:%M:%S")

# Output: Initial setup of the model with parameters
