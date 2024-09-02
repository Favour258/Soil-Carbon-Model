# 03_100_year_simulation.R

# Load necessary packages
library(dplyr)

# Set initial pools
soil_filtered[1, "meta"] <- 1200
soil_filtered[1, "struc"] <- 400
soil_filtered[1, "fastSOM"] <- 800
soil_filtered[1, "slowSOM"] <- 6000
soil_filtered[1, "CWD"] <- 2400

# Replicate dataset for 100-year simulation
soil_200 <- soil_filtered[rep(seq_len(nrow(soil_filtered)), 50), ]
soil_200$length <- seq_len(nrow(soil_200))

# Function for 100-year loop simulation
SoilCfunction_own_scaling <- function(time) {
  simulation <- soil_200
  simulation$leaf[1] <- simulation$input_leaves_hourly[1]
  simulation$fruits[1] <- simulation$input_fruits_hourly[1]
  
  for (i in 1:time) {
    scaling <- simulation$temp_factor_casa[i] * simulation$SWC_factor[i]
    simulation$leaf[i + 1] <- simulation$leaf[i] + simulation$input_leaves_hourly[i + 1] - k1 * scaling * simulation$leaf[i]
    simulation$fruits[i + 1] <- simulation$fruits[i] + simulation$input_fruits_hourly[i + 1] - k3 * scaling * simulation$fruits[i]
    # (Continue with other calculations...)
  }
  return(simulation)
}

# Output: Results of 100-year simulation
