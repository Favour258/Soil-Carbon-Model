# 04_model_calibration_and_sensitivity_analysis.R

# Load necessary packages
library(dplyr)
library(nloptr)

# Function for model calibration
SoilCfunction_own_scaling <- function(time, k) {
  simulation <- soil_filtered
  simulation$leaf[1] <- simulation$input_leaves_hourly[1]
  simulation$fruits[1] <- simulation$input_fruits_hourly[1]
  
  for (i in 1:time) {
    scaling <- (simulation$temp_factor_casa[i] * simulation$SWC_factor[i]) * k
    simulation$leaf[i + 1] <- simulation$leaf[i] + simulation$input_leaves_hourly[i + 1] - k1 * scaling * simulation$leaf[i]
    simulation$fruits[i + 1] <- simulation$fruits[i] + simulation$input_fruits_hourly[i + 1] - k3 * scaling * simulation$fruits[i]
    # (Continue with other calculations...)
  }
  return(simulation)
}

# Objective function to minimize RMSE
obj_crit <- function(x) {
  k_i <- x
  time_i <- (length(testrun$TIMESTAMP_END) - 1)
  mod_output <- SoilCfunction_own_scaling(time = time_i, k = k_i)
  mod_output$RESP_mol <- mod_output$RESP * 23.14814815
  rmse <- sqrt(mean((obs$Resp_obs - mod_output$RESP_mol)^2, na.rm = TRUE))
  return(rmse)
}

# Run Nelder-Mead Simplex to minimize RMSE
mod_calib <- neldermead(x0 = 2, fn = obj_crit, lower = 1, upper = 10)
print(mod_calib)

# Output: Calibrated model parameters and sensitivity analysis results
