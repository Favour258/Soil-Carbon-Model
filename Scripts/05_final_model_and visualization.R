# 05_final_model_and_visualization.R

# Load necessary packages
library(ggplot2)

# Final model run with calibrated parameters
k <- mod_calib$par
time <- (length(testrun$TIMESTAMP_END) - 1)
mod_output <- SoilCfunction_own_scaling(time, k)
mod_output$RESP_mol <- mod_output$RESP * 23.14814815
mod_output$Resp_obs <- obs$Resp_obs

# Plotting results
ggplot(data = mod_output, aes(x = TIMESTAMP_END)) +
  geom_line(aes(y = RESP_mol, color = "simulation new"), size = 0.7) +
  geom_line(aes(y = Resp_obs, color = "observations"), size = 0.7) +
  geom_line(aes(y = RESP_mol_old, color = "simulation old"), size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab('time') + ylab('Soil respiration (µmol CO2 / m2 /s)') +
  scale_color_manual(name = "", values = c("darkblue", "darkred", "darkgrey")) +
  labs(title = "Observed respiration at Hainich Forest vs simulated",
       subtitle = paste("Settings: own scaling factor multiplied by", round(k, 2), ", RMSE: ", round(mod_calib$value, 2))) +
  theme_light()

# Scatter plot of observations vs. simulated values
ggplot(data = mod_output) +
  geom_point(aes(x = RESP_mol, y = Resp_obs), size = 1, color = 'grey30', alpha = .5) +
  geom_smooth(aes(x = RESP_mol, y = Resp_obs), method = "lm", fullrange = TRUE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_abline(slope = 1, linetype = "dashed", color = "black") +
  xlab('simulated variable (µmol CO2 / m2 /s)') + ylab('observed variable (µmol CO2 / m2 /s)') +
  theme_light() + theme(aspect.ratio = 1, legend.position = "none")

# Output: Final visualizations comparing observed and simulated values
