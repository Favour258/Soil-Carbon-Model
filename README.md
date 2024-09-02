# Soil-Carbon-Model

### Overview
This project aims to simulate soil carbon dynamics at Hainich Forest using a process-based model. The model estimates soil respiration by considering various pools of soil organic matter, including metabolic litter, structural litter, coarse woody debris (CWD), and soil organic matter (SOM). The project involves data preparation, initial model setup, a 100-year simulation, calibration, sensitivity analysis, and visualization of the model's performance against observed data.

### Repository Structure
The repository is organized into five scripts, each representing a distinct step in the modeling process:

#### 1. Data Preparation (`01_data_preparation.R`)
- **Purpose:** This script handles data loading, cleaning, and preparation. It reads in meteorological data, soil data, and carbon input data (e.g., fruit and leaves) and processes them for use in the model.
- **Outputs:** Cleaned datasets ready for input into the model.

#### 2. Initial Model Setup (`02_initial_model_setup.R`)
- **Purpose:** This script sets up the initial parameters for the model, including the k-values and a-values that govern the dynamics of different soil carbon pools. It also sets up moisture and temperature forcing factors.
- **Outputs:** A model with all initial parameters and forcing factors set up, ready for simulation.

#### 3. 100-Year Simulation (`03_100_year_simulation.R`)
- **Purpose:** This script runs a 100-year simulation to estimate the long-term behavior of soil carbon pools. It replicates the dataset for 100 years and applies the process-based model to simulate soil carbon dynamics over this period.
- **Outputs:** Results from the 100-year simulation, including estimates of carbon in different soil pools over time.

#### 4. Model Calibration and Sensitivity Analysis (`04_model_calibration_and_sensitivity_analysis.R`)
- **Purpose:** This script calibrates the model using the Nelder-Mead Simplex optimization method to minimize the root mean square error (RMSE) between observed and simulated soil respiration. It also performs a sensitivity analysis to understand how changes in model parameters affect the output.
- **Outputs:** Calibrated model parameters and sensitivity analysis results.

#### 5. Final Model and Visualization (`05_final_model_and_visualization.R`)
- **Purpose:** This script runs the final calibrated model and generates visualizations comparing the simulated soil respiration against observed data. It also provides scatter plots to visualize the correlation between observed and simulated values.
- **Outputs:** Final visualizations and comparison plots.
