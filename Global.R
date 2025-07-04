# #########################
# DATA MANIPULATION LIBRARIES
# #########################
library(tidyverse)
library(purrr)
library(lubridate)
library(readr)
library(data.table)

# #########################
# STATISTICAL ANALYSIS LIBRARIES
# #########################
library(survival)
library(survminer)
library(lme4)
library(lmerTest)
library(simr)
library(kSamples)
library(cramer)
library(rstan, quietly = TRUE)
library(ggsurvfit)

# #########################
# VISUALIZATION LIBRARIES
# #########################
library(ggplot2)
library(grid)
library(png)
library(gridExtra)
library(patchwork)

# #########################
# REPORTING LIBRARIES
# #########################
library(rmarkdown)
library(knitr)
library(kableExtra)

# #########################
# WEB APPLICATION LIBRARIES
# #########################
library(shiny)
library(shinyBS)
library(bslib)
library(shinyWidgets)
library(htmltools)

# #########################
# UTILITY LIBRARIES
# #########################
# library(conflicted)
library(furrr)
library(purrr)

# #########################
# STAN CONFIGURATION
# #########################
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# #########################
# PACKAGE CONFLICT RESOLUTION
# #########################
# conflicts_prefer(dplyr::filter)
# conflicts_prefer(dplyr::lag)
# conflicts_prefer(stats::lag)
# conflicts_prefer(lme4::lmer)
# conflicts_prefer(data.table::first)

# #########################
# DATA LOADING
# #########################
# Primary Dataset
valid_data <- read.csv("data/valid_data_final.csv", header = TRUE)

# Secondary Dataset
SolarData_september <- read.csv("data/SolarData_september_updated.csv", header = TRUE)

# #########################
# EXTERNAL FUNCTION SOURCES
# #########################
# Load helper and utility functions
source("report.R") # Reporting functions
source("Simulations.R") # Simulation-related functions
source("Helpers.R") # General helper functions

# #########################
# DATA VALIDATION
# #########################
# Add data integrity checks if needed
stopifnot(
  nrow(valid_data) > 0,
  nrow(SolarData_september) > 0
)

# Print basic dataset information
message("Loaded datasets:")
message("- valid_data: ", nrow(valid_data), " rows")
message("- SolarData_september: ", nrow(SolarData_september), " rows")
