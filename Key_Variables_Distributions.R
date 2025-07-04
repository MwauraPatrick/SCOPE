# # Load the external function
# source("Key_functions.R")
# source("Data_Manipulation.R")
#
# Core tidyverse and data wrangling
library(tidyverse) # Includes dplyr, ggplot2, etc.
library(lubridate)
library(data.table)
library(hms)

# Statistical modelling and inference
library(mgcv) # Generalized Additive Models
library(MASS) # For glm.nb, etc.
library(car) # Companion to Applied Regression
library(lmtest) # e.g. Breusch-Pagan test
library(pwr) # Power analysis
library(powerSurvEpi) # Power analysis for survival

# Mixed models
library(lme4)
library(lmerTest)
library(pbkrtest) # Kenward-Roger approximation

# Survival analysis
library(survival)
library(survminer)
library(nph) # Non-proportional hazards

# GOF and distribution tests
library(goftest)
library(DescTools)
library(kSamples)
library(cramer)
library(twosamples)

# Tables and reporting
library(knitr)
library(kableExtra)
library(pander)
library(xtable)

# Visualization
library(RColorBrewer)
library(plotly)
library(ggimage)
library(grid)
library(htmlwidgets)

# Epidemiology tools
library(epitools)

# Optional base graphics (not usually needed explicitly)
require(graphics)


#
#
# valid_data <- read.csv("data/valid_data.csv", header = T)
# SolarData_september <- read.csv("data/SolarData_september_updated.csv")

#
# ###############################################################################
# # T1_baseline <- rbeta(n_total, shape1 = 0.926113, shape2 = 2.714559)
#
#
#
#
# #####################################################################
# # Ta_avg  <- runif(n_total, min = 20.1, max = 29.85)
#
#
# ###################################################################################
# ########################### Average Irradiation ###################################
# ###################################################################################
#
# # Linear model (LM)
# lm_model <- lm(I_avg ~ solar_time + solar_zenith_deg, data = valid_data)
#
# # Additive GAM (separate smooths)
# gam_additive <- gam(I_avg ~ s(solar_time) + s(solar_zenith_deg),
#                     data = valid_data, method = "REML")
#
# # Tensor product smooth
# gam_tensor <- gam(I_avg ~ te(solar_time, solar_zenith_deg),
#                   data = valid_data, method = "REML")
#
# # Interaction using 'ti' components (main + interaction)
# gam_ti <- gam(I_avg ~ ti(solar_time) + ti(solar_zenith_deg) +
#                 ti(solar_time, solar_zenith_deg),
#               data = valid_data, method = "REML")
#
# # Full interaction controlling for smoothing by zenith angle
# gam_by <- gam(I_avg ~ s(solar_time, by = solar_zenith_deg),
#               data = valid_data, method = "REML")
#
# # List of models
# model_list <- list(
#   lm = lm_model,
#   additive = gam_additive,
#   tensor = gam_tensor,
#   ti_interaction = gam_ti,
#   by_interaction = gam_by
# )
#
# # Initialize comparison table
# model_comparison <- data.frame(
#   Model = character(),
#   AIC = numeric(),
#   R2_adj = numeric(),
#   GCV = numeric(),
#   Deviance_Explained = numeric(),
#   Sigma = numeric(),
#   stringsAsFactors = FALSE
# )
#
# # Populate model comparison table
# for (name in names(model_list)) {
#   mod <- model_list[[name]]
#
#   if ("gam" %in% class(mod)) {
#     mod_summary <- summary(mod)
#     R2 <- mod_summary$r.sq
#     dev_exp <- mod_summary$dev.expl * 100
#     sigma_hat <- sqrt(mod$gcv.ubre)
#     model_comparison <- rbind(model_comparison, data.frame(
#       Model = name,
#       AIC = AIC(mod),
#       R2_adj = R2,
#       GCV = mod$gcv.ubre,
#       Deviance_Explained = dev_exp,
#       Sigma = sigma_hat
#     ))
#   } else { # Linear model
#     R2 <- summary(mod)$adj.r.squared
#     sigma_hat <- sigma(mod)
#     model_comparison <- rbind(model_comparison, data.frame(
#       Model = name,
#       AIC = AIC(mod),
#       R2_adj = R2,
#       GCV = NA,
#       Deviance_Explained = NA,
#       Sigma = sigma_hat
#     ))
#   }
# }
#
# # Sort models by AIC (ascending)
# (model_comparison <- model_comparison[order(model_comparison$AIC), ])
#
#
# # Extract and save the best model
# best_model_name <- model_comparison$Model[1]
# best_model <- model_list[[best_model_name]]
#
# # Extract estimated residual standard deviation (sigma)
# if ("gam" %in% class(best_model)) {
#   sigma_hat_best <- sqrt(best_model$gcv.ubre)
# } else {
#   sigma_hat_best <- sigma(best_model)
# }
#
# # Bundle model and sigma in a list
# gam_params <- list(
#   model = best_model,
#   sigma = sigma_hat_best
# )
#
# # Save to file
# saveRDS(gam_params, "data/gam_tensor_params.rds")
#
# cat("Best model saved:", best_model_name, "\n")
#
# gam_model <- gam_params$model
#
#
# #Simulation Function
# simulate_I_avg <- function(new_data, gam_model, gam_params, seed = 123, add_noise = TRUE) {
#   set.seed(seed)
#   mu <- predict(gam_model, newdata = new_data)
#   if (add_noise) {
#     sigma <- gam_params$sigma
#     return(rnorm(n = length(mu), mean = mu, sd = sigma))
#   } else {
#     return(mu)
#   }
# }
#
# # Simulate New Data Using Original Covariates
# new_data <- valid_data[, c("solar_time", "solar_zenith_deg", "I_avg")]  # use original covariates
#
# # Simulate I_avg
# (simulated_I_avg <- simulate_I_avg(new_data, gam_model, gam_params, seed = 123, add_noise = TRUE))
#
#
#
# #Goodness-of-Fit Tests
# (ks_result <- ks.test(valid_data$I_avg, simulated_I_avg))
# (cramer_result <- cramer.test(valid_data$I_avg, simulated_I_avg))
# # (ad_result <- ad.test(valid_data$I_avg, simulated_I_avg))
#
# #Error Metrics
# (rmse <- sqrt(mean((valid_data$I_avg - simulated_I_avg)^2, na.rm = TRUE)))
# (mae  <- mean(abs(valid_data$I_avg - simulated_I_avg), na.rm = TRUE))
#
# # Save ECDF and Histogram side-by-side
# png("ecdf_hist_comparison.png", width = 1200, height = 600)
#
# # Set plotting area to 1 row, 2 columns
# par(mfrow = c(1, 2))
#
# # ECDF Plot
# plot(ecdf(valid_data$I_avg), col = "blue", main = "Original vs Simulated",
#      xlab = "I_avg", ylab = "Empirical CDF")
# lines(ecdf(simulated_I_avg), col = "green")
# legend("bottomright", legend = c("Original", "Simulated"), col = c("blue", "green"), lty = 1)
#
# # Histogram Comparison
# hist(valid_data$I_avg, breaks = 30, col = rgb(0, 0, 1, 0.4),
#      main = "Original vs Simulated I_avg",
#      xlab = "I_avg", freq = FALSE)
# hist(simulated_I_avg, breaks = 30, col = rgb(0, 1, 0, 0.4),
#      add = TRUE, freq = FALSE)
# legend("topright", legend = c("Original", "Simulated"),
#        fill = c(rgb(0, 0, 1, 0.4), rgb(0, 1, 0, 0.4)))
#
# dev.off()
#
# ###################################################### For simulation
# #range(valid_data$solar_time)
# # 10.20769 13.98780
# #range(valid_data$solar_zenith_deg)
# #27.57080 61.04959
# #cor(valid_data$solar_time,valid_data$solar_zenith_deg)
# #0.7039366
# # Generate solar geometry data sequence
# generate_and_simulate_solar_data <- function(start_time = "09:00:00",
#                                              end_time = "14:00:00",
#                                              interval_minutes = 10,
#                                              TestDate = as.Date("2022-07-23"),
#                                              NB = 51.301,       # Latitude
#                                              WL = 5.3,# Longitude
#                                              seed=123,
#                                              L_st = 15,
#                                              tz = "Europe/Brussels",
#                                              total_n = NULL,
#                                             add_noise = TRUE) {
#   # Save current RNG state
#   old_seed <- .Random.seed
#
#   if (!is.null(seed)) {
#     set.seed(seed)
#   } else {
#     set.seed(as.numeric(Sys.time()))
#   }
#
#   on.exit({
#     if (!is.null(old_seed)) .Random.seed <<- old_seed
#   }, add = TRUE)
#
#   # --- Step 1: Time sequence ---
#   t_start <- as.POSIXct(paste(TestDate, start_time), tz = tz)
#   t_end   <- as.POSIXct(paste(TestDate, end_time), tz = tz)
#   time_seq <- seq(from = t_start, to = t_end, by = paste(interval_minutes, "mins"))
#
#   if (!is.null(total_n)) {
#     suppressWarnings({
#       time_seq <- rep(time_seq, length.out = total_n)
#     })
#   }
#
#   # --- Step 2: Solar time + geometry ---
#   data <- data.frame(
#     H1 = time_seq,
#     H2 = time_seq,
#     TestDate = TestDate
#   )
#
#   data <- compute_solar_time(data, TestDate = TestDate, WL = WL, L_st = L_st)
#   data <- compute_solar_geometry(data, WL = WL, NB = NB, L_st = L_st)
#
#   # --- Step 3: Prepare data for GAM prediction ---
#   prediction_data <- data %>%
#     transmute(
#       solar_time = round(solar_time, 2),
#       solar_zenith_deg = round(solar_zenith_deg, 2)
#     )
#
#   # --- Step 4: Load GAM model and predict ---
#   gam_params <- readRDS("data/gam_tensor_params.rds")
#   gam_model <- gam_params$model
#   sigma <- gam_params$sigma
#   mu <- predict(gam_model, newdata = prediction_data)
#
#   # --- Step 5: Simulate I_avg ---
#   I_avg <- if (add_noise) {
#     rnorm(n = length(mu), mean = mu, sd = sigma)
#   } else {
#     mu
#   }
#
#   # --- Step 6: Final output ---
#   output <- data %>%
#     transmute(
#       clock_time = format(H1, "%H:%M:%S"),
#       decimal_hour = hour(H1) + minute(H1)/60 + second(H1)/3600,
#       solar_time = round(solar_time, 2),
#       solar_zenith_deg = round(solar_zenith_deg, 2),
#       solar_azimuth_deg = round(solar_azimuth_deg, 2),
#       I_avg = round(I_avg, 2)
#     )
#
#   return(output)
# }
#
# # Generate solar + irradiance data with 50 time points
# (simulated_data <- generate_and_simulate_solar_data(total_n = 20))
# head(simulated_data)
#
# # (solar_data <- generate_solar_data_seq(total_n= nrow(valid_data)))
# # (simulated_I_avg <- simulate_I_avg(total_n= nrow(valid_data)))
# #(solar_data <- generate_solar_data_seq(total_n  = 20))
# #head(valid_data$solar_zenith_deg,20)
# #(simulated_I <- simulate_I_avg(total_n = 20))
# #head(valid_data$I_avg, 20)
# # Generate solar + irradiance data with 50 time points
# simulated_data <- generate_and_simulate_solar_data(total_n = 50)
# head(simulated_data)
#
# # Sort both original and simulated data by solar time
# new_data_sorted<-new_data[order(new_data$solar_time), ]
# simulated_data_sorted <- simulated_data[order(simulated_data$solar_time), ]
#
# # ---- Plot 1: Irradiance Comparison ----
# plot(new_data_sorted$solar_time, new_data_sorted$I_avg,
#      type = "l", col = "blue", lwd = 2,
#      xlab = "Solar Time", ylab = "Irradiance (I_avg)",
#      main = "Irradiance: Original vs Simulated")
#
# lines(simulated_data_sorted$solar_time, simulated_data_sorted$I_avg,
#       col = "red", lwd = 2, lty = 2)
#
# legend("topleft", legend = c("Original", "Simulated"),
#        col = c("blue", "red"), lty = c(1, 2), lwd = 2)
#
# # ---- Plot 2: Solar Zenith Comparison ----
# plot(new_data_sorted$solar_time, new_data_sorted$solar_zenith_deg,
#      type = "l", col = "green", lwd = 2,
#      xlab = "Solar Time", ylab = "Solar Zenith Angle (degrees)",
#      main = "Solar Zenith Angle: Original vs Simulated")
#
# lines(simulated_data_sorted$solar_time, simulated_data_sorted$solar_zenith_deg,
#       col = "red", lwd = 2, lty = 2)
#
# legend("topleft", legend = c("Original", "Simulated"),
#        col = c("green", "red"), lty = c(1, 2), lwd = 2)
#
# # Save plots to a PNG file
# png("irradiance_vs_solar_zenith.png", width = 1000, height = 500)
#
# # Arrange two plots side by side
# par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))  # Optional: adjust margins
#
# # ---- Plot 1: Irradiance Comparison ----
# plot(new_data_sorted$solar_time, new_data_sorted$I_avg,
#      type = "l", col = "blue", lwd = 2,
#      xlab = "Solar Time", ylab = "Irradiance (I_avg)",
#      main = "Irradiance: Original vs Simulated")
#
# lines(simulated_data_sorted$solar_time, simulated_data_sorted$I_avg,
#       col = "red", lwd = 2, lty = 2)
#
# legend("topleft", legend = c("Original", "Simulated"),
#        col = c("blue", "red"), lty = c(1, 2), lwd = 2)
#
# # ---- Plot 2: Solar Zenith Comparison ----
# plot(new_data_sorted$solar_time, new_data_sorted$solar_zenith_deg,
#      type = "l", col = "green", lwd = 2,
#      xlab = "Solar Time", ylab = "Solar Zenith Angle (degrees)",
#      main = "Solar Zenith Angle: Original vs Simulated")
#
# lines(simulated_data_sorted$solar_time, simulated_data_sorted$solar_zenith_deg,
#       col = "red", lwd = 2, lty = 2)
#
# legend("topleft", legend = c("Original", "Simulated"),
#        col = c("green", "red"), lty = c(1, 2), lwd = 2)
#
# # Close the PNG device
# dev.off()
#
#
# ##########################################################################################
# ############################## Wind Distribution #######################################
# ##########################################################################################
# # # Prepare data
# wind <- valid_data$Wind
# wind <- wind[!is.na(wind)]
#
# # Estimate empirical density
# dens <- density(wind, from = 0, to = max(wind), n = length(wind))
# v_vals <- dens$x
# log_p_hat <- log(dens$y)
#
# # # Fit EPD of order 5 with scaled x-values
# n_order <- 5
# v_scaled <- scale(v_vals)
# X <- sapply(1:n_order, function(i) v_scaled^i)
# fit <- lm(log_p_hat ~ X)
# a_hat <- coef(fit)[-1]  # exclude intercept (absorbed into log C)
#
# # # Create EPD functions using original v values
# # # Define unnormalized EPD function
# unnormalized_epd <- function(v) {
#   v_std <- (v - attr(v_scaled, "scaled:center")) / attr(v_scaled, "scaled:scale")
#   poly_val <- sapply(v_std, function(x) sum(a_hat * x^(1:n_order)))
#   exp(poly_val)
# }
#
# # # Compute normalizing constant C
# upper_bound <- max(wind) * 2
# C_inv <- integrate(unnormalized_epd, lower = 0, upper = upper_bound, subdivisions = 1000)$value
# C_hat <- 1 / C_inv
#
# # Normalized EPD density function
# epd_pdf <- function(v) {
#   v_std <- (v - attr(v_scaled, "scaled:center")) / attr(v_scaled, "scaled:scale")
#   poly_val <- sapply(v_std, function(x) sum(a_hat * x^(1:n_order)))
#   C_hat * exp(poly_val)
# }
# #
# # # Rejection sampling from EPD
# simulate_epd <- function(n) {
#   samples <- numeric(n)
#   max_pdf <- 3.468017#max(epd_pdf(v_vals))
#   count <- 0
#   while (count < n) {
#     u <- runif(1, 0, 4.8)#upper_bound)
#     w <- runif(1)
#     if (w < epd_pdf(u) / max_pdf) {
#       count <- count + 1
#       samples[count] <- u
#     }
#   }
#   samples
# }
#
# # # Simulate data
# simulated_wind <- simulate_epd(length(wind))
#
# # Goodness-of-fit tests
# (ks_result <- ks.test(wind, simulated_wind))
# (ad_result <- ad_test(wind, simulated_wind))
# (cvm_result <- cvm_test(wind, simulated_wind))
#
# # Compare EPD fit using RMSE and MAE
# fitted_vals <- epd_pdf(v_vals)
# # Root Mean Squared Error
# (rmse <- sqrt(mean((fitted_vals - dens$y)^2)))
# # Mean Absolute Error
# (mae <- mean(abs(fitted_vals - dens$y)))
#
# # Save to PNG
# png("epd_fit_comparison.png", width = 1200, height = 600)
#
# # Set layout: 2 plots side by side
# par(mfrow = c(1, 1))
#
# # Histogram with EPD Fit
# hist(wind, probability = TRUE, breaks = 40,
#      col = rgb(0, 1, 0, 0.3),
#      main = "Original vs. Simulated Wind Speed",
#      xlab = "Wind Speed")
# lines(v_vals, dens$y, col = "darkgreen", lwd = 2)
# hist(simulated_wind, probability = TRUE, breaks = 40,
#      col = rgb(0, 0, 1, 0.3), add = TRUE)
# curve(epd_pdf(x), from = 0, to = upper_bound, col = "red", lwd = 2, add = TRUE)
# legend("topright", legend = c("Original Data Density", "Simulated Data", "Fitted EPD"),
#        fill = c(rgb(0, 1, 0, 0.3), rgb(0, 0, 1, 0.3), NA),
#        border = c("darkgreen", "blue", NA),
#        lwd = c(2, NA, 2), col = c("darkgreen", NA, "red"))
#
# # # Absolute Error Plot
# # plot(v_vals, abs(fitted_vals - dens$y), type = "l", col = "purple",
# #      ylab = "Absolute Error", xlab = "Wind Speed",
# #      main = "Pointwise Absolute Error (EPD vs Empirical)")
# dev.off()


######################################## For simulation
# # Rejection sampling from EPD
# simulate_epd <- function(n) {
#   samples <- numeric(n)
#   max_pdf <- 3.468017#max(epd_pdf(v_vals))
#   count <- 0
#   while (count < n) {
#     u <- runif(1, 0, 4.8)#upper_bound)
#     w <- runif(1)
#     if (w < epd_pdf(u) / max_pdf) {
#       count <- count + 1
#       samples[count] <- u
#     }
#   }
#   samples
# }
#
# # Simulate data
# simulated_wind <- simulate_epd(n)


##############################################################################
# MeasureOpeningmm <- sample(c(0, 2, 4, 10), size = n_total, replace = TRUE, prob = c(37, 39, 107, 73) / 256)




########################################################################################
################################### Survival times #####################################
##########################################################################################

# # Step 1: Baseline CDF
# T_max    <- 600    # 6 hours in minutes
# k        <- 600/10     # number of control points
# time_grid <- 1:T_max
#
# # 1a) Choose control points
# x_pts <- sort(c(1, sample(2:(T_max-1), k-2), T_max))
# y_pts <- c(0, sort(runif(k-2)), 1)
#
# # 1b) Build piecewise‐linear CDF
# cdf_fun <- approxfun(x_pts, y_pts, method = "linear", rule = 2)
# Fvals   <- cdf_fun(time_grid)
#
# # 1c) Plot it
# plot(time_grid, Fvals, type="l", lwd=2, col="steelblue",
#      xlab="Time (min)", ylab="F₀(t)", main="Baseline CDF")
# points(x_pts, y_pts, pch=19, col="red")
#
#
#
# # Step 2: Baseline Survivor
# surv_vals <- 1 - Fvals
#
# # Plot
# plot(time_grid, surv_vals, type="l", lwd=2, col="darkgreen",
#      xlab="Time (min)", ylab="S₀(t)", main="Baseline Survivor")
#
#
# # Step 3: Baseline PDF & Hazard
# dt        <- diff(time_grid)
# pdf_vals  <- c(0, diff(Fvals)/dt)    # f₀(t_j) ≈ ΔF/Δt
# haz_vals  <- pdf_vals / surv_vals    # λ₀(t_j) = f₀(t_j)/S₀(t_j)
#
# # Plot PDF and Hazard side by side
# par(mfrow = c(1,2), mar = c(4,4,2,1))
# plot(time_grid, pdf_vals, type="l", lwd=2, col="purple",
#      xlab="Time", ylab="f₀(t)", main="Baseline PDF")
# plot(time_grid, haz_vals, type="l", lwd=2, col="firebrick",
#      xlab="Time", ylab=expression(lambda[0](t)), main="Baseline Hazard")
# par(mfrow = c(1,1))


###############################################################################

generate_baseline_functions <- function(T_max = 600,
                                        k = T_max / 10,
                                        plot = F,
                                        spline_type = "hyman",
                                        n = 100) {
  # Time grid
  time_grid <- seq(1, T_max, length.out = n)

  # Choose control points (x, y)
  x_pts <- sort(c(1, sample(2:(T_max - 1), k - 2), T_max))
  y_pts <- c(0, sort(runif(k - 2)), 1)

  # Cubic spline CDF
  cdf_fun <- splinefun(x_pts, y_pts, method = spline_type)
  Fvals <- cdf_fun(time_grid)

  # Survivor function: S₀(t) = 1 - F₀(t)
  surv_vals <- 1 - Fvals

  dt <- diff(time_grid)
  pdf_vals <- c(NA, diff(Fvals) / dt)
  haz_vals <- pdf_vals / surv_vals

  # Generate n uniform random numbers between 0 and 1
  U <- runif(n)

  # Interpolate survival times using cubic splines
  survival_times <- sapply(U, function(u) {
    spline_inv_fun <- splinefun(surv_vals, time_grid, method = spline_type)
    spline_inv_fun(u)
  })

  # Optional plotting
  if (plot) {
    op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

    plot(time_grid, Fvals,
      type = "l", lwd = 2, col = "steelblue",
      xlab = "Time (min)", ylab = "F₀(t)", main = "Baseline CDF"
    )
    points(x_pts, y_pts, pch = 19, col = "red")

    plot(time_grid, surv_vals,
      type = "l", lwd = 2, col = "darkgreen",
      xlab = "Time (min)", ylab = "S₀(t)", main = "Baseline Survivor"
    )

    plot(time_grid, pdf_vals,
      type = "l", lwd = 2, col = "purple",
      xlab = "Time (min)", ylab = "f₀(t)", main = "Baseline PDF"
    )

    plot(time_grid, haz_vals,
      type = "l", lwd = 2, col = "firebrick",
      xlab = "Time (min)", ylab = expression(lambda[0](t)),
      main = "Baseline Hazard"
    )

    par(op)
  }

  # Return the generated survival times and other details
  return(list(
    time = time_grid,
    cdf = Fvals,
    surv = surv_vals,
    survival_times = survival_times, # Return the survival times
    lambda0 = haz_vals,
    pdf_vals = pdf_vals,
    control_points = data.frame(x = x_pts, y = y_pts)
  ))
}

# #Call the function to generate 100 survival times with a fixed seed
# result <- generate_baseline_functions(n = 100)
#
# # View the first few survival times
# (result$survival_times)
# (result$cdf)
# (result$pdf_vals)
# (result$surv)
# (result$lambda0)
#


generate_baseline_from_pilot <- function(basehaz_df,
                                         n = 100,
                                         plot = FALSE,
                                         spline_type = "hyman") {
  # Ensure the basehaz_df has columns named "time" and "hazard"
  stopifnot(all(c("time", "hazard") %in% colnames(basehaz_df)))

  # Extract time and cumulative hazard
  time_pilot <- basehaz_df$time
  H0_pilot <- basehaz_df$hazard

  # Interpolate H₀(t) to a uniform grid
  T_max <- max(time_pilot)
  time_grid <- seq(min(time_pilot), T_max, length.out = n)
  H0_interp <- approx(x = time_pilot, y = H0_pilot, xout = time_grid)$y

  # Compute survival and CDF
  surv_vals <- exp(-H0_interp)
  Fvals <- 1 - surv_vals

  # Approximate density f₀(t)
  dt <- diff(time_grid)
  pdf_vals <- c(NA, diff(Fvals) / dt)

  # Compute hazard λ₀(t)
  haz_vals <- pdf_vals / surv_vals

  # Generate n uniform random numbers between 0 and 1
  U <- runif(n)

  # Invert survival function to sample survival times
  spline_inv_fun <- splinefun(surv_vals, time_grid, method = spline_type)
  survival_times <- spline_inv_fun(U)

  # Optional plots
  if (plot) {
    op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

    plot(time_grid, Fvals,
      type = "l", lwd = 2, col = "steelblue",
      xlab = "Time", ylab = "F₀(t)", main = "Baseline CDF"
    )

    plot(time_grid, surv_vals,
      type = "l", lwd = 2, col = "darkgreen",
      xlab = "Time", ylab = "S₀(t)", main = "Baseline Survivor"
    )

    plot(time_grid, pdf_vals,
      type = "l", lwd = 2, col = "purple",
      xlab = "Time", ylab = "f₀(t)", main = "Baseline PDF"
    )

    plot(time_grid, haz_vals,
      type = "l", lwd = 2, col = "firebrick",
      xlab = "Time", ylab = expression(lambda[0](t)),
      main = "Baseline Hazard"
    )

    par(op)
  }

  # Return results
  return(list(
    time            = time_grid,
    cdf             = Fvals,
    surv            = surv_vals,
    survival_times  = survival_times,
    lambda0         = haz_vals,
    pdf_vals        = pdf_vals,
    baseline_input  = basehaz_df
  ))
}
