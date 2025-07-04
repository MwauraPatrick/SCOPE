# rm(list = ls())
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# 0) PACKAGES

library(survival)
library(tidyverse)
library(gridExtra)
library(kSamples)
library(cramer)
library(data.table)
library(patchwork)
library(patchwork)
#
#
# valid_data <- read.csv("SCOPE/data/valid_data_final.csv", header = T)
# SolarData_september <- read.csv("SCOPE/data/SolarData_september_updated.csv")
#


###################################################################################

# compress PlasticBag due to few observations
valid_data$PlasticBag <- ifelse(valid_data$PlasticBag == 0, 0, 1)
# Collapse plastic bag (0= no bag or bag open and 1 bag present and closed)
valid_data <- valid_data %>%
  mutate(Plstbag = ifelse(PlasticBag == 0, 0, ifelse(PlasticBag == 1 & BagClosed == 0, 0, 1)))
# Delete 13 observations above 70 before the start of the experiment
valid_data <- valid_data %>%
  group_by(exp_window) %>%
  filter(first(T1) < 70)
# Filter out rows with 2mm and keep only 4mm and 10mm
# valid_data <-valid_data[valid_data$MeasureOpeningmm %in% c(4, 10), ]

# Add baseline temperature for each experiment window
valid_data <- valid_data %>%
  group_by(exp_window) %>%
  mutate(T1_baseline = first(T1))
# Calculate total time within each experiment group and add column T2.70
valid_data <- valid_data %>%
  group_by(exp_window) %>%
  mutate(cumtime = cumsum(time_diff_minutes)) %>%
  mutate(T2.70 = ifelse(T2 >= 70, 1, 0))

# valid_data$cumtime

# valid_data %>% group_by(exp_window) %>% dplyr::filter(Cooker=="Brother") %>% dplyr::select(Cooker,time_diff_minutes, cumtime,T2.70)


## Define the true event
valid_data <- valid_data %>%
  group_by(exp_window) %>%
  filter(if (any(T2.70 == 1)) {
    row_number() == min(which(T2.70 == 1)) | (T2.70 == 0 & cumsum(T2.70) < 1)
  } else {
    TRUE
  })
# Discard some devices due to data imbalance
# valid_data1 <- subset(valid_data1, (Cooker %in% c("YamoDudo","Brother","OvenProto2",
#                                                       "Proto2","Proto3", "Proto4")))

valid_data <- valid_data %>% filter(time_diff_minutes > 0)

# Reformat data into start, stop, status
valid_data <- valid_data %>%
  group_by(exp_window) %>%
  mutate(
    interval_start = lag(cumsum(time_diff_minutes), default = 0),
    interval_end = cumsum(time_diff_minutes),
    start = interval_start,
    stop = interval_end,
    interval = paste0(interval_start, "-", interval_end)
  ) # %>%  ungroup()


valid_data <- valid_data %>% filter(H1_dec >= 10 & H1_dec <= 14)

# table(valid_data$Cooker,valid_data$T2.70)

# write.csv(valid_data, "valid_data_final.csv")

#
# valid_data_ordered <- valid_data[order(valid_data$H1_dec), ]
# plot(valid_data_ordered$H1_dec, valid_data_ordered$I_avg, "l")


# Test if the generated data conforms to the original referenced data
test_distribution_similarity <- function(results, valid_data, var_list = NULL) {
  # Require Anderson–Darling package
  if (!requireNamespace("kSamples", quietly = TRUE)) {
    stop("Package 'kSamples' required for Anderson–Darling test")
  }

  # Pick numeric vars if none given
  if (is.null(var_list)) {
    common <- intersect(names(results), names(valid_data))
    var_list <- common[sapply(results[common], is.numeric)]
  }

  test_results <- list()

  for (var in var_list) {
    x <- na.omit(results[[var]])
    y <- na.omit(valid_data[[var]])

    # Skip if too few data points
    if (length(x) < 2 || length(y) < 2) {
      warning("Insufficient data for variable: ", var)
      next
    }

    cat("Variable:", var, "\n",
      "  results n=", length(x), " mean=", mean(x), " sd=", sd(x), "\n",
      "  valid   n=", length(y), " mean=", mean(y), " sd=", sd(y), "\n",
      sep = ""
    )

    # 1) Kolmogorov–Smirnov
    ks_res <- tryCatch(
      ks.test(x, y),
      error = function(e) {
        warning("KS test failed for ", var, ": ", e$message)
        NULL
      }
    )
    if (is.null(ks_res)) next
    ks_stat <- as.numeric(ks_res$statistic)
    ks_p <- as.numeric(ks_res$p.value)

    # 2) Anderson–Darling
    ad_stat <- NA_real_
    ad_p <- NA_real_

    ad_res <- tryCatch(
      kSamples::ad.test(list(x, y)),
      error = function(e) {
        warning("AD test failed for ", var, ": ", e$message)
        NULL
      }
    )

    if (!is.null(ad_res)) {
      # kSamples::ad.test returns an 'ad' matrix: [A, Z, P-value]
      if (!is.null(ad_res$ad) && ncol(ad_res$ad) >= 3) {
        ad_stat <- as.numeric(ad_res$ad[1, 1])
        ad_p <- as.numeric(ad_res$ad[1, 3])
      }
    }

    test_results[[var]] <- list(
      KS_Statistic  = ks_stat,
      KS_pvalue     = ks_p,
      AD_Statistic  = ad_stat,
      AD_pvalue     = ad_p
    )
  }

  # Build data frame
  if (length(test_results) == 0) {
    warning("No valid test results could be computed.")
    return(NULL)
  }
  df <- do.call(rbind, lapply(names(test_results), function(v) {
    r <- test_results[[v]]
    data.frame(
      Variable = v,
      KS_Statistic = r$KS_Statistic,
      KS_pvalue = r$KS_pvalue,
      AD_Statistic = r$AD_Statistic,
      AD_pvalue = r$AD_pvalue,
      stringsAsFactors = FALSE
    )
  }))
  rownames(df) <- NULL
  return(df)
}


###################################################################
# Cooker <- rep(c("Cooker1", "Cooker2"), each = N)
# Cooker_indicator <- ifelse(Cooker == "Cooker2", 1, 0)
# T1_baseline <- rbeta(total_n, shape1 = T1_shape1, shape2 = T1_shape2)
# Ta_avg <- runif(total_n, min = Ta_avg_min, max = Ta_avg_max)
# I_avg <- rweibull(total_n, shape = I_avg_shape, scale = I_avg_scale)
# Wind <- rweibull(total_n, shape = Wind_shape, scale = Wind_scale)
# MeasureOpeningmm <- sample(MeasureOpeningmm_values, size = total_n, replace = TRUE,
#                            prob = MeasureOpeningmm_prob)
#
# # Variable dependance on time of the experiment
# valid_data_ordered <- valid_data[order(valid_data$H1_dec), ]
# plot(valid_data_ordered$H1_dec,valid_data_ordered$T1_baseline,"l")
# plot(valid_data_ordered$H1_dec,valid_data_ordered$Wind,"l")
# plot(valid_data_ordered$H1_dec,valid_data_ordered$Ta_avg,"l")
# plot(valid_data_ordered$H1_dec,valid_data_ordered$I_avg,"l")

#    if you wanted descending instead:
# valid_data <- valid_data[order(-valid_data$H1_dec), ]



##########################################################################################################################
# Baseline survival times
#
#
# s <- with(valid_data, Surv(start, stop, T2.70))
# fit <- coxph(
#   s ~ as.factor(Cooker) + T1_baseline + Ta_avg + I_avg + Wind +
#     as.factor(PlasticBag) + MeasureOpeningmm +
#     as.factor(PlasticBag):as.factor(Cooker) + cluster(exp_window),
#   data   = valid_data,
#   method = "efron",
#   model  = TRUE
# )
#
#
# baseline_haz <- basehaz(fit, centered = FALSE)
# times_p <- baseline_haz$time
# H0_p    <- baseline_haz$hazard
#
#
# H0_fun <- splinefun(times_p, H0_p, method = "hyman")
#
#
# t_min    <- min(times_p)
# t_max    <- max(times_p)
# time_grid <- seq(t_min, t_max, length.out = 100)
#
#
# H0_grid <- H0_fun(time_grid)
# h0_grid <- H0_fun(time_grid, deriv = 1)
#
#
# S0_grid <- exp(-H0_grid)
# F0_grid <- 1 - S0_grid
# f0_grid <- S0_grid * h0_grid
#
#
#
# plot(baseline_haz$time, baseline_haz$hazard, type = "l",
#      xlab = "Time", ylab = "Baseline Hazard", col = "black", lwd = 2)
# lines(time_grid, H0_grid, type = "o", col = "red", pch = 16)
# legend("topleft", legend = c("Estimated", "True"), col = c("black", "red"),
#        lty = 1, pch = c(NA, 16), lwd = 2)
#
#
# png("baseline_hazard_plot.png", width = 800, height = 600)
# plot(baseline_haz$time, baseline_haz$hazard, type = "l",
#      xlab = "Time", ylab = "Baseline Hazard", col = "black", lwd = 2)
# lines(time_grid, H0_grid, type = "o", col = "red", pch = 16)
# legend("topleft", legend = c("Estimated", "True"),
#        col = c("black", "red"), lty = 1, pch = c(NA, 16), lwd = 2)
# dev.off()



###############################################################################################
#
# # Handling the irradiation
#
# I_avg<-valid_data$I_avg
# Hour_of_the_day <- as.POSIXct(valid_data$H1, format = "%Y-%m-%d %H:%M:%S")
# I_avg_hour<- cbind(Hour_of_the_day, I_avg)
# I_avg_hour<- as.data.frame(I_avg_hour)
# I_avg_hour$Hour_of_the_day <- as.POSIXct(I_avg_hour$Hour_of_the_day, origin = "1970-01-01")
# I_avg_hour$H1_dec <- as.numeric(format(I_avg_hour$Hour_of_the_day, "%H")) +
#                            as.numeric(format(I_avg_hour$Hour_of_the_day, "%M")) / 60
# I_avg_hour_sorted <- I_avg_hour[order(I_avg_hour$H1_dec), ]
# head(I_avg_hour_sorted)
# plot(I_avg_hour_sorted$H1_dec, I_avg_hour_sorted$I_avg,
#      type = "l",
#      xlab = "Hour of the Day",
#      ylab = expression(I[avg]),
#      main = expression(paste("Hourly Variation of ", I[avg], " with Cubic Spline Fit")),
#      col = "blue",
#      lwd = 2)
# n_points <- 100
# hour_seq <- seq(min(I_avg_hour_sorted$H1_dec),
#                 max(I_avg_hour_sorted$H1_dec),
#                 length.out = n_points)
#
# spline_fit <- spline(I_avg_hour_sorted$H1_dec, I_avg_hour_sorted$I_avg, xout = hour_seq)
#
# lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, lty = 2)
# legend("topleft",
#        legend = c("Observed Line", "Cubic Spline Interpolation"),
#        col = c("blue", "red"),
#        lty = c(1, 2),
#        lwd = 2,
#        bty = "n")
# png("I_avg_hourly_spline_plot.png", width = 800, height = 600)
# plot(I_avg_hour_sorted$H1_dec, I_avg_hour_sorted$I_avg,
#      type = "l",
#      xlab = "Hour of the Day",
#      ylab = expression(I[avg]),
#      main = expression(paste("Hourly Variation of ", I[avg], " with Cubic Spline Fit")),
#      col = "blue",
#      lwd = 2)
# n_points <- 100
# hour_seq <- seq(min(I_avg_hour_sorted$H1_dec),
#                 max(I_avg_hour_sorted$H1_dec),
#                 length.out = n_points)
#
# spline_fit <- spline(I_avg_hour_sorted$H1_dec, I_avg_hour_sorted$I_avg, xout = hour_seq)
#
# lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, lty = 2)
#
# legend("topleft",
#        legend = c("Observed Line", "Cubic Spline Interpolation"),
#        col = c("blue", "red"),
#        lty = c(1, 2),
#        lwd = 2,
#        bty = "n")
# dev.off()

##################################

# plot_I_avg_spline <- function(valid_data, n = 100) {
#   I_avg <- valid_data$I_avg
#   Hour_of_the_day <- as.POSIXct(valid_data$H1, format = "%Y-%m-%d %H:%M:%S")
#   I_avg_hour <- cbind(Hour_of_the_day, I_avg)
#   I_avg_hour <- as.data.frame(I_avg_hour)
#   I_avg_hour$Hour_of_the_day <- as.POSIXct(I_avg_hour$Hour_of_the_day, origin = "1970-01-01")
#   I_avg_hour$H1_dec <- as.numeric(format(I_avg_hour$Hour_of_the_day, "%H")) +
#     as.numeric(format(I_avg_hour$Hour_of_the_day, "%M")) / 60
#   I_avg_hour_sorted <- I_avg_hour[order(I_avg_hour$H1_dec), ]
#   plot(I_avg_hour_sorted$H1_dec, I_avg_hour_sorted$I_avg,
#        type = "l",
#        xlab = "Hour of the Day",
#        ylab = expression(I[avg]),
#        main = expression(paste("Hourly Variation of ", I[avg], " with Cubic Spline Fit")),
#        col = "blue",
#        lwd = 2)
#   hour_seq <- seq(min(I_avg_hour_sorted$H1_dec),
#                   max(I_avg_hour_sorted$H1_dec),
#                   length.out = n)
#
#   spline_fit <- spline(I_avg_hour_sorted$H1_dec,
#                        I_avg_hour_sorted$I_avg,
#                        xout = hour_seq)
#   lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, lty = 2)
#   legend("topleft",
#          legend = c("Observed Line", "Cubic Spline Interpolation"),
#          col = c("blue", "red"),
#          lty = c(1, 2),
#          lwd = 2,
#          bty = "n")
#   interpolated_df <- data.frame(H1_dec = spline_fit$x,
#                                 I_avg_interp = spline_fit$y)
#   return(interpolated_df)
# }

# (interpolated_data <- plot_I_avg_spline(valid_data, n = 150))
# head(interpolated_data)

################################################################################################################################
#
# # Ambient Temparature
#
# Ta_avg<-valid_data$Ta_avg
# Hour_of_the_day <- as.POSIXct(valid_data$H1, format = "%Y-%m-%d %H:%M:%S")
# Ta_avg_hour<- cbind(Hour_of_the_day, Ta_avg)
# Ta_avg_hour<- as.data.frame(Ta_avg_hour)
# Ta_avg_hour$Hour_of_the_day <- as.POSIXct(Ta_avg_hour$Hour_of_the_day, origin = "1970-01-01")
# Ta_avg_hour$H1_dec <- as.numeric(format(Ta_avg_hour$Hour_of_the_day, "%H")) +
#   as.numeric(format(Ta_avg_hour$Hour_of_the_day, "%M")) / 60
# Ta_avg_hour_sorted <- Ta_avg_hour[order(Ta_avg_hour$H1_dec), ]
#
# head(Ta_avg_hour_sorted)
# plot(Ta_avg_hour_sorted$H1_dec, Ta_avg_hour_sorted$Ta_avg,
#      type = "l",
#      xlab = "Hour of the Day",
#      ylab = expression(Ta[avg]),
#      main = expression(paste("Hourly Variation of ", Ta[avg], " with Cubic Spline Fit")),
#      col = "blue",
#      lwd = 2)
# n_points <- 100
# hour_seq <- seq(min(Ta_avg_hour_sorted$H1_dec),
#                 max(Ta_avg_hour_sorted$H1_dec),
#                 length.out = n_points)
#
# spline_fit <- spline(Ta_avg_hour_sorted$H1_dec, Ta_avg_hour_sorted$Ta_avg, xout = hour_seq)
#
# lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, lty = 2)
# legend("topleft",
#        legend = c("Observed Line", "Cubic Spline Interpolation"),
#        col = c("blue", "red"),
#        lty = c(1, 2),
#        lwd = 2,
#        bty = "n")
#########################################################################################################
#
# # # All the other variables
# get_spline_df_dup_cookers <- function(valid_data, var_list, n = 20, seed = 456) {
#   if (!is.null(seed)) set.seed(seed)
#
#   # Ensure Td and exp_window are included
#   if ("Td" %in% names(valid_data) && !"Td" %in% var_list) {
#     var_list <- c(var_list, "Td")
#   }
#   if ("exp_window" %in% names(valid_data) && !"exp_window" %in% var_list) {
#     var_list <- c(var_list, "exp_window")
#   }
#
#   # Parse dates
#   ts <- as.POSIXct(valid_data$H1, format = "%Y-%m-%d %H:%M:%S")
#   valid_data$TestDate <- as.Date(ts)
#
#   # Normalize I_avg
#   if ("I_avg" %in% var_list) {
#     max_Iavg <- max(valid_data$I_avg, na.rm = TRUE)
#     valid_data$I_avg <- valid_data$I_avg / max_Iavg
#   }
#
#   # Subset complete cases
#   time_df <- valid_data[complete.cases(valid_data[, var_list]),
#                         c("H1_dec", "TestDate", var_list)]
#   hour_seq <- seq(min(time_df$H1_dec), max(time_df$H1_dec), length.out = n)
#
#   # STEP 1: Smoothed daily profiles
#   smoothed_profiles <- do.call(rbind, lapply(split(time_df, time_df$TestDate), function(daily) {
#     out <- data.frame(H1_dec = hour_seq, TestDate = unique(daily$TestDate))
#     for (v in var_list) {
#       if (length(unique(daily$H1_dec)) > 2) {
#         out[[v]] <- spline(daily$H1_dec, daily[[v]],
#                            xout = hour_seq, method = "fmm")$y
#       } else {
#         out[[v]] <- rep(mean(daily[[v]], na.rm = TRUE), length(hour_seq))
#       }
#     }
#     out
#   }))
#
#   # STEP 2: Residuals (centered)
#   resids_long <- do.call(rbind, lapply(split(time_df, time_df$TestDate), function(daily) {
#     smooth_d <- smoothed_profiles[smoothed_profiles$TestDate == unique(daily$TestDate), ]
#     do.call(rbind, lapply(seq_len(nrow(daily)), function(i) {
#       hr  <- daily$H1_dec[i]
#       idx <- which.min(abs(smooth_d$H1_dec - hr))
#       vals <- sapply(var_list, function(v) daily[[v]][i] - smooth_d[[v]][idx])
#       data.frame(H1_dec = hr,
#                  TestDate = daily$TestDate[i],
#                  as.list(vals),
#                  stringsAsFactors = FALSE)
#     }))
#   }))
#   # center each residual column
#   for (v in var_list) {
#     resids_long[[v]] <- resids_long[[v]] - mean(resids_long[[v]], na.rm = TRUE)
#   }
#
#   # STEP 3: Sample and add residuals grouped by exp_window
#   sampled_dates <- sample(unique(smoothed_profiles$TestDate), 2, replace = TRUE)
#   get_with_resid <- function(date) {
#     base_df <- smoothed_profiles[smoothed_profiles$TestDate == date, ]
#     for (v in var_list) {
#       base_df[[v]] <- mapply(function(hr, d, val) {
#         rows <- which(
#           abs(resids_long$H1_dec - hr) < 0.25 &
#             resids_long$TestDate == d
#         )
#         if (length(rows) > 0) {
#           subset_resid <- resids_long[[v]][rows]
#         } else {
#           subset_resid <- resids_long[[v]]
#         }
#         val + sample(subset_resid, 1)
#       }, base_df$H1_dec, base_df$TestDate, base_df[[v]])
#     }
#     base_df
#   }
#
#   cooker1_df <- get_with_resid(sampled_dates[1])
#   cooker2_df <- get_with_resid(sampled_dates[2])
#
#   # Clamp wind
#   if ("Wind" %in% var_list) {
#     clamp_wind <- function(df) pmin(pmax(df$Wind, 0), 2.5)
#     cooker1_df$Wind <- clamp_wind(cooker1_df)
#     cooker2_df$Wind <- clamp_wind(cooker2_df)
#   }
#
#   # Combine
#   cooker1_df$Cooker <- "Cooker1"
#   cooker2_df$Cooker <- "Cooker2"
#   final_df <- rbind(cooker1_df, cooker2_df)
#   final_df$Cooker_indicator <- as.integer(final_df$Cooker == "Cooker2")
#   rownames(final_df) <- NULL
#
#   # STEP 4: Quantile mapping back to original marginal distributions
#   for (v in var_list) {
#     orig_vals <- sort(valid_data[[v]][!is.na(valid_data[[v]])])
#     ranks     <- rank(final_df[[v]], ties.method = "average")
#     final_df[[v]] <- orig_vals[ceiling(ranks / max(ranks) * length(orig_vals))]
#   }
#   if ("I_avg" %in% var_list) {
#     final_df$I_avg <- final_df$I_avg * max_Iavg
#   }
#
#   return(final_df)
# }
#
# get_spline_df_dup_cookers <- function(valid_data, var_list, n = 20, seed = 456) {
#
#   # Restore original RNG state on exit
#   rng_state <- if (exists(".Random.seed", inherits = FALSE)) .Random.seed else NULL
#   on.exit({
#     if (!is.null(rng_state)) .Random.seed <<- rng_state
#     else rm(.Random.seed, envir = .GlobalEnv)
#   }, add = TRUE)
#
# # Set seed
#   set.seed(as.integer(Sys.time()) %% .Machine$integer.max)
#
# # Subset to complete cases for selected variables
#   time_df <- valid_data[complete.cases(valid_data[, var_list]),
#                         c("H1_dec", "TestDate", var_list)]
#
# # Create dense grid for interpolation
#   dense_grid <- seq(min(time_df$H1_dec), max(time_df$H1_dec), length.out = 1000)
# # Take index samples
#   sampled_idx <- sample(seq_along(dense_grid), size = n, replace = FALSE)
#   hour_seq <- sort(dense_grid[sampled_idx])
#
# # STEP 1: Smooth daily profiles via spline
#   smoothed_profiles <- do.call(rbind, lapply(split(time_df, time_df$TestDate), function(daily) {
#     out <- data.frame(H1_dec = hour_seq, TestDate = unique(daily$TestDate))
#     for (v in var_list) {
#       if (length(unique(daily$H1_dec)) > 2) {
#         out[[v]] <- spline(daily$H1_dec, daily[[v]], xout = hour_seq, method = "fmm")$y
#       } else {
#         out[[v]] <- rep(mean(daily[[v]], na.rm = TRUE), length(hour_seq))
#       }
#     }
#     out
#   }))
#
#  # STEP 2: Compute residuals and centre them
#   resids_long <- do.call(rbind, lapply(split(time_df, time_df$TestDate), function(daily) {
#     smooth_d <- smoothed_profiles[smoothed_profiles$TestDate == unique(daily$TestDate), ]
#     do.call(rbind, lapply(seq_len(nrow(daily)), function(i) {
#       hr <- daily$H1_dec[i]
#       idx <- which.min(abs(smooth_d$H1_dec - hr))
#       vals <- sapply(var_list, function(v) daily[[v]][i] - smooth_d[[v]][idx])
#       data.frame(H1_dec = hr,
#                  TestDate = daily$TestDate[i],
#                  as.list(vals),
#                  stringsAsFactors = FALSE)
#     }))
#   }))
#   for (v in var_list) {
#     resids_long[[v]] <- resids_long[[v]] - mean(resids_long[[v]], na.rm = TRUE)
#   }
#
# # STEP 3: Sample two dates for Cooker1 and Cooker2 and add residuals
#   sampled_dates <- sample(unique(smoothed_profiles$TestDate), 2, replace = TRUE)
#   make_cooker_df <- function(date) {
#     base_df <- smoothed_profiles[smoothed_profiles$TestDate == date, ]
#     for (v in var_list) {
#       base_df[[v]] <- mapply(function(hr, d, val) {
#         rows <- which(abs(resids_long$H1_dec - hr) < 0.25 & resids_long$TestDate == d)
#         subset_resid <- if (length(rows) > 0) resids_long[[v]][rows] else resids_long[[v]]
#         val + sample(subset_resid, 1)
#       }, base_df$H1_dec, base_df$TestDate, base_df[[v]])
#     }
#     return(base_df)
#   }
#
#   cooker1_df <- make_cooker_df(sampled_dates[1])
#   cooker2_df <- make_cooker_df(sampled_dates[2])
#
#   # Clamp Wind to realistic range
#   if ("Wind" %in% var_list) {
#     clamp_wind <- function(df) pmin(pmax(df$Wind, 0), 2.5)
#     cooker1_df$Wind <- clamp_wind(cooker1_df)
#     cooker2_df$Wind <- clamp_wind(cooker2_df)
#   }
#
#   # Combine profiles and assign Cooker labels before quantile mapping
#   cooker1_df$Cooker <- "Cooker1"
#   cooker2_df$Cooker <- "Cooker2"
#   final_df <- rbind(cooker1_df, cooker2_df)
#   final_df$Cooker_indicator <- as.integer(final_df$Cooker == "Cooker2")
#   rownames(final_df) <- NULL
#
#   # STEP 4: Quantile mapping back to original marginals
#   for (v in var_list) {
#     orig_vals <- sort(valid_data[[v]][!is.na(valid_data[[v]])])
#     ranks     <- rank(final_df[[v]], ties.method = "average")
#     final_df[[v]] <- orig_vals[ceiling(ranks / max(ranks) * length(orig_vals))]
#   }
#   if ("I_avg" %in% var_list) {
#     final_df$I_avg <- final_df$I_avg * max_Iavg
#   }
#
#   return(final_df)
# }

# # 1. Define which variables to interpolate
# var_list <- c("I_avg", "Ta_avg", "Wind", "T1_baseline")
#
# # 2. Pull out only the columns we need, and ensure TestDate is in Date form
# time_df <- valid_data %>%
#   dplyr::select(TestDate, H1_dec, all_of(var_list)) %>%
#   dplyr::mutate(
#     # if TestDate is not already Date, convert it
#     TestDate = as.Date(TestDate)
#   ) %>%
#   # 3. Keep only rows with no missing values in any of these columns
#   dplyr::filter(complete.cases(.))
#
# # 4. Inspect the result
# str(time_df)
# summary(time_df)
# head(time_df, 10)
#
#
# library(dplyr)
#
# # 1. Split by TestDate
# by_date <- split(time_df, time_df$TestDate)
#
# # 2. Interpolate per date
# interp_list <- lapply(by_date, function(df) {
#   this_date <- unique(df$TestDate)
#
#   # Build a dense grid of 500 points on this date
#   grid <- seq(min(df$H1_dec), max(df$H1_dec), length.out = 500)
#   out  <- data.frame(TestDate = this_date, H1_dec = grid)
#
#   # For each variable (including T1_baseline), either spline or repeat
#   for (var in var_list) {
#     vals <- df[[var]]
#     if (length(unique(vals)) < 2) {
#       # constant: just repeat that one value
#       out[[var]] <- rep(unique(vals), length(grid))
#     } else {
#       # time‐varying: fit cubic spline and evaluate on grid
#       fn         <- splinefun(x = df$H1_dec, y = vals, method = "fmm")
#       out[[var]] <- fn(grid)
#     }
#   }
#
#   out
# })
#
# # 3. Drop NULLs (if any) and bind into one data.frame
# interp_all <- bind_rows(interp_list)
#
# # 4. Quick check
# str(interp_all)
# head(interp_all)
# summary(interp_all)
#
#
# # --- STEP 3: SAMPLE FROM THE INTERPOLATED TABLE ---
#
# # 1. Set your desired simulation size
# n <- 20
#
# # 2. Determine total number of interpolated points
# M_total <- nrow(interp_all)
#
# # 3. Draw indices (with replacement)
# set.seed(2025)
# idx <- sample.int(M_total, size = n, replace = TRUE)
#
# # 4. Subset to your simulated dataset
# sim_data <- interp_all[idx, ]
#
# # 5. Inspect your simulation draw
# str(sim_data)
# summary(sim_data)
# head(sim_data)
#
#
# # Assume sim_data already exists from Step 3
#
# set.seed(2025)  # for reproducibility
#
# sim_data$Cooker <- sample(
#   x      = c("Cooker1", "Cooker2"),
#   size   = nrow(sim_data),
#   replace= TRUE,
#   prob   = c(0.5, 0.5)
# )
#
# # Quick
# table(sim_data$Cooker)
# head(sim_data)

#####################################################################################################


#' @param valid_data data.frame; must contain columns TestDate, H1_dec, and variables in var_list
#' @param var_list character vector; variable names to interpolate (e.g., c("I_avg","Ta_avg","Wind","T1_baseline","Td"))
#' @param n integer; number of simulated rows to draw
#' @param grid_length integer; number of grid points per TestDate for interpolation (default 500)
#' @param cooker_names character vector of length 2; labels for Cooker factor (default c("Cooker1","Cooker2"))
#' @param cooker_prob numeric vector of length 2; sampling probabilities for cooker_names (default c(0.5,0.5))
#' @param seed integer; optional seed for reproducibility
#'
#' @return data.frame with columns: TestDate, H1_dec, var_list..., Cooker, Cooker_indicator
#' @importFrom dplyr select mutate filter bind_rows
#' @export
#'


get_spline_df_dup_cookers <- function(valid_data = NULL, var_list = NULL, n = NULL, seed = 123, completed_sims = NULL, inter_count = NULL) {
  # —— set seed ——
  if (!is.null(completed_sims)) {
    set.seed(as.integer(seed + (completed_sims)))
  } else if (!is.null(inter_count)) {
    set.seed(as.integer(seed + (inter_count)))
  } else {
    set.seed(seed)
  }

  # valid_data <- get("valid_data", envir = .GlobalEnv)
  # —— prepare & interpolate original data ——
  valid_dt <- copy(as.data.table(valid_data))
  time_df <- valid_dt[, .SD, .SDcols = c("TestDate", "H1_dec", var_list)]
  time_df <- time_df[complete.cases(time_df)]
  by_date <- split(time_df, time_df$TestDate)

  interp_list <- lapply(by_date, function(df_date) {
    this_date <- df_date$TestDate[1]
    h_min <- min(df_date$H1_dec)
    h_max <- max(df_date$H1_dec)
    grid <- if (h_min == h_max) rep(h_min, 1000) else seq(h_min, h_max, length.out = 1000)
    out <- data.table(TestDate = this_date, H1_dec = grid)
    for (var in var_list) {
      dfc <- df_date[, .(val = mean(get(var), na.rm = TRUE)), by = H1_dec]
      if (nrow(dfc) < 3 || length(unique(dfc$val)) < 2) {
        out[, (var) := dfc$val[1]]
      } else {
        fit <- try(splinefun(dfc$H1_dec, dfc$val), silent = TRUE)
        pred <- fit(grid) + rnorm(length(grid), 0, 0.05)
        out[, (var) := pred]
      }
    }
    return(out)
  })
  interp_all <- rbindlist(interp_list)

  # —— ensure at least 3 samples per date in each draw ——
  date_counts <- table(time_df$TestDate)
  all_dates <- names(date_counts)
  D <- length(all_dates)
  min_per_date <- 3
  # determine which dates to pre-sample
  if (n >= min_per_date * D) {
    presel_dates <- all_dates
  } else {
    k <- floor(n / min_per_date)
    presel_dates <- names(sort(date_counts, decreasing = TRUE))[seq_len(k)]
  }
  # pre-sample exactly 3 for each selected date
  pre_idx <- unlist(lapply(presel_dates, function(d) {
    rows_d <- which(interp_all$TestDate == d)
    sample(rows_d, min_per_date, replace = length(rows_d) < min_per_date)
  }))

  # —— compute joint weights for remaining sampling ——
  date_w <- date_counts / sum(date_counts)
  interp_all[, joint_w := date_w[as.character(TestDate)]]

  # sample remaining to reach n
  rem <- n - length(pre_idx)
  w <- interp_all$joint_w
  w <- w / sum(w)
  rem_idx <- sample(seq_len(nrow(interp_all)), rem, replace = TRUE, prob = w)

  # combine indices & assemble sim_data
  sim_idx <- c(pre_idx, rem_idx)
  sim_data <- interp_all[sim_idx]


  # —— normalize covariates to original mean/sd ——
  sim_df <- as.data.frame(sim_data)
  for (var in var_list) {
    orig_m <- mean(valid_data[[var]], na.rm = TRUE)
    orig_s <- sd(valid_data[[var]], na.rm = TRUE)
    sim_m <- mean(sim_df[[var]], na.rm = TRUE)
    sim_s <- sd(sim_df[[var]], na.rm = TRUE)
    if (orig_s > 0 && sim_s > 0) {
      sim_df[[var]] <- (sim_df[[var]] - sim_m) * (orig_s / sim_s) + orig_m
    } else {
      sim_df[[var]] <- orig_m
    }
  }

  # —— clamp Wind to [0, 2.5] ——
  if ("Wind" %in% names(sim_df)) {
    sim_df$Wind <- pmax(0, pmin(sim_df$Wind, 2.5))
  }

  # —— add categorical and indicator variables ——
  # sim_df$Cooker <- sample(c("Cooker1", "Cooker2"), n, replace = TRUE)
  n_half <- floor(n / 2)
  cookers <- rep(c("Cooker1", "Cooker2"), each = n_half)
  if (2 * n_half < n) cookers <- c(cookers, sample(c("Cooker1", "Cooker2"), 1)) # add 1 if n is odd
  sim_df$Cooker <- sample(cookers) # randomly shuffle


  sim_df$Cooker_indicator <- as.integer(sim_df$Cooker == "Cooker2")
  pb_tab <- table(valid_data$PlasticBag)
  sim_df$PlasticBag <- sample(names(pb_tab), n, replace = TRUE, prob = pb_tab / sum(pb_tab))
  sim_df$PlasticBag_indicator <- as.integer(sim_df$PlasticBag == names(pb_tab)[2])
  mo_vals <- c(0, 2, 4, 10)
  mo_p <- c(37, 39, 107, 73) / sum(c(37, 39, 107, 73))
  sim_df$MeasureOpeningmm <- sample(mo_vals, n, replace = TRUE, prob = mo_p)

  # —— diagnostic print ——
  cat(sprintf("Simulated TestDate counts (n=%d):\n", n))
  print(table(sim_df$TestDate))

  return(sim_df)
}



# Define variable list
# var_list <- c("I_avg", "Ta_avg", "Wind", "Td", "T1_baseline")
var_list <- c("I_avg", "Ta_avg", "Wind", "Td", "T1_baseline")

results <- get_spline_df_dup_cookers(valid_data, var_list, n = 300)
# (cor_mat <- round(cor(results[, var_list]), 2))
#
# (cor_mat <- round(cor(valid_data[, var_list], use = "complete.obs"), 2))


table(results$Cooker)

# Distribution‐similarity test
(test_results <- test_distribution_similarity(
  results = results,
  valid_data = valid_data,
  var_list = var_list
))

summary(valid_data$I_avg)
summary(results$I_avg)


summary(results$Wind)
summary(valid_data$Wind)

summary(results$T1_baseline)
summary(valid_data$T1_baseline)

summary(results$Ta_avg)
summary(valid_data$Ta_avg)


var(valid_data$Wind)
var(results$Wind)

var(results$I_avg)
var(valid_data$I_avg)

# var(valid_data$T1_baseline)
# var(results$T1_baseline)

var(valid_data$Ta_avg)
var(results$Ta_avg)

var(valid_data$Td)
var(results$Td)


# Quick checks

valid_data <- valid_data[order(valid_data$H1_dec), ]

sim_df <- results[order(valid_data$H1_dec), ]


# Define font size
base_font_size <- 11


# Define label map for title and y-axis labels
label_map <- c(
  I_avg = expression(bold(I[avg])),
  Ta_avg = expression(bold(Ta[avg])),
  Wind = expression(bold("Wind Speed")),
  Td = expression(bold(T[d])),
  T1_baseline = expression(bold(T[1] * " baseline"))
)

# Set base font size
base_font_size <- 11

# Order the data by time
valid_data <- valid_data[order(valid_data$H1_dec), ]
sim_df <- results[order(valid_data$H1_dec), ]

# Set a larger base font size
base_font_size <- 16 # Increase this if needed

# Generate plots
plots <- lapply(var_list, function(v) {
  ggplot() +
    geom_line(
      data = valid_data,
      aes(x = H1_dec, y = .data[[v]], color = "Original"),
      size = 1, linetype = "solid"
    ) +
    geom_line(
      data = sim_df,
      aes(x = H1_dec, y = .data[[v]], color = Cooker),
      size = 1, linetype = "dotted"
    ) +
    scale_color_manual(
      values = c(Original = "darkgreen", Cooker1 = "purple", Cooker2 = "firebrick"),
      breaks = c("Original", "Cooker1", "Cooker2")
    ) +
    labs(
      title = label_map[[v]],
      x = "Time of Day",
      y = label_map[[v]],
      color = "Source"
    ) +
    theme_minimal(base_size = base_font_size) +
    theme(
      plot.title = element_text(size = base_font_size + 6, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_font_size),
      axis.text = element_text(size = base_font_size),
      legend.title = element_text(size = base_font_size + 2),
      legend.text = element_text(size = base_font_size + 2),
      legend.key.size = unit(1.2, "lines") # Optional: increase legend key size
    )
})


# Combine plots with shared legend at the bottom
combined <- wrap_plots(plots, ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")

# Save the combined plot
ggsave("plots/ggplot_orig_vs_sim.png", combined, width = 8, height = 8, units = "in", dpi = 300)

# View interactively if needed
combined



# File: R/generate_sim_plots.R

library(ggplot2)
library(patchwork)
library(ggtext) # For optional markdown-style titles

# Define variable names and LaTeX-like labels
var_list <- c("I_avg") # , "Ta_avg", "Wind", "Td", "T1_baseline")
var_labels <- c(
  I_avg = expression(I[avg]),
  Ta_avg = expression(T[a[avg]]),
  Wind = "Wind Speed",
  Td = expression(T[d]),
  T1_baseline = expression(T[1] ~ "(baseline)")
)

# Ensure consistent ordering
valid_data <- valid_data[order(valid_data$H1_dec), ]
sim_df <- results[order(results$H1_dec), ]

# # Set base font size
# base_font_size <- 11
#
# # Create list of plots
# plots <- lapply(var_list, function(v) {
#   ggplot() +
#     geom_line(
#       data = valid_data,
#       aes(x = H1_dec, y = .data[[v]], color = "Original"),
#       size = 0.2, linetype = "dashed"
#     ) +
#     geom_line(
#       data = sim_df,
#       aes(x = H1_dec, y = .data[[v]], color = Cooker),
#       size = 0.2
#     ) +
#     scale_color_manual(
#       values = c(
#         Original = "darkgreen",
#         Cooker1 = "purple",
#         Cooker2 = "firebrick"
#       ),
#       breaks = c("Original", "Cooker1", "Cooker2")
#     ) +
#     labs(
#       title = bquote("Original vs. Simulated — " ~ .(var_labels[[v]])),
#       x = "Time of Day",
#       y = NULL,
#       color = "Source"
#     ) +
#     theme_minimal(base_size = base_font_size) +
#     theme(
#       plot.title = element_text(size = base_font_size + 1, face = "bold"),
#       axis.text = element_text(size = base_font_size - 1),
#       axis.title.x = element_text(size = base_font_size),
#       axis.title.y = element_text(size = base_font_size),
#       legend.title = element_text(size = base_font_size),
#       legend.text = element_text(size = base_font_size),
#       legend.position = "bottom"
#     )
# })
#
# # Combine all plots
# combined_plot <- wrap_plots(plots, ncol = 2, guides = "collect") &
#   theme(legend.position = "bottom")
#
# # # Print or save
# # print(combined_plot)

#############################################################################################

generate_cox_baseline <- function(
    ref_cooker,
    n = 100,
    seed = 123,
    plot = FALSE,
    include_plastic_bag = TRUE, completed_sims = NULL) {
  # seed setting
  if (!is.null(completed_sims)) {
    set.seed(as.integer(123 * (completed_sims)))
  } else {
    set.seed(seed)
  }

  # Load data
  valid_data <- read.csv("data/valid_data_final.csv", header = TRUE)

  # valid_data$Cooker <- with(valid_data, fct_collapse(Cooker,ProtoGroup = c("OnlyPot","OvenProto1","Proto3")))

  # Relevel Cooker to  the specified reference
  valid_data$Cooker <- factor(valid_data$Cooker, levels = unique(valid_data$Cooker))
  valid_data$Cooker <- relevel(valid_data$Cooker, ref = ref_cooker)

  # Create Surv object
  s <- with(valid_data, survival::Surv(start, stop, T2.70))

  # Build model formula RHS
  if (include_plastic_bag) {
    rhs <- paste(
      "as.factor(Cooker)",
      "+ T1_baseline + Ta_avg + I_avg + Wind",
      "+ as.factor(PlasticBag) + MeasureOpeningmm",
      "+ as.factor(PlasticBag):as.factor(Cooker)",
      "+ cluster(exp_window)"
    )
  } else {
    rhs <- paste(
      "as.factor(Cooker)",
      "+ T1_baseline + Ta_avg + I_avg + Wind",
      "+ MeasureOpeningmm",
      "+ cluster(exp_window)"
    )
  }
  fml <- as.formula(paste("s ~", rhs))

  # Fit Cox model
  fit <- coxph(
    fml,
    data    = valid_data,
    method  = "efron",
    control = coxph.control(eps = 1e-9, iter.max = 50),
    model   = TRUE
  )

  # Estimate the baseline survival curve using KM estimator for single events
  lvl <- levels(valid_data$Cooker)

  newdata <- data.frame(
    Cooker = factor(ref_cooker, levels = lvl),
    T1_baseline = 0,
    Ta_avg = 0,
    I_avg = 0,
    MeasureOpeningmm = 0,
    Wind = 0,
    PlasticBag = factor(if (include_plastic_bag) 0 else NA,
      levels = c(0, 1)
    )
  )


  baseline_surv <- survfit(fit,
    newdata = newdata
  )

  # survival times and survival probabilities
  times_p <- baseline_surv$time
  S0_p <- baseline_surv$surv
  # S0_fun <- approxfun(times_p, S0_p, method = "constant", f = 0, rule = 2, ties = mean)
  # S0_fun <- splinefun(times_p, S0_p)
  # S0_fun <- splinefun(times_p, S0_p,method = "natural")
  # S0_fun <- splinefun(times_p, S0_p, method = "monoH.FC")
  S0_fun <- splinefun(times_p, S0_p, method = "hyman") # "monoH.FC") #"hyman")


  # Function to find median survival time
  find_median_survival <- function(S0_fun, max_time = 120) {
    for (threshold in seq(0.5, 0.9, by = 0.01)) {
      tryCatch(
        {
          return(uniroot(function(t) S0_fun(t) - threshold,
            interval = c(0, max_time)
          )$root)
        },
        error = function(e) NULL
      )
    }
    warning("No survival time found")
    return(NA)
  }

  median_survival <- find_median_survival(S0_fun)

  # Grid to evaluate H0(t)
  time_grid <- seq(min(times_p), max(times_p), length.out = 1000)
  S0_grid <- S0_fun(time_grid)
  # Sample N unique sorted indices to ensure monotonicity
  sampled_indices <- sort(sample(seq_along(time_grid), size = n, replace = T), decreasing = F)

  # Extract interpolated data from sampled points
  time_grid <- time_grid[sampled_indices]
  S0_grid <- S0_grid[sampled_indices]
  # diagnostic plot
  if (plot) {
    png(paste0("plots/baseline_cumSurvival_", ref_cooker, ".png"), 800, 600, res = 120)
    plot(times_p, S0_p,
      type = "s",
      xlab = "Time", ylab = expression(S[0](t)),
      main = paste("Baseline survival function (ref=", ref_cooker, ")"),
      cex.main = 0.9, # title font size
      cex.lab = 0.8, # axis label font size
      cex.axis = 0.7
    ) # tick label font size
    lines(time_grid, S0_grid, col = "blue", type = "s", lty = 2)
    points(time_grid, S0_grid, col = "red", pch = 16)
    legend("bottomleft",
      legend = c("Empirical", "Approx. Dist", "Interpolated points"),
      col = c("black", "blue", "red"), lty = c(1, 2, NA), pch = c(NA, NA, 16)
    )
    dev.off()
  }


  # Return interpolated results
  list(
    fit = fit,
    beta_vector = coef(fit),
    baseline_surv = baseline_surv,
    full_grid = list(time = time_grid, survival = S0_grid),
    median_survival = median_survival
  )
}
# # Quick Check
# results <- generate_cox_baseline("YamoDudo", n = 1000, plot = T,include_plastic_bag=F)
# results$full_grid$survival
#
#
# results$median_survival
#
# # Extract the Ta_avg coefficient
# tes<-results$beta_vector["Ta_avg"]


#
# generate_cox_baseline("Proto4",n = 1000, plot = T,include_plastic_bag=F)$full_grid$survival
