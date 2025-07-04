# rm(list = ls())
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(simr)
library(nlme)
# Load functions from file
source("New_Cox_simulations.R")
# source("Memory_Management.R")

# #
# #
# # source("New_cox_simulations.R")
# # Load data
# valid_data <- read.csv("data/valid_data_final.csv", header = T)
# SolarData_september <- read.csv("data/SolarData_september_updated.csv")
#
# #  Relevel Cooker
# valid_data$Cooker <- factor(valid_data$Cooker, levels = unique(valid_data$Cooker))
# valid_data$Cooker <- relevel(valid_data$Cooker, ref = "Proto6")
#
# # Fit he lmm model
#
#
#
#


model_ar1 <- lme(
  P_s ~ as.factor(Cooker) + Td + Wind + PlasticBag + MeasureOpeningmm,
  random = ~ 1 | TestDate,
  correlation = corAR1(form = ~  | TestDate),
  data = valid_data,
  method = "REML"
)

model_mixed <- lmer(P_s ~ as.factor(Cooker) + Td + Wind + PlasticBag + MeasureOpeningmm + (1 | TestDate),
  data = valid_data, REML = T
)
#
summary(model_mixed)
#
# # Extract the variance components of the random effects
#
VarCorr(model_mixed)
vc <- VarCorr(model_mixed)
as.data.frame(vc)
#
#
vc_df <- as.data.frame(VarCorr(model_mixed))
sigma_b2 <- vc_df$vcov[vc_df$grp == "TestDate"]
sigma2 <- vc_df$vcov[vc_df$grp == "Residual"]

n_dates <- 30
sigma_b <- sqrt(sigma_b2)
sigma_e <- sqrt(sigma2)
#
random_effects <- rnorm(n_dates, mean = 0, sd = sigma_b)
residuals_sim <- rnorm(n_dates, mean = 0, sd = sigma_e)



vc <- as.data.frame(VarCorr(model_mixed))
sigma_e2 <- vc$vcov[vc$grp == "Residual"]

# Confidence intervals for random effect standard deviations
ci <- confint(model_mixed, method = "profile")
# Upper 95% CI limit for sd_(TestDate)
upper_sd_b <- ci[".sig01", "97.5 %"]

# ICC upper bound from upper variance limit
upper_sigma_b2_from_sd <- upper_sd_b^2
ICC_upper <- upper_sigma_b2_from_sd / (upper_sigma_b2_from_sd + sigma_e2)
message("Upper bound ICC from profile: ", round(ICC_upper, 4))
# Use ICC to compute between-group variance
sigma_b2_from_ICC <- (ICC_upper * sigma_e2) / (1 - ICC_upper)
message("Calculated σ²_b (TestDate) from ICC: ", round(sigma_b2_from_ICC, 4))

# Use this variance for simulation
sigma_b2 <- sigma_b2_from_ICC
sigma_b <- sqrt(sigma_b2)



# Data <- valid_data[valid_data$Cooker== "Proto6", ]



#' @param valid_data        Data frame containing: P_s, covariates in var_list, Cooker, TestDate, H1 timestamp
#' @param var_list          Character vector of covariate names (e.g. c("Td","Wind",...))
#' @param n_grid            Integer vector: candidate sample sizes per cooker
#' @param delta             Numeric: target effect size (mean difference) to detect
#' @param alpha             Significance level (one‐sided), default 0.05
#' @param power_tgt         Desired power (1−β), default 0.8
#' @param n_sim              Number of Monte Carlo simulations per n, default 1000
#' @param sigma             Noise scale for spline covariate sim, default 0.2
#' @param tau               Random‐intercept shift scale for covariate sim, default 0
#' @param ICC               Optional ICC to override between‐date variance, default NULL
#' @param m                 Number of simulated TestDate clusters per run, default length(unique(valid_data$TestDate))
#' @param alternative       "one-sided" or "two-sided", default "one-sided"
#' @param ref_cooker        Character: name of reference cooker in simulation, default "Cooker1"
#' @param seed              Integer: random seed for reproducibility, default NULL
#' @return A list with:
#'   * sample_size: optimal sample size per cooker
#'   * power_curve: data.frame(n, power)
#'   * summary_table: data.frame of input parameters and optimal n
#'   * plot_path: file path to saved power curve PNG
#'
#'    (icc_value <- performance::icc(model_mixed))
# Intraclass Correlation Coefficient

# Adjusted ICC: 0.276
# Unadjusted ICC: 0.098
#

power_sample_size <- function(
    valid_data = NULL,
    n_grid, delta_pct,
    ref_cooker = "Proto6",
    alpha = 0.05,
    power_tgt = 0.8,
    n_sim = 1000,
    sigma = 0.2,
    tau = 0,
    ICC = NULL,
    alternative = c("greater", "less", "two.sided"),
    seed = 123,
    include_plasticbag = FALSE,
    progress_callback = NULL) {
  # match and seed
  alternative <- match.arg(alternative)
  set.seed(seed)

  if (nrow(valid_data) == 0) stop("valid_data is empty")

  # covariates
  var_list <- c("I_avg", "Ta_avg", "Wind", "T1_baseline", "Td")
  if (include_plasticbag) var_list <- c(var_list, "PlasticBag")

  # effect
  delta <- delta_pct #* mean(valid_data$P_s)
  grid_pts <- seq(30, n_grid, by = 2)

  # helper: build formula
  build_fmla <- function(resp, fixed, rand = NULL) {
    f <- paste(c(resp, "~", paste(fixed, collapse = " + ")), collapse = " ")
    if (!is.null(rand)) f <- paste0(f, " + (1|", rand, ")")
    as.formula(f)
  }

  # model settings
  control <- lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5, xtol_rel = 1e-8),
    check.nobs.vs.nlev = "ignore"
  )

  # full model: to extract variance components
  fmla_full <- build_fmla("P_s", c("Cooker", var_list), "TestDate")
  mod_full <- lmerTest::lmer(fmla_full, data = valid_data, REML = TRUE, control = control)

  # variance components
  vc <- as.data.frame(VarCorr(mod_full))
  sigma_e2 <- vc$vcov[vc$grp == "Residual"]
  sigma_b2 <- if (is.null(ICC)) vc$vcov[vc$grp == "TestDate"] else ICC * sigma_e2 / (1 - ICC)
  sigma_e <- sqrt(sigma_e2)
  sigma_b <- sqrt(sigma_b2)
  message("σ²_b (TestDate): ", round(sigma_b2, 4))


  ref_data <- valid_data[valid_data$Cooker == ref_cooker, ]

  # base model: for simulation (Cooker excluded)
  fmla_base <- build_fmla("P_s", var_list, "TestDate")
  base_mod <- lmerTest::lmer(fmla_base, data = ref_data, REML = TRUE, control = control)

  # simulation formula: same structure as full model
  fmla_sim <- fmla_full


  # counters
  total_iters <- length(grid_pts) * n_sim
  iter_count <- 0L
  power_vals <- numeric(length(grid_pts))

  # main double‐loop
  for (j in seq_along(grid_pts)) {
    N <- grid_pts[j]
    rejections <- 0L

    for (i in seq_len(n_sim)) {
      # advance counter and report
      iter_count <- iter_count + 1L
      if (!is.null(progress_callback)) {
        progress_callback(iter_count / total_iters * 100)
      }

      # Generat dataset
      sim_df <- get_spline_df_dup_cookers(
        valid_data, var_list,
        n = N,
        inter_count = iter_count
      )

      # add random effects & noise
      re_vals <- rnorm(length(unique(sim_df$TestDate)), 0, sigma_b)
      names(re_vals) <- unique(sim_df$TestDate)
      sim_df$re <- re_vals[as.character(sim_df$TestDate)]
      sim_df$eps <- rnorm(nrow(sim_df), 0, sigma_e)

      # simulate outcome
      sim_df$pred0 <- predict(base_mod, newdata = sim_df, re.form = NA)

      sim_df$P_s <- sim_df$pred0 *
        ifelse(sim_df$Cooker == "Cooker2", (1 + delta), 1) +
        sim_df$re + sim_df$eps

      #
      # sim_df$P_s <- sim_df$pred0 +
      #   delta * (sim_df$Cooker == "Cooker2") +
      #   sim_df$re + sim_df$eps

      # fit and test
      fit <- tryCatch(
        suppressWarnings(
          lmerTest::lmer(
            fmla_sim,
            data = sim_df, REML = FALSE,
            control = lme4::lmerControl(
              optimizer = "bobyqa",
              optCtrl = list(maxfun = 100000, xtol_rel = 1e-8),
              check.nobs.vs.nlev = "ignore"
            )
          )
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      cf <- summary(fit)$coefficients
      if (!"Pr(>|t|)" %in% colnames(cf)) next

      tval <- cf["CookerCooker2", "Estimate"] / cf["CookerCooker2", "Std. Error"]
      p_raw <- cf["CookerCooker2", "Pr(>|t|)"]
      pval <- switch(alternative,
        greater    = if (tval > 0) p_raw / 2 else 1 - p_raw / 2,
        less       = if (tval < 0) p_raw / 2 else 1 - p_raw / 2,
        two.sided  = p_raw
      )
      if (pval < alpha) rejections <- rejections + 1L
    }

    # record power
    power_vals[j] <- rejections / n_sim
  }

  # assemble results
  summary_table <- data.frame(
    SampleSize = grid_pts,
    Power      = power_vals
  )
  valid_pts <- grid_pts[power_vals >= power_tgt]
  optimal_n <- if (length(valid_pts) > 0) min(valid_pts) else NA_integer_

  # plot
  plastic_lbl <- if (include_plasticbag) "wpb" else "npb"
  plot_path <- file.path("plots", paste0("power_curve_LMM_", plastic_lbl, ".png"))
  png(plot_path, width = 800, height = 500)
  on.exit(dev.off(), add = TRUE)
  plot(
    grid_pts, power_vals,
    type = "o", pch = 16,
    xlab = "Sample Size", ylab = "Power",
    main = paste(
      "LMM Power Curve -",
      if (include_plasticbag) "With Plastic Bag" else "No Plastic Bag"
    ),
    ylim = c(0, 1)
  )
  abline(h = power_tgt, lty = 2, col = "red")
  abline(h = power_tgt + 0.1, lty = 2, col = "red")
  # if (!is.na(optimal_n)) {
  #   abline(v = optimal_n, lty = 2, col = "red")
  #   text(optimal_n, power_tgt + 0.05, bquote(n[opt] == .(optimal_n)))
  # }
  legend("bottomright", legend = c(
    paste("Target Change:", delta_pct * 100),
    paste("Optimal Sample:", optimal_n),
    paste("n_sim:", n_sim),
    paste("α:", alpha),
    paste("Target power:", power_tgt * 100),
    paste("Alternative:", alternative),
    paste("Seed:", seed)
  ), bty = "n", cex = 0.9)

  summary_results <- data.frame(
    Parameter = c(
      "Optimal Sample Size",
      "Target Power",
      "Alpha",
      "Effect (%)",
      "n_sim",
      "Seed"
    ),
    Value = c(
      optimal_n,
      power_tgt,
      alpha,
      delta_pct,
      n_sim,
      seed
    ),
    stringsAsFactors = FALSE
  )

  list(
    power_curve     = summary_table,
    summary_results = summary_results,
    plot_path       = plot_path
  )
}

# Quick Check
n_grid <- 100
delta_pct <- 0.50
valid_data <- valid_data

res <- power_sample_size(
  valid_data = valid_data, n_grid, delta_pct,
  alpha = 0.05, power_tgt = 0.8, n_sim = 10, ref_cooker = "Proto6",
  alternative = "two.sided", include_plasticbag = T, seed = 123
)

print(res$power_curve)
print(res$summary_results)
cat("Plot saved to", res$plot_path, "\n")
