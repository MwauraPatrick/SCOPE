###################################################
########################### LMM####################
#################################################

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
  delta <- delta_pct
  grid_pts <- seq(30, n_grid, by = 2)

  # helper: build formula
  build_fmla <- function(resp, fixed, rand = NULL) {
    f <- paste(c(resp, "~", paste(fixed, collapse = " + ")), collapse = " ")
    if (!is.null(rand)) f <- paste0(f, " + (1|", rand, ")")
    as.formula(f)
  }

  # model settings
  # control <- lme4::lmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 1e5, xtol_rel = 1e-8),
  #   check.nobs.vs.nlev = "ignore"
  # )
  control <- lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e4)
  )

  # full model: to extract variance components
  fmla_full <- build_fmla("P_s", c("Cooker", var_list), "TestDate")
  mod_full <- lmerTest::lmer(fmla_full, data = valid_data, REML = TRUE, control = control)
  # Residual variance
  vc <- as.data.frame(VarCorr(mod_full))
  sigma_e2 <- vc$vcov[vc$grp == "Residual"]
  sigma_e <- sqrt(sigma_e2)

  # Confidence intervals for random effect standard deviations
  ci <- confint(mod_full, method = "profile")
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


  # # variance components
  # vc <- as.data.frame(VarCorr(mod_full))
  # sigma_e2 <- vc$vcov[vc$grp == "Residual"]
  # sigma_b2 <- if (is.null(ICC)) vc$vcov[vc$grp == "TestDate"] else ICC * sigma_e2 / (1 - ICC)
  # sigma_e <- sqrt(sigma_e2)
  # sigma_b <- sqrt(sigma_b2)
  # message("σ²_b (TestDate): ", round(sigma_b2, 4))

  # Reference data for the reference cooker
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
            control = control
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
      "LMM Power Curve" # ,
      # if (include_plasticbag) "With Plastic Bag" else "No Plastic Bag"
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
    paste("Optimal Sample size:", optimal_n),
    paste("Reference Cooker:", ref_cooker),
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


###################################################
####################### CoxPH#######################
###################################################

Cox_PH_model_simPlot <- function(valid_data = NULL, n_grid, ref_cooker,
                                 MST,
                                 alpha = 0.05,
                                 target_power = 0.80,
                                 n_sims = 1,
                                 seed = 123,
                                 T_max = 120,
                                 include_plastic_bag = F,
                                 alternative = c("two.sided", "less", "greater"),
                                 progress_callback = NULL) {
  # Hypothesis
  alternative <- match.arg(alternative)

  n_grid <- seq(30, n_grid, 5)
  set.seed(seed)
  var_list <- c("Ta_avg", "I_avg", "Wind", "T1_baseline", "Td")

  # Initialize the power matrix
  power_vec <- numeric(length(n_grid))
  total_sims <- length(n_grid) * n_sims
  completed_sims <- 0
  # sim_data_storage <- list()
  best_diff <- Inf
  optimal_sim_data <- NULL
  best_sample_size <- NA

  # Loop
  for (j in seq_along(n_grid)) {
    p_values <- numeric(n_sims)
    N <- n_grid[j]
    total_n <- N

    # Generate Baselines
    baseline_result <- generate_cox_baseline(valid_data,
      n = total_n, ref_cooker = ref_cooker,
      include_plastic_bag = include_plastic_bag,
      completed_sims = completed_sims
    )

    # Calculate Median Survival Time (MST) for reference cooker and new
    CT_MST <- baseline_result$median_survival
    TR_MST <- CT_MST - MST

    # Hazard Ratio Calculation
    HR <- exp(log(CT_MST) - log(TR_MST))
    # Log Hazard Ratio (Beta)
    beta_Cooker <- log(HR)
    survival_func_0 <- baseline_result$full_grid$survival


    # Shared coefficients
    beta_vector <- c(
      beta_Cooker,
      beta_ambient_temp = as.numeric(baseline_result$beta_vector["Ta_avg"]),
      beta_irradiation = as.numeric(baseline_result$beta_vector["I_avg"]),
      beta_baseline_temp = as.numeric(baseline_result$beta_vector["T1_baseline"]),
      beta_wind = as.numeric(baseline_result$beta_vector["Wind"]),
      beta_measureopening = as.numeric(baseline_result$beta_vector["MeasureOpeningmm"])
    )

    # Add PlasticBag and interaction terms if applicable
    if (include_plastic_bag) {
      beta_vector <- c(
        beta_vector,
        beta_plastic_bags = as.numeric(baseline_result$beta_vector["as.factor(PlasticBag)1"]),
        beta_interaction_Proto6 = as.numeric(baseline_result$beta_vector["as.factor(Cooker)Proto4:as.factor(PlasticBag)1"])
      )
    }

    for (i in 1:n_sims) {
      interp_df <- get_spline_df_dup_cookers(valid_data, var_list = var_list, completed_sims = completed_sims, n = total_n)
      Cooker <- interp_df$Cooker
      Cooker_indicator <- interp_df$Cooker_indicator
      Ta_avg <- interp_df$Ta_avg
      I_avg <- interp_df$I_avg
      T1_baseline <- interp_df$T1_baseline
      Wind <- interp_df$Wind
      PlasticBag <- as.numeric(interp_df$PlasticBag)
      MeasureOpeningmm <- as.numeric(interp_df$MeasureOpeningmm)
      Interaction_ProtoX <- Cooker_indicator * PlasticBag

      # Base matrix and column names
      Z_i <- cbind(
        Cooker_indicator,
        Ta_avg,
        I_avg,
        T1_baseline,
        Wind,
        MeasureOpeningmm
      )
      col_names <- c("Cooker_indicator", "Ta_avg", "I_avg", "T1_baseline", "Wind", "MeasureOpeningmm")

      # Add PlasticBag and interaction if applicable
      if (include_plastic_bag) {
        Interaction_ProtoX <- Cooker_indicator * PlasticBag
        Z_i <- cbind(Z_i, PlasticBag, Interaction_ProtoX)
        col_names <- c(col_names, "PlasticBag", "Interaction_ProtoX")
      }

      # Assign column names
      colnames(Z_i) <- col_names


      cat("Z_i dimensions:", dim(Z_i), "\n")
      cat("beta_vector length:", length(beta_vector), "\n")

      # LP
      eta <- as.numeric(Z_i %*% beta_vector) # LP
      # eta <- eta - mean(eta)  # Center η
      # eta <- scale(eta) * 0.5  # shrink variation if needed

      # baseline survival function S0(t) on the grid
      S0_vec <- survival_func_0 # vector of baseline survival values
      t_grid <- baseline_result$full_grid$time # baseline_result$time  # corresponding time grid

      # quick checks
      cat(
        "length(S0_vec) =", length(S0_vec),
        "  anyNA(S0_vec) =", anyNA(S0_vec), "\n"
      )
      cat(
        "length(t_grid) =", length(t_grid),
        "  anyNA(t_grid) =", anyNA(t_grid), "\n"
      )

      # Survival times

      time_data <- simulate_survival_times(eta, S0_vec, t_grid)

      time <- time_data$time
      status <- time_data$status

      # Base data
      sim_data <- data.frame(
        time = time,
        status = status,
        Cooker = factor(Cooker),
        T1_baseline,
        Ta_avg,
        I_avg,
        Wind,
        MeasureOpeningmm
      )

      # Add PlasticBag column if required
      if (include_plastic_bag) {
        sim_data$PlasticBag <- factor(PlasticBag)
      }

      # Construct formula based on inclusion of PlasticBag
      formula_parts <- c("as.factor(Cooker)", "T1_baseline", "Ta_avg", "I_avg", "Wind", "MeasureOpeningmm")

      if (include_plastic_bag) {
        formula_parts <- c(
          formula_parts,
          "as.factor(PlasticBag)",
          "as.factor(PlasticBag):as.factor(Cooker)"
        )
      }

      cox_formula <- as.formula(paste("Surv(time, status, type = 'right') ~", paste(formula_parts, collapse = " + ")))

      # Fit model
      fit_result <- try(suppressWarnings(coxph(cox_formula, ties = "efron", data = sim_data)), silent = FALSE)


      if (!inherits(fit_result, "try-error") && all(is.finite(diag(fit_result$var)))) {
        coef_summary <- summary(fit_result)$coefficients
        coef_names <- rownames(coef_summary)

        if ("as.factor(Cooker)Cooker2" %in% coef_names) {
          p_val <- coef_summary["as.factor(Cooker)Cooker2", "Pr(>|z|)"]
          zval <- coef_summary["as.factor(Cooker)Cooker2", "z"]

          # Inline function definition
          p_values[i] <- (function(p_val, zval, alternative) {
            if (is.na(zval) || is.na(p_val)) {
              return(NA)
            }

            switch(alternative,
              "greater" = ifelse(zval > 0, p_val / 2, 1 - p_val / 2),
              "less" = ifelse(zval < 0, p_val / 2, 1 - p_val / 2),
              "two.sided" = p_val,
              {
                warning("Invalid 'alternative' specified. Choose from 'greater', 'less', or 'two.sided'.")
                NA
              }
            )
          })(p_val, zval, alternative)
        } else {
          p_values[i] <- NA
        }
      } else {
        p_values[i] <- NA
      }

      # update counters
      completed_sims <- completed_sims + 1

      # report progress
      if (!is.null(progress_callback)) {
        pct <- completed_sims / total_sims * 100
        progress_callback(pct)
      }
    }

    p_values <- p_values[!is.na(p_values)]

    if (length(p_values) > 0) {
      power_vec[j] <- mean(p_values < alpha)
    } else {
      power_vec[j] <- NA
    }

    power_vec[j] <- mean(p_values < alpha, na.rm = TRUE)
    this_power <- power_vec[j]
    diff <- abs(this_power - target_power)
    if (!is.na(diff) && diff < best_diff) {
      best_diff <- diff
      optimal_sim_data <- sim_data
      best_sample_size <- N
    }
  }

  summary_table <- data.frame(SampleSizePerGroup = n_grid, Power = power_vec)
  n_target <- min(summary_table$SampleSizePerGroup[summary_table$Power >= target_power], na.rm = TRUE)


  summary_results <- data.frame(
    Parameter = c("Reference Cooker", "Target Power", "Alpha", "Target MS.Time", "Harzad Ratio", "Optimal Sample Size", "Simulation", "seed"),
    Value = c(ref_cooker, target_power * 100, alpha * 100, MST, round(HR, 2), n_target, n_sims, seed)
  )

  # Save plot
  pb_tag <- if (include_plastic_bag) "withPB" else "noPB"
  plot_filename <- paste0("plots/power_curve_", pb_tag, ".png")


  png(filename = plot_filename, width = 8, height = 5, units = "in", res = 300)

  plot_title <- if (include_plastic_bag) {
    "Power Curve (Cox PH) " # – Including Plastic Bag
  } else {
    "Power Curve (Cox PH)" # – No Plastic Bag
  }

  plot(n_grid, power_vec,
    type = "o", pch = 16, col = "blue",
    xlab = "Experiments per cooker", ylab = "Estimated Power",
    main = plot_title,
    cex.main = 1.2, cex.lab = 1.1, cex.axis = 0.9
  )
  abline(h = target_power, col = "red", lty = 2)
  abline(h = target_power + 0.1, col = "red", lty = 2)
  # abline(v = n_target, col = "red", lty = 2)

  # # annotation n
  # text(n_target + 1, target_power + 0.015,
  #   labels = bquote(n[opt] == .(n_target)),
  #   col = "red", cex = 0.95
  # )

  legend("bottomright", legend = c(
    paste("MST:", MST),
    paste("Minimum Sample size:", n_target),
    paste("n_sim:", n_sims),
    paste("α:", alpha),
    paste("Target power:", target_power),
    paste("PlasticBag:", include_plastic_bag),
    paste("Alternative:", alternative)
  ), bty = "n", cex = 0.9)

  plot_obj <- recordPlot()
  dev.off()

  # Optimal sample
  select_optimal_sample <- function(summary_table, target_power = 0.8) {
    power_meeting_target <- summary_table$Power >= target_power

    if (any(power_meeting_target)) {
      n_target <- min(summary_table$SampleSizePerGroup[power_meeting_target])
      return(n_target)
    }

    # sample size with highest power
    max_power_index <- which.max(summary_table$Power)
    optimal_sample <- summary_table$SampleSizePerGroup[max_power_index]

    return(optimal_sample)
  }
  # Optimal sample size
  n_optimal <- select_optimal_sample(
    summary_table,
    target_power
  )

  # Optimal simulation data
  # optimal_sim_data <- sim_data_storage[[as.character(n_optimal)]]$survival_data
  # Survival plot
  survival_plot_result <- generate_survival_plot(
    sim_data = optimal_sim_data,
    n_target = n_optimal,
    HR = round(HR, 2),
    ref_cooker = ref_cooker,
    include_plastic_bag = include_plastic_bag,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    n_sims = n_sims
  )

  # Return results
  return(list(
    n_grid = n_grid,
    power_vec = power_vec,
    summary_table = summary_table,
    summary_results = summary_results,
    n_target = n_optimal,
    plot_path = plot_filename,
    sim_data_optimal = optimal_sim_data,
    survival_plot_path = survival_plot_result$plot_path,
    survival_plot_p_value = survival_plot_result$p_value
  ))
}
