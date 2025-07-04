#############################################################################
################## Generate Environmental data and other variables###########
#############################################################################


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



##########################################################################
################ Baseline survival function ###############################
##########################################################################

generate_cox_baseline <- function(
    valid_data = NULL, ref_cooker = NULL,
    n = 100,
    seed = 123,
    plot = FALSE,
    include_plastic_bag = TRUE, completed_sims = NULL) {
  # seed setting
  if (!is.null(completed_sims)) {
    set.seed(as.integer(seed + (completed_sims)))
  } else {
    set.seed(seed)
  }
  # Re level to reference cooker
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



##########################################################################
## Test if the generated data conforms to the original referenced data#####
##########################################################################

test_distribution_similarity <- function(results, var_list = NULL) {
  valid_data <- get("valid_data", envir = .GlobalEnv)
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



############################################################################
######################## Survival Plot#######################################
############################################################################

generate_survival_plot <- function(
    sim_data,
    n_target,
    HR,
    ref_cooker,
    include_plastic_bag = FALSE,
    alternative = "two.sided",
    alpha = 0.05,
    target_power = 0.80,
    n_sims = 1000) {
  # Tag and output filename
  pb_tag <- if (include_plastic_bag) "withPB" else "noPB"
  plot_filename <- paste0("plots/survival_curve_", pb_tag, ".png")

  # Survival object and model
  surv_obj <- Surv(sim_data$time, sim_data$status)
  surv_fit <- survfit(surv_obj ~ Cooker, data = sim_data)
  log_rank_test <- survdiff(surv_obj ~ Cooker, data = sim_data)
  p_value <- pchisq(log_rank_test$chisq, df = length(log_rank_test$n) - 1, lower.tail = FALSE)

  # Plot title
  plot_title <- if (include_plastic_bag) {
    "Survival Curve (Cox PH)" # – Including Plastic Bag
  } else {
    "Survival Curve (Cox PH)" # – No Plastic Bag
  }

  # Generate survival plot with risk table
  p <- survfit2(Surv(time, status) ~ Cooker, data = sim_data) |>
    ggsurvfit() +
    add_risktable() +
    labs(
      title = plot_title,
      x = "Time",
      y = "Survival Probability",
      color = "Cooker"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    ) +
    scale_color_manual(values = c("blue", "red")) +
    labs(caption = paste(
      "n optimal:", n_target,
      "| HR:", HR,
      "| n_sim:", n_sims,
      "| α:", alpha,
      "| Power:", target_power,
      "| Plastic Bag:", include_plastic_bag,
      "| Alternative:", alternative,
      "| Log-rank p:", sprintf("%.4f", p_value)
    ))

  # Save the plot
  ggsave(filename = plot_filename, plot = p, width = 8, height = 6, dpi = 300)

  return(list(
    plot_path = plot_filename,
    p_value = p_value
  ))
}

########################################################################
################# Survival times #######################################
#######################################################################


simulate_survival_times <- function(eta, S0_vec, t_grid, max_time = 120) {
  U <- runif(length(eta))
  logS0 <- log(S0_vec)
  simulate_single_time <- function(i) {
    logSi <- exp(eta[i]) * logS0
    S_i_vec <- exp(logSi)
    idx <- which(S_i_vec <= U[i])[1]
    if (is.na(idx)) Inf else t_grid[idx]
  }
  T_true <- vapply(seq_along(eta), simulate_single_time, numeric(1))
  data.frame(
    time = pmin(T_true, max_time),
    status = as.integer(T_true <= max_time)
  )
}
