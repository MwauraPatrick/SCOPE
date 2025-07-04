# clear workspace
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load functions from file
source("New_Cox_simulations.R")
valid_data <- read.csv("data/valid_data_final.csv", header = TRUE)
# source("Memory_Management.R")

# Load required packages
library(tidyverse)
library(lubridate)
library(survival)
library(survival)
library(survminer)
library(graphics)
library(ggplot2)
library(survival)
library(ggsurvfit)

#
# > HR=1.1
# > alpha=0.05
# > target_power=0.8
# > set.seed(123)
# > T_max=120
# > n_grid=30


########################################################################################################
# # survival plot
# generate_survival_plot <- function(
#     sim_data,
#     n_target,
#     HR,
#     ref_cooker,
#     include_plastic_bag = FALSE,
#     alternative = "two.sided",
#     alpha = 0.05,
#     target_power = 0.80,
#     n_sims = 1000
# ) {
#
#   # filename
#   pb_tag <- if (include_plastic_bag) "withPB" else "noPB"
#   plot_filename <- paste0("plots/survival_curve_", pb_tag,".png")
#
#   # log-rank test
#   surv_fit <- survfit(Surv(time, status) ~ Cooker, data = sim_data)
#   log_rank_test <- survdiff(Surv(time, status) ~ Cooker, data = sim_data)
#   p_value <- pchisq(log_rank_test$chisq, length(log_rank_test$n) - 1, lower.tail = FALSE)
#
#   # Open PNG device
#   png(filename = plot_filename, width = 8, height = 5, units = "in", res = 300)
#
#   # Plot title
#   plot_title <- if (include_plastic_bag) {
#     "Survival Curve (Cox PH) – Including Plastic Bag"
#   } else {
#     "Survival Curve (Cox PH) – No Plastic Bag"
#   }
#
#   # Plot survival curves
#   plot(surv_fit,
#        col = c("blue", "red"),
#        lwd = 2,
#        mark.time = TRUE,
#        xlab = "Time",
#        ylab = "Survival Probability",
#        main = plot_title,
#        cex.main = 1.2,
#        cex.lab = 1.1,
#        cex.axis = 0.9)
#
#   # Add legend
#   legend("topright",
#          legend = levels(sim_data$Cooker),
#          col = c("blue", "red"),
#          lwd = 2)
#   # Additional legend
#   legend("bottomleft",
#          legend = c(
#            paste("n Optimal:", n_target),
#            paste("Hazard Ratio:", HR),
#            paste("n_sim:", n_sims),
#            paste("α:", alpha),
#            paste("Target Power:", target_power),
#            paste("Plastic Bag:", include_plastic_bag),
#            paste("Alternative:", alternative),
#            paste("Log-rank p-value:", sprintf("%.4f", p_value))
#          ),
#          bty = "n",
#          cex = 0.9)
#   dev.off()
#
#   return(list(
#     plot_path = plot_filename,
#     p_value = p_value
#   ))
# }

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
  # Required packages
  library(survival)
  library(ggplot2)
  library(ggsurvfit)

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
    "Survival Curve (Cox PH) – Including Plastic Bag"
  } else {
    "Survival Curve (Cox PH) – No Plastic Bag"
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
      "n Optimal:", n_target,
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

############################################################################################


##########################################################################################

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

  n_grid <- seq(5, n_grid, 5)
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
    if (diff < best_diff) {
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
    "Power Curve (Cox PH) – Including Plastic Bag"
  } else {
    "Power Curve (Cox PH) – No Plastic Bag"
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



# Quick Test
result <- Cox_PH_model_simPlot(valid_data, n_grid = 100, ref_cooker = "Proto6", MST = 50, alpha = 0.05, target_power = 0.8, n_sims = 10, seed = 123, include_plastic_bag = F, alternative = "greater")


library(microbenchmark)
#
# Time to ex-cute check
# microbenchmark(
#   result <- Cox_PH_model_simPlot(30,ref_cooker= "Proto6", HR = 1.1, alpha = 0.05,target_power = 0.8, n_sims = 100, seed = 123,include_plastic_bag = F,alternative = "greater"),
#   times = 10
# )
#
print(result$summary_results)
print(result$summary_table)
# Print the power plot
print(result$plot)

# # For shiny
# replayPlot(result$plot)

#
