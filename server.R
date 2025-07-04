# server.R

# #########################
# SOURCE GLOBAL FUNCTIONS
# #########################
source("Global.R")

# server logic
server <- function(input, output, session) {
  # Read data
  rv_data <- reactive(df <- valid_data)

  # Results containers for Cox and LMM models
  sim_results <- reactiveValues(cox = NULL, lmm = NULL)

  # Reactive plot index
  plot_index <- reactiveVal(1)

  # Time-to-Event Simulation (Cox model)
  observeEvent(input$runSim, {
    req(
      input$n_grid, input$MST, input$ref_cooker, input$alpha,
      input$target_power, input$n_sims, input$seed
    )

    progress_cb <- function(pct) {
      session$sendCustomMessage("updateProgressBar", list(percent = round(pct, 2)))
    }

    # Initialize progress
    session$sendCustomMessage("updateProgressBar", list(percent = 0))

    # Run simulation
    res <- Cox_PH_model_simPlot(
      valid_data = rv_data(),
      n_grid = input$n_grid,
      MST = input$MST,
      ref_cooker = input$ref_cooker,
      alpha = input$alpha / 100,
      target_power = input$target_power / 100,
      n_sims = input$n_sims,
      seed = as.numeric(input$seed),
      # include_plastic_bag = input$plastic_bag,
      alternative = input$alternative,
      progress_callback = progress_cb
    )

    # Store Cox simulation result
    sim_results$cox <- res

    # Complete progress bar
    session$sendCustomMessage("updateProgressBar", list(percent = 100))

    # Reset plot index
    plot_index(1)
  })

  # LMM Simulation
  observeEvent(input$runSim_LMM, {
    req(
      input$n_grid_lmm, input$delta_pct_lmm, input$alpha_lmm,
      input$power_tgt_lmm, input$n_sim_lmm
    )

    progress_cb <- function(pct) {
      session$sendCustomMessage("updateProgressBar", list(percent = round(pct, 2)))
    }

    # Initialize progress
    session$sendCustomMessage("updateProgressBar", list(percent = 0))

    # Run simulation
    res <- power_sample_size(
      valid_data = rv_data(),
      ref_cooker = input$ref_cooker,
      n_grid = input$n_grid_lmm,
      delta_pct = as.numeric(input$delta_pct_lmm / 100),
      alpha = input$alpha_lmm / 100,
      power_tgt = as.numeric(input$power_tgt_lmm / 100),
      n_sim = input$n_sim_lmm,
      alternative = input$alternative,
      # include_plasticbag = input$plastic_bag,
      progress_callback = progress_cb,
      seed = as.numeric(input$seed)
    )

    # Store LMM simulation result
    sim_results$lmm <- res

    # Complete progress bar
    session$sendCustomMessage("updateProgressBar", list(percent = 100))

    # Reset plot index
    plot_index(1)
  })

  # Helper to choose the current model result
  current_sim_result <- reactive({
    if (input$Design == "LMM") {
      sim_results$lmm
    } else {
      sim_results$cox
    }
  })

  # Plot Navigation
  observeEvent(input$nextPlot, {
    req(current_sim_result())
    plot_paths <- c(current_sim_result()$plot_path, current_sim_result()$survival_plot_path)

    current_index <- plot_index()
    new_index <- if (current_index < length(plot_paths)) current_index + 1 else 1
    plot_index(new_index)
  })

  observeEvent(input$prevPlot, {
    req(current_sim_result())
    plot_paths <- c(current_sim_result()$plot_path, current_sim_result()$survival_plot_path)

    current_index <- plot_index()
    new_index <- if (current_index > 1) current_index - 1 else length(plot_paths)
    plot_index(new_index)
  })

  # Render Plot
  output$power_plot <- renderPlot({
    req(current_sim_result())
    plot_paths <- c(current_sim_result()$plot_path, current_sim_result()$survival_plot_path)

    if (length(plot_paths) > 0 && file.exists(plot_paths[plot_index()])) {
      img <- readPNG(plot_paths[plot_index()])
      par(mar = c(0, 0, 0, 0))
      plot(c(0, 1), c(0, 1),
        type = "n",
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n",
        frame.plot = FALSE
      )
      rasterImage(img, 0, 0, 1, 1, interpolate = FALSE)
    } else {
      plot(0, 0,
        type = "n",
        main = "No Plot Available",
        xlab = "", ylab = ""
      )
    }
  })

  # Render Summary Table
  output$summary_table <- renderTable({
    req(current_sim_result())
    current_sim_result()$summary_results
  })

  # Plot Index Display
  output$plotIndex <- renderText({
    req(current_sim_result())
    plot_paths <- c(current_sim_result()$plot_path, current_sim_result()$survival_plot_path)

    if (length(plot_paths) > 1) {
      paste0("Plot ", plot_index(), " of ", length(plot_paths))
    } else {
      "Single Plot"
    }
  })

  # Download Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$Design, "_plot.png")
    },
    content = function(file) {
      req(current_sim_result())
      plot_paths <- c(current_sim_result()$plot_path, current_sim_result()$survival_plot_path)

      if (length(plot_paths) > 0 && file.exists(plot_paths[plot_index()])) {
        file.copy(plot_paths[plot_index()], file)
      } else {
        png(file)
        plot(0, 0, type = "n", main = "No plot available")
        dev.off()
      }
    }
  )

  # Download Summary Table
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$Design, "_summary.csv")
    },
    content = function(file) {
      req(current_sim_result())
      write.csv(current_sim_result()$summary_results, file, row.names = FALSE)
    }
  )

  # Report Generation
  output$Report <- downloadHandler(
    filename = function() {
      paste0(input$Design, "_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(current_sim_result())

      # Generate report
      generate_report(
        sim_result = current_sim_result(),
        design_type = input$Design,
        output_file = file
      )
    }
  )
}
