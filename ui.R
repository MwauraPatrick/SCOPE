# UI.R

ui <- fluidPage(
  # Head section
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
        var progress = document.getElementById('sim-progress');
        if (progress) {
          progress.style.width = message.percent + '%';
          progress.setAttribute('aria-valuenow', message.percent);
          progress.textContent = message.percent + '% Complete';
        }
      });
    "))
  ),
  # Title Bar
  div(
    class = "title-bar",
    div(
      class = "title-left",
      tags$a(
        href = "https://www.uhasselt.be/en/instituten-en/dsi/solar-cookers-for-all",
        target = "_blank",
        style = "text-decoration: none; color: inherit;",
        h2("SCOPE Dashboard: Sample Size Planning for Solar Cooker Evaluation")
      )
    ),
    div(
      class = "title-right",
      tags$a(
        href = "https://www.uhasselt.be", target = "_blank",
        img(src = "UHasselt-standaard.png", height = "40px", alt = "UHasselt Logo")
      )
    )
  ),
  # Description Container
  div(
    class = "description-container",
    h4("What is this about?"),
    p("The SCOPE (Solar Cooker Optimization and Performance Evaluation) Dashboard aims to perform sample size calculations and power analysis for solar cooker performance evaluation experiments. The designs are based on Cox Proportional Hazards (Cox PH) and The linear mixed model (LMM). You can specify the desired parameters below and click on run simulation. The bar below should show you the progress of the iterations in percentage so that you know when the simulations finish for you to see the results. You can also download the summary table and the power plot. Also you can print a report that has everything you need including the summary table and the plot and other details after a run is complete.")
  ),

  # Simulation Progress
  div(
    class = "simulation-progress-container",
    h4("Simulation Progress"),
    div(
      class = "progress",
      id = "sim-progress-bar",
      div(
        class = "progress-bar",
        id = "sim-progress",
        role = "progressbar",
        `aria-valuenow` = 0,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        "0% Complete"
      )
    )
  ),

  # Main UI
  fluidRow(
    column(
      2,
      h5("Simulation Inputs"),

      # Analysis Design
      tags$label(
        "Analysis Design",
        `data-bs-toggle` = "tooltip",
        title = "Choose the statistical model to base simulation on"
      ),
      selectInput("Design", NULL,
        choices = c("Time to Event" = "Cox", "Linear Mixed Model" = "LMM"),
        width = "100%"
      ),

      # # Include Plastic Bag
      # div(
      #   class = "form-check",
      #   tags$input(
      #     type = "checkbox",
      #     class = "form-check-input",
      #     id = "plastic_bag",
      #     checked = "checked"
      #   ),
      #   tags$label(
      #     class = "form-check-label",
      #     `for` = "plastic_bag",
      #     `data-bs-toggle` = "tooltip",
      #     title = "Whether to consider plastic bag as a covariate in the model",
      #     "Include Plastic Bag"
      #   )
      # ),

      # Random Seed
      div(
        class = "mb-3",
        tags$label("Random Seed", `for` = "seed", class = "form-label"),
        tags$input(
          id = "seed",
          type = "text",
          class = "form-control form-control-sm",
          value = "123",
          `data-bs-toggle` = "tooltip",
          `data-bs-placement` = "top",
          title = "Set a fixed seed for reproducible results"
        )
      ),

      # Hypothesis Type
      tags$label(
        "Hypothesis Type",
        `data-bs-toggle` = "tooltip",
        title = "Choose the alternative hypothesis for power calculation"
      ),
      selectInput("alternative", NULL,
        choices = c("Greater" = "greater", "Two-sided" = "two.sided", "Less" = "less"),
        width = "100%"
      ),

      # Reference Cooker
      tags$label(
        "Reference Cooker",
        `data-bs-toggle` = "tooltip",
        title = "The baseline cooker to which others are compared"
      ),
      selectInput("ref_cooker", NULL,
        choices = c(
          "Proto6", "Brother", "Fornelia", "OnlyPot",
          "OvenProto1", "OvenProto2", "Proto2", "Proto3",
          "Proto4", "Proto5", "Sk14", "YamoDudo"
        ),
        width = "100%"
      ),

      # Conditional Panel for Cox Model
      conditionalPanel(
        "input.Design == 'Cox'",
        h6("Cox PH Inputs"),
        tags$label("Grid Points", `data-bs-toggle` = "tooltip", title = "Number of sample sizes evaluated in the simulation"),
        numericInput("n_grid", NULL, 150, min = 1, max = 5000, width = "100%"),
        tags$label("Median Time Reduction", `data-bs-toggle` = "tooltip", title = "Expected improvement in median survival time"),
        numericInput("MST", NULL, 50, min = 10, max = 100, width = "100%"),
        tags$label("Significance Level (%)", `data-bs-toggle` = "tooltip", title = "Probability of Type I error (false positive)"),
        numericInput("alpha", NULL, 5, min = 1, max = 10, width = "100%"),
        tags$label("Target Power (%)", `data-bs-toggle` = "tooltip", title = "Desired power to detect a true effect"),
        numericInput("target_power", NULL, 80, min = 50, max = 99, width = "100%"),
        tags$label("Number of Simulations", `data-bs-toggle` = "tooltip", title = "Number of Monte Carlo simulations to run"),
        numericInput("n_sims", NULL, 100, min = 100, max = 10000, width = "100%"),
        actionButton("runSim", "Run Cox PH Simulation", class = "primary-action-btn")
      ),

      # Conditional Panel for LMM
      conditionalPanel(
        "input.Design == 'LMM'",
        h6("LMM Inputs"),
        tags$label("Grid Points", `data-bs-toggle` = "tooltip", title = "Number of sample sizes evaluated in the simulation"),
        numericInput("n_grid_lmm", NULL, 150, min = 30, max = 2000, width = "100%"),
        tags$label("Effect Size (%)", `data-bs-toggle` = "tooltip", title = "Expected relative difference between cookers"),
        numericInput("delta_pct_lmm", NULL, 50, width = "100%"),
        tags$label("Significance Level (%)", `data-bs-toggle` = "tooltip", title = "Probability of Type I error (false positive)"),
        numericInput("alpha_lmm", NULL, 5, width = "100%"),
        tags$label("Target Power (%)", `data-bs-toggle` = "tooltip", title = "Desired probability of detecting a true effect"),
        numericInput("power_tgt_lmm", NULL, 80, width = "100%"),
        tags$label("Number of Simulations", `data-bs-toggle` = "tooltip", title = "Number of Monte Carlo simulations to run"),
        numericInput("n_sim_lmm", NULL, 100, width = "100%"),
        actionButton("runSim_LMM", "Run LMM Simulation", class = "primary-action-btn")
      )
    ),

    # Summary Table Column
    column(
      2,
      h5("Summary Table"),
      tableOutput("summary_table"),
      downloadButton("downloadTable", "Download Table",
        class = "btn btn-success btn-sm custom-btn"
      )
    ),

    # Plot Column
    column(
      8,
      h5("Power Sample Plot"),
      div(
        class = "plot-container",
        style = "height: 700px; position: relative;",
        actionButton("prevPlot", label = NULL, icon = icon("chevron-left"), class = "btn btn-outline-secondary btn-sm plot-nav-btn plot-nav-btn-left"),
        plotOutput("power_plot", height = "100%", width = "100%"),
        actionButton("nextPlot", label = NULL, icon = icon("chevron-right"), class = "btn btn-outline-secondary btn-sm plot-nav-btn plot-nav-btn-right")
      ),
      fluidRow(
        column(
          3,
          downloadButton("downloadPlot", "Download Plot",
            class = "btn btn-info btn-sm btn-block custom-btn"
          )
        ),
        column(
          3,
          downloadButton("Report", "Download Report",
            class = "btn btn-warning btn-sm btn-block custom-btn"
          )
        ),
        column(
          6,
          div(class = "plot-info text-center", textOutput("plotIndex"))
        )
      )
    )
  ),

  # Footer
  div(class = "footer small", p("© 2025 sc4all | Open Rights"))
)
