library(DiagrammeR)
library(htmltools)
library(DiagrammeRsvg)
library(rsvg)
library(grid)
library(png)

# Ensure plots directory exists
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

# Function to safely export diagrams
export_diagram <- function(diagram, filename, width = 2400, height = 1600) {
  tryCatch(
    {
      # Create SVG
      svg_file <- file.path("plots", paste0(filename, ".svg"))
      export_svg(diagram) %>%
        writeLines(svg_file)

      # Create PNG
      png_file <- file.path("plots", paste0(filename, ".png"))
      rsvg_png(svg_file, png_file, width = width, height = height)

      # High-resolution PNG with safer grid method
      high_res_png <- file.path("plots", paste0(filename, "_high_res.png"))
      png(filename = high_res_png, width = 3000, height = 1800, res = 300)

      # Use grid.newpage() and viewport for safer rendering
      grid.newpage()
      pushViewport(viewport(width = 1, height = 1))

      # Read and display PNG
      img <- readPNG(png_file)
      grid.raster(img)

      dev.off()

      message(sprintf("Diagram %s exported successfully!", filename))

      return(list(
        svg = svg_file,
        png = png_file,
        high_res_png = high_res_png
      ))
    },
    error = function(e) {
      message("Error exporting diagram: ", e$message)
      return(NULL)
    }
  )
}



# Original Detailed Shiny Architecture Diagram
shiny_architecture_diagram <- grViz("
digraph ShinyArchitecture {
    // Global Graph Attributes
    graph [
        layout = dot,
        rankdir = TB,
        splines = ortho,
        nodesep = 0.8,
        ranksep = 0.8,
        fontname = 'Arial',
        bgcolor = 'white'
    ]

    // Node Attributes
    node [
        shape = box,
        style = filled,
        fontname = 'Arial Bold',
        fontsize = 10,
        color = black,
        fontcolor = black
    ]

    // Edge Attributes
    edge [
        color = '#666666',
        arrowhead = vee,
        arrowsize = 0.7
    ]

    // Client Layer
    subgraph cluster_client {
        label = 'Client Layer: User Interface';
        style = rounded;
        bgcolor = '#F0E6FF';

        UI [label='UI Components\n• Responsive Design\n• Interactive Elements\n• Real-time Feedback',
            fillcolor='#FF99CC']
        Inputs [label='Input Handlers\n• Action Buttons\n• Dropdown Menus\n• Text Inputs',
                fillcolor='#99CCFF']
        Outputs [label='Output Renderers\n• Dynamic Plots\n• Interactive Tables\n• Statistical Visualizations',
                 fillcolor='#99FFCC']
    }

    // Server Layer
    subgraph cluster_server {
        label = 'Server Layer: Application Logic';
        style = rounded;
        bgcolor = '#E6F2FF';

        GlobalConfig [label='global.R\n• Package Imports\n• Configuration\n• Constant Definitions',
                     fillcolor='#FFD700']
        UILogic [label='ui.R\n• Layout Design\n• Responsive Components\n• Theme Management',
                 fillcolor='#98FB98']
        ServerLogic [label='server.R\n• Core Processing\n• Reactive Computations\n• Error Handling',
                     fillcolor='#FFFF99']

        // State Management
        subgraph cluster_state {
            label = 'State Management';
            style = rounded;
            bgcolor = '#E0FFFF';

            ReactiveValues [label='Reactive Values\n• Dynamic Data Storage\n• State Tracking\n• Synchronization',
                            fillcolor='#87CEFA']
            DataProcessing [label='Data & Simulation\n• Statistical Modeling\n• Computational Engine\n• Algorithmic Transformations',
                            fillcolor='#DDA0DD']
        }
    }

    // Backend Services
    subgraph cluster_backend {
        label = 'Backend Services';
        style = rounded;
        bgcolor = '#F0FFF0';

        Database [label='Data Sources\n• SQL Databases\n• CSV/Excel Files\n• External APIs',
                  fillcolor='#90EE90']
        ComputeEngine [label='Compute Resources\n• Parallel Processing\n• Distributed Computing\n• GPU Acceleration',
                       fillcolor='#F0E68C']
    }

    // Connections
    Client [label='User Browser', shape=ellipse, fillcolor='#E0E0E0']
    Server [label='Shiny Server', shape=ellipse, fillcolor='#E0E0E0']

    Client -> Server [label='HTTP/WebSocket', style=dashed]
    Server -> Client [label='HTTP/WebSocket', style=dashed]

    UI -> Inputs
    Inputs -> ServerLogic [label='User Interactions']
    ServerLogic -> ReactiveValues [label='Process Inputs']
    ReactiveValues -> DataProcessing [label='Data Flow']
    DataProcessing -> Database [label='Optional Integration']
    DataProcessing -> ComputeEngine [label='Compute Tasks']
    ServerLogic -> Outputs [label='Generate Outputs']
    Outputs -> Client

    GlobalConfig -> ServerLogic
    UILogic -> Server
}
")

# Shiny Project Structure Diagram
shiny_project_structure <- grViz("
digraph ShinyAppStructure {
    // Global Graph Attributes
    graph [
        layout = dot,
        rankdir = TB,
        splines = ortho,
        nodesep = 0.8,
        ranksep = 0.8,
        fontname = 'Helvetica',
        bgcolor = 'white'
    ]

    // Node Attributes
    node [
        shape = folder,
        style = filled,
        fontname = 'Helvetica Bold',
        fontsize = 12,
        color = black,
        fontcolor = black
    ]

    // Project Root
    root [label='ShinyApp/', fillcolor='#E6F3FF']

    // Directories
    data [label='data/\nStatic datasets', fillcolor='#87CEEB']
    logs [label='logs/\nError tracking', fillcolor='#20B2AA']
    plots [label='plots/\nGenerated visualizations', fillcolor='#4682B4']
    reports [label='reports/\nDynamic PDF reports', fillcolor='#48D1CC']
    rsconnect [label='rsconnect/\nDeployment configs', fillcolor='#7B68EE']
    www [label='www/\nWeb assets', fillcolor='#BA55D3']

    // R Scripts
    ui [label='ui.R\nUser Interface', shape=note, fillcolor='#FFD700']
    server [label='server.R\nServer Logic', shape=note, fillcolor='#98FB98']
    global [label='global.R\nShared Resources', shape=note, fillcolor='#FF6347']
    helpers [label='helpers.R\nUtility Functions', shape=note, fillcolor='#DDA0DD']
    simulation [label='simulation.R\nComputation Engine', shape=note, fillcolor='#00CED1']
    app [label='app.R\nMain Entry Point', shape=note, fillcolor='#32CD32']

    // Connections
    root -> {data logs plots reports rsconnect www ui server global helpers simulation app}

    // Relationships
    app -> {ui server global}
    server -> {helpers simulation global}
    ui -> www

    // Styling for connections
    edge [color='#666666', style=dashed]
}
")

# Modify export call to be more robust
arch_export <- try(export_diagram(shiny_architecture_diagram, "shiny_architecture_diagram"))
struct_export <- try(export_diagram(shiny_project_structure, "shiny_project_structure"))

# Safer display method
if (!inherits(arch_export, "try-error") && !is.null(arch_export)) {
  print(htmltools::browsable(shiny_architecture_diagram))
}

if (!inherits(struct_export, "try-error") && !is.null(struct_export)) {
  print(htmltools::browsable(shiny_project_structure))
}


# Modified export function with trimming
export_diagram_trimmed <- function(diagram, filename, width = 2400, height = 1600) {
  tryCatch(
    {
      # Create SVG
      svg_file <- file.path("plots", paste0(filename, ".svg"))
      export_svg(diagram) %>%
        writeLines(svg_file)

      # Create PNG
      png_file <- file.path("plots", paste0(filename, ".png"))
      rsvg_png(svg_file, png_file, width = width, height = height)

      # Trim white spaces
      img <- image_read(png_file)
      trimmed_img <- image_trim(img)

      # Save trimmed image
      trimmed_png_file <- file.path("plots", paste0(filename, "_trimmed.png"))
      image_write(trimmed_img, trimmed_png_file)

      message(sprintf("Diagram %s exported and trimmed successfully!", filename))

      return(list(
        svg = svg_file,
        original_png = png_file,
        trimmed_png = trimmed_png_file
      ))
    },
    error = function(e) {
      message("Error exporting and trimming diagram: ", e$message)
      return(NULL)
    }
  )
}

# Use the new trimmed export function
struct_export <- try(export_diagram_trimmed(shiny_project_structure, "shiny_project_structure"))

##############################################

# Define the layout
graph <- grViz("
digraph ui_layout {
  graph [layout = dot, rankdir = TB]
  node [shape = box, style = filled, fontname = Helvetica, fillcolor = lightgray]

  title    [label = 'Title Bar\\n(Left: Title, Right: Logo)']
  desc     [label = 'Description Section\\n- Purpose\\n- Usage\\n- Features']
  progress [label = 'Simulation Progress\\n[Progress Bar]']

  col1 [label = 'Inputs \\n- Design\\n- Seed\\n- Plastic\\n- Hypothesis\\n- Inputs', fillcolor = lightblue]
  col2 [label = 'Main Results \\n- Summary Table\\n- Download (button)', fillcolor = lightyellow]
  col3 [label = 'Plots and Result \\n- Power Plot\\n- Plots Navigation\\n- Downloads(Plots & Report)', fillcolor = lightblue]

  footer [label = 'Footer\\n© 2025 sc4all', fillcolor = gray90]

  title -> desc -> progress
  progress -> col1
  progress -> col2
  progress -> col3
  col1 -> footer
  col2 -> footer
  col3 -> footer
}
")

# Save PNG
tmpfile <- "plots/ui_layout_sketch.png"
export_svg(graph) |>
  charToRaw() |>
  rsvg_png(file = tmpfile)

# Preview
img <- readPNG(tmpfile)
grid.newpage()
grid.raster(img)


# Load required libraries
library(DiagrammeR)
library(htmltools)
library(DiagrammeRsvg)
library(rsvg)
library(grid)
library(png)
library(magick)

# Create the directory if it doesn't exist
plots_dir <- "plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Define the layout graph
graph <- grViz("
digraph ui_layout {
  graph [layout = dot, rankdir = TB, bgcolor = 'white']

  node [shape = box, style = filled, fontname = 'Helvetica', fontsize = 12, color = '#48D1CC']

  graph_title [label = 'UI Layout for Solar Cooker Sample Size Application', shape = none, fontsize = 16, fontcolor = '#007bff']

  title    [label = 'Title Bar\\n(Left: Title, Right: Logo)', fillcolor = '#007bff', fontcolor = 'white']
  desc     [label = 'Description Section\\n- Purpose\\n- Usage\\n- Features', fillcolor = '#f8f9fa', fontcolor = '#333333']
  progress [label = 'Simulation Progress\\n[Progress Bar]', fillcolor = '#007bff', fontcolor = 'white']

  col1     [label = 'Inputs\\n- Design\\n- Parameters\\n- Seed\\n- Plastic Bag\\n- Hypothesis', fillcolor = '#f8f9fa', fontcolor = '#333333']
  col2     [label = 'Main Results\\n- Summary Table\\n- Download (button)', fillcolor = '#f8f9fa', fontcolor = '#333333']
  col3     [label = 'Plots and Result\\n- Power Plot\\n- Plots Navigation\\n- Downloads (Plots & Report)', fillcolor = '#f8f9fa', fontcolor = '#333333']

  footer   [label = 'Footer\\n© 2025 sc4all', fillcolor = '#007bff', fontcolor = 'white']

  graph_title -> title
  title -> desc -> progress
  progress -> {col1 col2 col3}
  {col1 col2 col3} -> footer
}
")

# Export SVG then convert to high-res PNG
svg_raw <- export_svg(graph) |> charToRaw()
png_file <- file.path(plots_dir, "ui_layout_sketch_hd.png")

# Render high-resolution PNG (e.g., 3x default DPI)
rsvg_png(svg_raw, file = png_file, width = 1200, height = 1000)

# Trim image whitespace using magick
trimmed <- image_read(png_file) |> image_trim()
image_write(trimmed, path = png_file, format = "png")

# Preview the trimmed high-res image
img <- readPNG(png_file)
grid.newpage()
grid.raster(img)
