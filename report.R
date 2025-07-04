generate_report <- function(sim_result, design_type, output_file) {
  
  # Check inputs
  if (is.null(sim_result)) {
    stop("Simulation result is null")
  }
  
  # plot paths check
  plot_paths <- c(sim_result$plot_path, sim_result$survival_plot_path)
  valid_plot_paths <- plot_paths[file.exists(plot_paths)]
  
  # Create PDF report
  pdf(output_file, width = 11, height = 8.5)
  
  # Title Page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 1)))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.text(paste(design_type, "Sample Size Determination"), 
            x = 0.5, y = 0.7, gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text(paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), 
            x = 0.5, y = 0.5, gp = gpar(fontsize = 12))
  popViewport(2)
  
  # Summary Table Page
  if (!is.null(sim_result$summary_results)) {
    grid.newpage()
    title <- textGrob("Summary Results", gp = gpar(fontsize = 16, fontface = "bold"))
    
    tryCatch({
      table_grob <- tableGrob(sim_result$summary_results)
      grid.draw(title)
      grid.draw(table_grob)
    }, error = function(e) {
      grid.text("Unable to render summary table", gp = gpar(col = "red"))
    })
  }
  
  # Add plots
  if (length(valid_plot_paths) > 0) {
    for (plot_path in valid_plot_paths) {
      # New page for each plot
      grid.newpage()
      
      tryCatch({
        img <- readPNG(plot_path)
        grid.raster(img)
        grid.text(paste("Plot:", basename(plot_path)), 
                  y = 0.05, gp = gpar(fontsize = 10))
      }, error = function(e) {
        grid.text(paste("Unable to load plot:", basename(plot_path)), 
                  gp = gpar(col = "red"))
      })
    }
  } else {
    grid.newpage()
    grid.text("No plots available", gp = gpar(col = "red"))
  }
  
  # Close PDF device
  dev.off()
  
  return(output_file)
}