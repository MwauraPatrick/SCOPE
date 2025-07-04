# App.R

library(shiny)

# # Optional settings
# options(
#   shiny.sanitize.errors = TRUE,
#   shiny.maxRequestSize = 50 * 1024^2,
#   shiny.fullstacktrace = TRUE
# )

# Load UI and server
source("ui.R")
source("server.R")

# Launch the app
shinyApp(ui = ui, server = server)


rsconnect::writeManifest()
