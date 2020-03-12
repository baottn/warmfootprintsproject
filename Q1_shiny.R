library("shiny")
source("Q1_shiny_server.R")
source("Q1_shiny_ui.R")

shinyApp(ui = my_ui, server = my_server)
  
