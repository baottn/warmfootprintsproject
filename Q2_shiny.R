library(shiny)
library(ggplot2)
library(dplyr)
source("base.R")
source("map.R")

my_ui <- fluidPage(
  h2("Percentage change in emission"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "emission_slider", label = "Pick a birth year (left) and an emissions year (right)", min = 1950, max = 2017, value = c(1997, 2017)),
      checkboxInput("outlier_check_Q2", label = "Remove outliers", value = TRUE)
    ),
    mainPanel(
      plotOutput("emission_map")
    )
  )
)

my_server <- function(input, output){
  output$emission_map <- renderPlot({
    emissions_wide$emissions_change <- (emissions_wide[[toString(input$emission_slider[2])]] - emissions_wide[[toString(input$emission_slider[1])]]) / emissions_wide[[toString(input$emission_slider[1])]] * 100
    names(emissions_wide)[2] <- "iso3c"
    if(input$outlier_check_Q2) {
      quartiles_emissions <- quantile(emissions_wide$emissions_change, na.rm = TRUE)
      iqr_emissions <- quartiles_emissions[4] - quartiles_emissions[2]
      emissions_wide <- emissions_wide[emissions_wide$emissions_change <= (1.5 * iqr_emissions + quartiles_emissions[3]) & 
                                             emissions_wide$emissions_change >= (quartiles_emissions[1] - 1.5 * iqr_emissions), ]
    }
    emissions_joined <- left_join(world_map, emissions_wide, by = "iso3c")
    emissions_change_plot <- ggplot(data = emissions_joined) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = emissions_change)) +
      scale_fill_distiller(palette = "RdYlGn") +
      theme_void() +
      coord_quickmap() +  
      labs(title = paste("Change in CO2 Emissions from", toString(input$emission_slider[1]), "to", toString(input$emission_slider[2])), fill = "% Change")
    return(emissions_change_plot)
  })
}

shinyApp(ui = my_ui, server = my_server)