library(shiny)
library(ggplot2)
library(dplyr)
source("base.R")
source("map.R")

  sidebar_content_q2 <- sidebarPanel(
    sliderInput(inputId = "emission_slider", 
                label = "Year Range", 
                min = 1950, 
                max = 2017, 
                value = c(1997, 2017)),
    checkboxInput("outlier_check_Q2", 
                  label = "Remove outliers", 
                  value = TRUE)
  )
  
  main_content_q2 <- mainPanel(
    plotOutput("q2_plot"),
    textOutput("q2_text")
  )
  
  q2_layout <- sidebarLayout(
    sidebar_content_q2,
    main_content_q2
  )
  
my_server <- function(input, output){
  output_list$q2_plot <- renderPlot({
    emissions_wide$emissions_change <- (emissions_wide[[toString(input_list$emission_slider[2])]] - emissions_wide[[toString(input_list$emission_slider[1])]]) / emissions_wide[[toString(input_list$emission_slider[1])]] * 100
    names(emissions_wide)[2] <- "iso3c"
    if(input_list$outlier_check_Q2) {
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
      labs(title = paste("Change in CO2 Emissions from", toString(input_list$emission_slider[1]), "to", toString(input_list$emission_slider[2])), fill = "% Change")
    return(emissions_change_plot)
  })
  
  output_list$q2_text <- renderText({
    return(paste("As seen through the map data, an overall increase in CO2 can be observed as countries become more developed and populated over time. While this increase in emissions may not be solely reliant on the overall increase in our world's population, the overall world CO2 increase is a direct cause of countries growing over the range of years. However, many countries have also made drastic decreases in their CO2 emissions. For example, countries who were likely developed prior to 1997 such as Denmark, Malta, and Ukraine were able to decrease their CO2 emissions between 70 to 90%. The drastic increase in CO2 emission in countries such as China may also be attributed to the industrial and population growth of such countries. Looking at the minimum and maximum percentage changes, the difference is striking. Some countries have been capable of reduce CO2 emissions by nearly 90%, whereas some have increased over the twenty year period by the same percentage. Overall, we have witnessed a dramatic increase of over 30% in tons of CO2 emissions globally."))
  })
}

shinyApp(ui = my_ui, server = my_server)