library("shiny")
library("ggplot2")
library("dplyr")
source("Q1.R")
source("base.R")
source("map.R")

birth_area <- function(selected_range, features){
  #data wrangling
  birth_rates_wide$birth_change <- birth_rates_wide[[toString(selected_range[2])]] - birth_rates_wide[[toString(selected_range[1])]]
  names(birth_rates_wide)[2] <- "iso3c"
  if(features == "Countries that have birth rate decrease or no change") {
    birth_rates_wide <- filter(birth_rates_wide, birth_change <= 0)
  } else {
    birth_rates_wide <- filter(birth_rates_wide, birth_change > 0)
  }
  return(birth_rates_wide)
}

my_server <- function(input_list, output_list){
  output_list$my_plot <- renderPlot({
    new_map_data <- mutate(map_data("world"), iso3c = iso.alpha(region, n = 3))
    joined_map <- left_join(new_map_data, birth_area(input_list$year_choice, input_list$features), by = "iso3c", na.rm = TRUE)
    
    #plot
    the_plot <- ggplot(data = joined_map) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = birth_change)) +
      scale_fill_distiller(palette = "RdYlGn", direction = 1) + 
      labs(title = paste0("Change in Birth Rate (", input_list$year_choice[1],"-" , input_list$year_choice[2], ")"), fill = "Average Birth Change Per Woman") + 
      coord_quickmap() +
      theme_void()
    return(the_plot)
  })
  output_list$mean <- renderText({
    birth_stats <- birth_area(input_list$year_choice, input_list$features)
    
    return(paste("The mean birthrate change for years", input_list$year_choice[1], "and", input_list$year_choice[2], "is", mean(birth_stats$birth_change), ". This data signifies the different overall means of the fertility rate of different countries as well as how it compares to the individual countries. This also shows how most of the birth rates are negative, which supports the fact that global warming affects birth rates."))
  })
}  

