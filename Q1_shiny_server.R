library("shiny")
library("ggplot2")
library("dplyr")
source("Q1.R")
source("base.R")
source("map.R")

my_server <- function(input_list, output_list){
  output_list$my_plot <- renderPlot({
    selected_range <- input_list$year_choice
    #data wrangling
    birth_rates_wide$birth_change <- birth_rates_wide[[toString(selected_range[2])]] - birth_rates_wide[[toString(selected_range[1])]]
    names(birth_rates_wide)[2] <- "iso3c"
    new_map_data <- mutate(map_data("world"), iso3c = iso.alpha(region, n = 3))
    if(input_list$features == "Countries that have birth rate decrease or no change") {
      birth_rates_wide <- filter(birth_rates_wide, birth_change <= 0)
    } else {
      birth_rates_wide <- filter(birth_rates_wide, birth_change > 0)
    }
    joined_map <- left_join(new_map_data, birth_rates_wide, by = "iso3c", na.rm = TRUE)
    
    #plot
    the_plot <- ggplot(data = joined_map) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = birth_change)) +
      scale_fill_distiller(palette = "RdYlGn", direction = 1) + 
      labs(title = paste0("Change in Birth Rate (", selected_range[1],"-" , selected_range[2], ")"), fill = "Average Birth Change Per Woman") + 
      coord_quickmap() +
      theme_void()
    return(the_plot)
  })
}  