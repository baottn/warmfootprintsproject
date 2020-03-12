library("shiny")
library("ggplot2")
library("dplyr")
source("base.R")

my_server <- function(input_list, output_list) {
  output_list$custom_country_and_year_plot <- renderPlot({
    country_name <- input_list$country_name
    year_range <- input_list$year_range
    #data wrangling
    emissions_over_time <- emissions %>% 
      filter(Entity == country_name, Year <= year_range[2] & Year >= year_range[1] - 1) %>% 
      mutate(change_between_years = CO2_emissions - lag(CO2_emissions)) %>% 
      mutate(CO2_emissions_change_between_years = change_between_years / lag(CO2_emissions)) %>%  
      filter(Year > year_range[1] - 1) %>% 
      select(Year, CO2_emissions, change_between_years, CO2_emissions_change_between_years)
    
    birthrates_over_time <- birth_rates %>% 
      filter(Entity == country_name, Year <= year_range[2] & Year >= year_range[1] - 1) %>% 
      mutate(change_between_years = (birth_rate - lag(birth_rate))) %>% 
      mutate(birthrates_change_between_years = change_between_years / lag(birth_rate)) %>% 
      filter(Year > year_range[1] - 1)
    
    # combine birthrate and emissions data together
    birthrate_emission_changes_over_time <- birthrates_over_time %>% 
      left_join(emissions_over_time, by = "Year") %>% 
      select(Year, birthrates_change_between_years, CO2_emissions_change_between_years)
    
    birthrate_emission_changes_over_time_gathered <- birthrate_emission_changes_over_time %>% 
      gather(
        key = type,
        value = rate,
        c(birthrates_change_between_years, CO2_emissions_change_between_years)
      )
    
    # plot birthrates and emissiona
    the_plot <- ggplot(data = birthrate_emission_changes_over_time_gathered, mapping = aes(x = Year, y = rate, color = type)) +
      geom_point() +
      geom_smooth(method = "loess", formula = y ~ x) +
      scale_color_discrete(labels = c("Birth rate", "CO2 emission rate")) +
      labs(
        title = paste("Changes in birth rate and CO2 emission rate of", country_name ,"from", year_range[1], "to", year_range[2]),
        y = "Percent change(%) per year",
        color = "Type"
      ) +
      theme(axis.text.x = element_text(size = 5,angle = 90))
    return(the_plot)
  })
  
  output_list$q3_analysis <- renderText({
    
  })
}

