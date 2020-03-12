library("shiny")
library("ggplot2")
library("dplyr")
source("base.R")

#data wrangling
emission_data <- function (country_name, year_range) {
  emissions_over_time <- emissions %>% 
    filter(Entity == country_name, Year <= year_range[2] & Year >= year_range[1] - 1) %>% 
    mutate(change_between_years = CO2_emissions - lag(CO2_emissions)) %>% 
    mutate(CO2_emissions_change_between_years = change_between_years / lag(CO2_emissions)) %>%  
    filter(Year > year_range[1] - 1) %>% 
    select(Year, CO2_emissions, change_between_years, CO2_emissions_change_between_years)
  return(emissions_over_time)
}

birthrate_data <- function (country_name, year_range) {
  birthrates_over_time <- birth_rates %>% 
    filter(Entity == country_name, Year <= year_range[2] & Year >= year_range[1] - 1) %>% 
    mutate(change_between_years = (birth_rate - lag(birth_rate))) %>% 
    mutate(birthrates_change_between_years = change_between_years / lag(birth_rate)) %>% 
    filter(Year > year_range[1] - 1)
  return(birthrates_over_time)
}

my_server <- function(input_list, output_list) {
  output_list$custom_country_and_year_plot <- renderPlot({
    country_name <- input_list$country_name
    year_range <- input_list$year_range
    
    # combine birthrate and emissions data together
    birthrate_emission_changes_over_time <- birthrate_data(country_name, year_range) %>% 
      left_join(emission_data(country_name, year_range), by = "Year") %>% 
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
      )
    return(the_plot)
  })
  
  output_list$q3_analysis <- renderText({
    country_name <- input_list$country_name
    year_range <- input_list$year_range
    
    mean_birthrate_change <- mean(birthrate_data(country_name, year_range)$birthrates_change_between_years, na.rm = TRUE)
    mean_emission_change <- mean(emission_data(country_name, year_range)$CO2_emissions_change_between_years, na.rm = TRUE)
    birthrate_change_percentage <- paste0(round(mean_birthrate_change * 100, digits = 2), "%")
    emission_change_percentage <- paste0(round(mean_emission_change * 100, digits = 2), "%")
    absolute_birthrate_change_percentage <- paste0(round(abs(mean_birthrate_change) * 100, digits = 2), "%")
    absolute_emission_change_percentage <- paste0(round(abs(mean_emission_change) * 100, digits = 2), "%")

    if(mean_birthrate_change < 0 & mean_emission_change > 0) {
      conclusion <- paste("The birthrate of", country_name, "is inversely correlated with its CO2 emission in this period.", "The birthrate of", country_name, "decrease by", absolute_birthrate_change_percentage, "while its CO2 emission increase by", emission_change_percentage)
    } else if (mean_birthrate_change > 0 & mean_emission_change < 0) {
      conclusion <-  paste("The birthrate of", country_name, "is inversely correlated with its CO2 emission in this period.", "The birthrate of", country_name, "increase by", birthrate_change_percentage, "while its CO2 emission decrease by", absolute_emission_change_percentage)
    } else if(mean_birthrate_change == 0 | mean_emission_change == 0) {
      conclusion <- paste("The birth rate and CO2 emission of", country_name, "are not correlated with each other because one of them has not changed in this period")
    } else if(mean_birthrate_change > 0 & mean_emission_change > 0) {
      conclusion <-  paste("The birthrate of", country_name, "is positive correlated with its CO2 emission in this period.", "The birthrate of", country_name, "increase by", birthrate_change_percentage, "and its CO2 emission alse increase by", emission_change_percentage)
    } else {
      conclusion <-  paste("The birthrate of", country_name, "is positive correlated with its CO2 emission in this period.", "The birthrate of", country_name, "decrease by", absolute_birthrate_change_percentage, "and its CO2 emission alse decrease by", absolute_emission_change_percentage)
    }
    
    return(paste("In", country_name, "from", year_range[1], "to", year_range[2], ", the birth rate change is", birthrate_change_percentage, "on average and the CO2 emission change is", emission_change_percentage, "on average.", conclusion))
  })
}

