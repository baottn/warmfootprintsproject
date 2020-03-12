library("shiny")
library("ggplot2")
library("dplyr")
source("base.R")
source("map.R")
source("Q1.R")


my_server <- function(input_list, output_list){
  #q1 
  birth_area <- function(selected_range, features){
    #data wrangling
    birth_rates_wide$birth_change <- birth_rates_wide[[toString(selected_range[2])]] - birth_rates_wide[[toString(selected_range[1])]]
    names(birth_rates_wide)[2] <- "iso3c"
    if(features == "Countries that have birth rate decrease or no change") {
      birth_rates_wide <- filter(birth_rates_wide, birth_change <= 0)
    } else if (features == "Countries that have birth rate increase"){
      birth_rates_wide <- filter(birth_rates_wide, birth_change > 0)
    }
    return(birth_rates_wide)
  }

  output_list$q1_plot <- renderPlot({
    joined_map <- left_join(world_map, birth_area(input_list$year_choice, input_list$features), by = "iso3c", na.rm = TRUE)
    
    #plot
    the_plot <- ggplot(data = joined_map) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = birth_change)) +
      scale_fill_distiller(palette = "RdYlGn", direction = 1) + 
      labs(title = paste0("Change in Birth Rate (", input_list$year_choice[1],"-" , input_list$year_choice[2], ")"), fill = "Average Birth Change Per Woman") + 
      coord_quickmap() +
      theme_void()
    return(the_plot)
  })
  output_list$q1_analysis <- renderText({
    birth_stats <- birth_area(input_list$year_choice, input_list$features)
    return(paste("The mean birthrate change for years", input_list$year_choice[1], "and", input_list$year_choice[2], "is", mean(birth_stats$birth_change), ". This data signifies the different overall means of the fertility rate of different countries as well as how it compares to the individual countries. This also shows how most of the birth rates are negative, which supports the fact that global warming affects birth rates. While the reason for the change may not be solely due to global warming, the amount of countries with negative birth rates shows how there is a decrease in childbirth that matches with the average increase in CO2 emissions worldwide. The highest decrease in average birth rate is Yemen with -3.92, which could be explained due to the civil unrest in the country. On the other hand, the highest increase in average birth rate is Russia with 0.43, which can be attributed to many different factors, including gender roles and government policies. However, with the highest increase in average birth rate being 0.43, the overall change in birth rate is skewed more negative."))
  })
  
  #q2
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
  
  output_list$q2_analysis <- renderText({
    return(paste("As seen through the map data, an overall increase in CO2 can be observed as countries become more developed and populated over time. While this increase in emissions may not be solely reliant on the overall increase in our world's population, the overall world CO2 increase is a direct cause of countries growing over the range of years. However, many countries have also made drastic decreases in their CO2 emissions. For example, countries who were likely developed prior to 1997 such as Denmark, Malta, and Ukraine were able to decrease their CO2 emissions between 70 to 90%. The drastic increase in CO2 emission in countries such as China may also be attributed to the industrial and population growth of such countries. Looking at the minimum and maximum percentage changes, the difference is striking. Some countries have been capable of reduce CO2 emissions by nearly 90%, whereas some have increased over the twenty year period by the same percentage. Overall, we have witnessed a dramatic increase of over 30% in tons of CO2 emissions globally."))
  })
  
  #q3 
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
  
  output_list$q3_plot <- renderPlot({
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
  
  #q4 
  world_table <- function(slider, check) {
    emissions_change <- data.frame(emissions_wide$Entity, emissions_wide$Code, 
                                   (emissions_wide[[toString(slider[2])]] - 
                                      emissions_wide[[toString(slider[[1]])]]) / 
                                     emissions_wide[[toString(slider[[1]])]] * 100, stringsAsFactors = FALSE)
    
    names(emissions_change) <- c("Country", "iso3c", "emissions_change_percent")
    
    birth_rates_year <- data.frame(birth_rates_wide$Code, birth_rates_wide[[toString(slider[[1]])]], stringsAsFactors = FALSE)
    names(birth_rates_year) <- c("iso3c", "birth_rate_plot")
    if(check) {
      quartiles_birth <- quantile(birth_rates_year$birth_rate_plot, na.rm = TRUE)
      iqr_birth <- quartiles_birth[4] - quartiles_birth[2]
      birth_rates_year <- birth_rates_year[birth_rates_year$birth_rate_plot < (1.5 * iqr_birth + quartiles_birth[3]) & 
                                             birth_rates_year$birth_rate_plot > (quartiles_birth[1] - 1.5 * iqr_birth), ]
      
      quartiles_emissions <- quantile(emissions_change$emissions_change_percent, na.rm = TRUE)
      iqr_emissions <- quartiles_emissions[4] - quartiles_emissions[2]
      emissions_change <- emissions_change[emissions_change$emissions_change_percent <= (1.5 * iqr_emissions + quartiles_emissions[3]) & 
                                             emissions_change$emissions_change_percent >= (quartiles_emissions[1] - 1.5 * iqr_emissions), ]
    }
    combined_table <- left_join(emissions_change, birth_rates_year, by = "iso3c") %>% 
      drop_na()
    return(combined_table)
  }
  
  output_list$q4_plot <- renderPlot({
    combined_table <- world_table(input_list$world_year_slider, input_list$outlier_check_Q4)
    world_wide_comparison <- ggplot(data = combined_table, mapping = aes(x = birth_rate_plot, y = emissions_change_percent)) + 
      geom_point() +
      geom_smooth(method = "loess", formula = "y ~ x") +
      labs(title = paste("Effect of birth rate in", input_list$world_year_slider[1], "on CO2 emission growth", input_list$world_year_slider[2] - input_list$world_year_slider[1], "years later"), 
           x = paste("Birth rate per women in", input_list$world_year_slider[1]) , y = paste("% change in CO2 emissions from", input_list$world_year_slider[1], "to", input_list$world_year_slider[2]))
    return(world_wide_comparison)
  })
  
  output_list$error <- renderText({
    if(input_list$world_year_slider[1] == input_list$world_year_slider[2]){
      return("ERROR: Growth rate cannot be calculated when starting year is equal to ending year. Please set a different starting or ending year")
    }
  })
  
  output_list$correlation_q4 <- renderText({
    if(input_list$world_year_slider[1] == input_list$world_year_slider[2]){
      return("")
    }
    combined_table <- world_table(input_list$world_year_slider, input_list$outlier_check_Q4)
    return(paste("The correlation between the birthrate in", input_list$world_year_slider[1], "and the % change in CO2 emissions from", input_list$world_year_slider[1], "to", input_list$world_year_slider[2], "is", cor(combined_table$emissions_change_percent, combined_table$birth_rate_plot)))
  })
}