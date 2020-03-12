library("shiny")
library("ggplot2")
library("dplyr")
source("base.R")
source("map.R")
source("Q1.R")


my_server <- function(input_list, output_list){
  
  #q1 
  output_list$q1_plot <- renderPlot({
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
  
  #q3 
  output_list$q3_plot <- renderPlot({
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