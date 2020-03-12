library(shiny)
library(ggplot2)
library(dplyr)
source("base.R")

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

my_ui <- fluidPage(
  h2("Effect of birthrate on emission growth in later years"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "world_year_slider", label = "Pick a birth year (left) and an emissions year (right)", min = 1950, max = 2017, value = c(1993, 2011), sep = ""),
      checkboxInput("outlier_check_Q4", label = "Remove outliers", value = TRUE)
    ),
    mainPanel(
      plotOutput("world_scatterplot"),
      span(textOutput("error"), style="color:red"),
      textOutput("correlation")
    )
  ),
  p("This graph allows you to cross-sectionally analyis how birth rate in a single year is correlated with emission growths in any suceeding year.
    By examining every country, we get the largest sample size we can. This allows us to answer the question of if, in general, 
    having more children correlates with higher growth in CO2 emissions. Generally, you can find a very strong correlation between higher birth rate 
    and higher growth in CO2 emissions in subsequent years. Showing that the two are strongly linked (by social science standards")
)

my_server <- function(input, output){
  output$world_scatterplot <- renderPlot({
    combined_table <- world_table(input$world_year_slider, input$outlier_check_Q4)
    world_wide_comparison <- ggplot(data = combined_table, mapping = aes(x = birth_rate_plot, y = emissions_change_percent)) + 
      geom_point() +
      geom_smooth(method = "loess", formula = "y ~ x") +
      labs(title = paste("Effect of birth rate in", input$world_year_slider[1], "on CO2 emission growth", input$world_year_slider[2] - input$world_year_slider[1], "years later"), 
           x = paste("Birth rate per women in", input$world_year_slider[1]) , y = paste("% change in CO2 emissions from", input$world_year_slider[1], "to", input$world_year_slider[2]))
    return(world_wide_comparison)
  })
  
  output$error <- renderText({
    if(input$world_year_slider[1] == input$world_year_slider[2]){
      return("ERROR: Growth rate cannot be calculated when starting year is equal to ending year. Please set a different starting or ending year")
    }
  })

  output$correlation <- renderText({
    if(input$world_year_slider[1] == input$world_year_slider[2]){
      return("")
    }
    combined_table <- world_table(input$world_year_slider, input$outlier_check_Q4)
    return(paste("The correlation between the birthrate in", input$world_year_slider[1], "and the % change in CO2 emissions from", input$world_year_slider[1], "to", input$world_year_slider[2], "is", cor(combined_table$emissions_change_percent, combined_table$birth_rate_plot)))
  })
}

shinyApp(ui = my_ui, server = my_server)