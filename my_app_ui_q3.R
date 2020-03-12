library("shiny")

countries_name <- unique(emissions$Entity)
main_content_q3 <- mainPanel(
  plotOutput(outputId = "custom_country_and_year_plot")
)

country_input <- selectInput(inputId = "country_name", 
                             label = "Select a country", 
                             choices = countries_name,
                             selected  = "Vietnam")

year_range_input <- sliderInput(inputId = "year_range", 
                                label = "Year Range", 
                                min = 1967, 
                                max = 2017, 
                                value = c(1967, 2017),
                                sep = "")


sidebar_content_q3 <- sidebarPanel(
  country_input,
  year_range_input
)

q3_layout <- sidebarLayout(
  sidebar_content_q3,
  main_content_q3,
  position = "right"
)

custom_country_and_year <- tabPanel(
  title = "Discover how birth rate affect CO2 emission of a country",
  titlePanel("Birth rate vs CO2 emission"),
  q3_layout
)

my_ui <- navbarPage(
  title = "Babies and CO2",
  custom_country_and_year
)