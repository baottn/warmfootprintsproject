library("shiny")
source("base.R")

#Question 1 layout
birth_rates <- birth_rates %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Year > 1994, Year < 2020) 

features <- birth_rates$Entity
radio_button_input <- radioButtons(
  inputId = "features",
  label = "Birth rate change",
  choices = c("Countries that have birth rate increase", "Countries that have birth rate decrease or no change"))

year_input <- sliderInput(
  inputId = "year_choice" ,
  label = "Year",
  min = 1995,
  max = 2019,
  value = c(1995,2019),
  sep = "")


year_range <- range(birth_rates$Year)

sidebar_content_q1 <- sidebarPanel(
  radio_button_input,
  year_input
)

main_content_q1 <- mainPanel(
  plotOutput(outputId = "q1_plot")
)

q1_layout <- sidebarLayout(
  sidebar_content_q1,
  main_content_q1,
  position = "right"
)

#Question 2 layout
sidebar_content_q2 <- sidebarPanel(
  sliderInput(inputId = "emission_slider", label = "Pick a birth year (left) and an emissions year (right)", min = 1950, max = 2017, value = c(1997, 2017)),
  checkboxInput("outlier_check_Q2", label = "Remove outliers", value = TRUE)
)
main_content_q2 <- mainPanel(
  plotOutput("q2_plot")
)
q2_layout <- sidebarLayout(
  sidebar_content_q2,
  main_content_q2,
)

#Question 3 layout
countries_name <- unique(emissions$Entity)
main_content_q3 <- mainPanel(
  plotOutput(outputId = "q3_plot")
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

#Question 4 layout
sidebar_content_q4 <- sidebarPanel(
  sliderInput(inputId = "world_year_slider", label = "Pick a birth year (left) and an emissions year (right)", min = 1950, max = 2017, value = c(1993, 2011), sep = ""),
  checkboxInput("outlier_check_Q4", label = "Remove outliers", value = TRUE)
)

main_content_q4 <- mainPanel(
  plotOutput("q4_plot"),
  span(textOutput("error"), style="color:red"),
  textOutput("correlation_q4")
)

q4_layout <- sidebarLayout(
  sidebar_content_q4,
  main_content_q4
)
  
#All questions tabs
q1_tab <- tabPanel(
  title = "Birthrate Changes around the world" ,
  titlePanel("Birthrate changes around the world"),
  q1_layout,
  p(textOutput(outputId = "q1_analysis"))
)

q2_tab <- tabPanel(
  title = "CO2 emission around the world",
  titlePanel("Percentage change in emission"),
  q2_layout,
  p(textOutput(outputId = "q2_analysis"))
)

q3_tab <- tabPanel(
  title = "Discover how birth rate affect CO2 emission of a country",
  titlePanel("Birth rate vs CO2 emission"),
  q3_layout,
  p(textOutput(outputId = "q3_analysis"))
)

q4_tab <- tabPanel(
  title = "Comparing every country, how has the birth rate in a single year impacted their increase in CO2 emissions in succeeding years?",
  titlePanel("Effect of birthrate on emission growth in later years"),
  q4_layout,
  p("This graph allows you to cross-sectionally analyis how birth rate in a single year is correlated with emission growths in any suceeding year.
    By examining every country, we get the largest sample size we can. This allows us to answer the question of if, in general, 
    having more children correlates with higher growth in CO2 emissions. Generally, you can find a very strong correlation between higher birth rate 
    and higher growth in CO2 emissions in subsequent years. Showing that the two are strongly linked (by social science standards")
)

my_ui <- navbarPage(
  title = "Warm Babies",
  q1_tab,
  q2_tab,
  q3_tab,
  q4_tab
)


