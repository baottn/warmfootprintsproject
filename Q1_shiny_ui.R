library("shiny")

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
  plotOutput(outputId = "my_plot")
)

q1_layout <- sidebarLayout(
  sidebar_content_q1,
  main_content_q1,
  position = "right"
)

birth_rate_in_different_countries <- tabPanel(
  title = "Birth Rate Changes" ,
  titlePanel("Birth Rate In Different Countries"),
  q1_layout
)

my_ui <- navbarPage(
  title = "Warm babies",
  birth_rate_in_different_countries
)