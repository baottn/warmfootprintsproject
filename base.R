library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 999)

birth_rates <- read.csv("data/children-born-per-woman.csv", stringsAsFactors = FALSE)

# Rename column for easy reading
names(birth_rates)[4] <- "birth_rate"

# Transform and filter data to clean and organize
birth_rates <- birth_rates %>% 
  mutate(Year = as.integer(Year), birth_rate = as.double(birth_rate)) %>% 
  filter(Year > 1949, Year < 2020)

# Turn into wide form 
birth_rates_wide <- spread(birth_rates, key = Year, value = birth_rate)

# Insert missing values
birth_rates_wide[birth_rates_wide$Entity == "Micronesia", "Code"] <- "FSM"
birth_rates_wide[birth_rates_wide$Entity == "Slovak Republic", "Code"] <- "SVK"
birth_rates_wide[birth_rates_wide$Entity == "Timor-Leste", "Code"] <- "TLS"

# Eliminates countries that no longer exist
birth_rates_wide <- filter(birth_rates_wide, nchar(Code) == 3)

birth_rates <- gather(birth_rates_wide, value = birth_rate, key = Year, -Entity, -Code)
birth_rates <- mutate(birth_rates, Year = as.integer(Year))

emissions <- read.csv("data/annual-co2-emissions-per-country.csv", stringsAsFactors = FALSE)

# Rename column for easy reading
names(emissions)[4] <- "CO2_emissions"

# Filters for needed columns
emissions <- emissions %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Year > 1949, Year < 2020)

# Spread data, filled in missing codes
emissions_wide <- spread(emissions, key = Year, value = CO2_emissions)
emissions_wide[emissions_wide$Entity == "Kyrgysztan", "Code"] <- "KGZ"
emissions_wide[emissions_wide$Entity == "Wallis and Futuna Islands", "Code"] <- "WLF"

# added missing iso codes back into long form data
emissions_wide <- filter(emissions_wide, nchar(Code) == 3)
emissions <- gather(emissions_wide, value = CO2_emissions, key = Year, -Entity, -Code) %>% 
  mutate(Year = as.integer(Year))


