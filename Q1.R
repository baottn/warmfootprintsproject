source("base.R")
source("map.R")

#1. What are the top ten countries that have the greatest decrease in birthrate between 1995 and 2020?

#add the change in birth
birth_area <- mutate(birth_rates_wide, birth_change = `2019` - `1995`) 
names(birth_area)[2] <- "iso3c"


#use left_join to combine data sets 
joined_map <- left_join(world_map, birth_area, by = "iso3c")

#make a plot
birth_plot <- ggplot(data = joined_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = birth_change)) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) + 
  labs(title = "Change in Birth Rate (1995-2019)", fill = "Average Birth Change Per Woman") + 
  coord_quickmap() +
  theme_void()

#sample birth rate table
birth_rate_sample_df <- head(birth_area %>% 
                               select(Entity, iso3c, '1995', '2019', birth_change))
#anaylsis of the birth rate table
birth_rate_analysis <- head(mutate(birth_rate_sample_df, 
                                   overall_average_change = mean(birth_change, na.rm = TRUE),
                                   overall_max_change = max(birth_change, na.rm = TRUE),
                                   overall_min_change = min (birth_change, na.rm = TRUE)))
#plot for analysis
birth_rate_plot_analysis <-  
  birth_area %>% 
  drop_na(birth_change) %>% 
  ggplot(aes(x = reorder(iso3c, birth_change), y= birth_change)) +
  geom_bar(stat = "identity") +
  xlab("") 

birth_rate_plot_analysis <- birth_rate_plot_analysis + labs(title = "Average Birth Rate Change in Different Countries (1995-2019)") +
  xlab("Country (iso3)") + ylab("Average Birth Change") +
  theme(axis.text.x = element_text(size = 3, angle = 90))