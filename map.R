library(maps)

world_map <- mutate(map_data("world"), iso3c = iso.alpha(region, n = 3))
names(world_map)[5] <- "Country"