# packages ----
library(tidyverse)
library(ggplot2)
library(ggthemes) # includes colorblind color palette
library(pracma) # includes findpeaks() function
library(sf) # for working with spatial data
library(tigris) # for getting shapefiles

# data import and cleanup ----
flu_data <- read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/target-data/target-hospital-admissions.csv")
flu_data_orig <- flu_data # original copy of data

flu_data <- flu_data |>
  mutate(week = epiweek(date), year = epiyear(date)) # adding columns for epiweek/epiyear

## function to assign season given date
## seasons defined as epiweek 40 to epiweek 39 (roughly October to October)
find_season <- function(date) {
  date <- as.Date(date)
  epiyr <- epiyear(date)
  epiwk <- epiweek(date)
  start_yr <- ifelse(epiwk >= 40, epiyr, epiyr - 1)
  end_yr <- start_yr + 1
  paste0(start_yr,"-",end_yr)
}

flu_data$season <- find_season(flu_data$date) # adding column for season

# making choropleth maps ----

# data frame with max weekly rates by state, by season
flu_maxes <- flu_data |>
  group_by(location_name, season) |>
  slice_max(weekly_rate) |>
  relocate(location_name, location, season, date, weekly_rate, value)

states <- states(cb = TRUE) # importing shapefiles for each state
states <- states |>
  filter(!STATEFP %in% c("60", "66", "69", "78")) |>
  shift_geometry() # removing US territories other than PR and moving AK, HI, PR 
# to below the mainland

# looping code over each unique flu season
seasons <- unique(flu_data$season)
choropleths <- as.list(rep(NA, 4)) # creating empty list, ignoring "2021-2022" season
# due to incomplete data
names(choropleths) <- seasons[1:4]

## creates choropleth maps for each season
for(n in 1:4) {
  flu_maxes_filtered <- flu_maxes |>
    filter(location_name != "US", season == seasons[n]) |>
    rename(GEOID = location) # filtering out US data and subsetting for given season
  
  flu_maxes_sf <- left_join(states, flu_maxes_filtered, by = "GEOID")
  # joins data with shapefiles
  
  ## all choropleth maps stored in list
  choropleths[[n]] <- ggplot(flu_maxes_sf, aes(fill = date)) +
    geom_sf() + # choropleth map, filled according to date of max weekly rate
    coord_sf(datum = NA) + # removes coordinate grid on map
    scale_fill_viridis_c(trans = "date", # date transformation on date column
                         labels = scales::label_date("%B %d"), # Month day format
                         option = "inferno", 
                         guide = guide_colorbar(barheight = unit(6, "cm"), reverse = TRUE),
                         # extends colorbar to prevent overlapping, puts older at top
                         breaks = seq(from = min(flu_maxes_sf$date), 
                                      to = max(flu_maxes_sf$date), by = 7)) +
    theme_classic() +
    labs(title = paste("Peak Weekly Influenza Hospitalization Rate in", seasons[n]), fill = "Date")
  
  ggsave(paste0("flu-maxes-", seasons[n], ".jpg"), plot = choropleths[[n]])
}
