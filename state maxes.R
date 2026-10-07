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

## creates choropleth maps for each season ----
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

# finding peaks by state ----

## creating a list of states (plus DC and PR) in alphabetical order, removing national data
state_list <- unique(flu_data$location_name)
state_list <- state_list[order(state_list)]
state_list <- state_list[state_list != "US"]

## function for finding findpeaks() matrix for a given state and season, and converting
## it into a data frame
state_peak <- function(locale, szn) {
  flu_data_state <- flu_data |>
    filter(location_name %in% locale, season %in% szn) |>
    arrange(date) # filters flu_data by a given state and season
  
  peaks_df_state <- flu_data_state |>
    pull(weekly_rate) |>
    findpeaks(threshold = 1) |> # requires peaks to have a threshold of a weekly 
    # hospitalization rate greater than 1
    as.data.frame() # converts to data frame for easy manipulation/use with tidyverse
  
  if (dim(peaks_df_state)[1] > 0) { # requires peaks to actually exist
    peaks_df_state <- peaks_df_state |>
      set_names(c("weekly_rate", "peak", "start", "end")) |>
      mutate(state = locale, 
             peak = flu_data_state$date[peak],
             start = flu_data_state$date[start], 
             end = flu_data_state$date[end]) |>
      relocate(state)
    
    return(peaks_df_state)
  } else {
    return(NULL)
  } # if no peaks, return nothing
}

peaks_2526 <- lapply(as.list(state_list), function(x) {state_peak(x, "2025-2026")}) |>
  list_rbind() # make a list with each entry showing the peaks in every state
# and then bind together in one big data frame using list_rbind()
peaks_2425 <- lapply(as.list(state_list), function(x) {state_peak(x, "2024-2025")}) |>
  list_rbind() # repeating for each season (other than 2021-22)
peaks_2324 <- lapply(as.list(state_list), function(x) {state_peak(x, "2023-2024")}) |>
  list_rbind()
peaks_2223 <- lapply(as.list(state_list), function(x) {state_peak(x, "2022-2023")}) |>
  list_rbind()

## example visualization of peaks for Virginia, 2025-2026 ----
flu_data_va <- flu_data |>
  filter(location_name == "Virginia", season == "2025-2026") |>
  arrange(date)

ggplot(data = flu_data_va, mapping = aes(x = date, y = weekly_rate)) +
  annotate("rect", 
           xmin = state_peak("Virginia", "2025-2026")$start, 
           xmax = state_peak("Virginia", "2025-2026")$end,
           ymin = 0, 
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  geom_line() +
  labs(x = "Date", y = "Weekly Hospitalization Rate", title = "Weekly Influenza Hospitalization Rate in Virginia")

## example visualization of peaks for Wyoming, 2025-2026 ----
flu_data_wy <- flu_data |>
  filter(location_name == "Wyoming", season == "2025-2026") |>
  arrange(date)

ggplot(data = flu_data_wy, mapping = aes(x = date, y = weekly_rate)) +
  # hard-coding each of the peaks for now, will iterate later
  annotate("rect", 
           xmin = state_peak("Wyoming", "2025-2026")$start[1], 
           xmax = state_peak("Wyoming", "2025-2026")$end[1],
           ymin = 0, 
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  annotate("rect", 
           xmin = state_peak("Wyoming", "2025-2026")$start[2], 
           xmax = state_peak("Wyoming", "2025-2026")$end[2],
           ymin = 0, 
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  annotate("rect", 
           xmin = state_peak("Wyoming", "2025-2026")$start[3], 
           xmax = state_peak("Wyoming", "2025-2026")$end[3],
           ymin = 0, 
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  annotate("rect", 
           xmin = state_peak("Wyoming", "2025-2026")$start[4], 
           xmax = state_peak("Wyoming", "2025-2026")$end[4],
           ymin = 0, 
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  geom_point() +
  labs(x = "Date", y = "Weekly Hospitalization Rate", title = "Weekly Influenza Hospitalization Rate in Wyoming")
