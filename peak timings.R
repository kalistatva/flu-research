library(tidyverse)
library(ggplot2)
library(ggthemes)
library(pracma)

flu_data_orig <- read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/target-data/target-hospital-admissions.csv", name_repair = "universal")
flu_data <- flu_data_orig

# data cleanup ----

# adding epiweek
flu_data <- flu_data |>
  mutate(epiweek = epiweek(date), year = epiyear(date))
# function to sort dates into season (epiweeks 40-39 defining a season, 
# October-to-October)
find_season <- function(date) {
  date <- as.Date(date)
  epiyr <- epiyear(date)
  epiwk <- epiweek(date)
  start_yr <- ifelse(epiwk >= 40, epiyr, epiyr - 1)
  end_yr <- start_yr + 1
  paste0(start_yr,"-",end_yr)
}
# adding flu season
flu_data$season <- find_season(flu_data$date)

# using findpeaks() for peaks, storing peaks by state in a list ----

peaks <- function(state, szn) {
  # filters flu_data by state and season, arranges by date
  flu_data_local <- flu_data |>
    filter(location_name == state, season == szn) |>
    arrange(date)
  # finds the peaks of the weekly rate and stores as fp_matrix
  fp_matrix <- flu_data_local |>
    pull(weekly_rate) |>
    findpeaks(threshold = 1)
  # rounds weekly rate and fills in dates
  fp_matrix[,1] <- round(fp_matrix[,1], 3)
  fp_matrix[,2:4] <- format(flu_data_local$date[fp_matrix[,2:4]], "%Y-%m-%d")
  return(fp_matrix)
}

state_list <- unique(flu_data$location_name)
state_list <- state_list[order(state_list)]
flu_seasons <- unique(flu_data$season)

peaks2526 <- sapply(state_list, function(extract) { 
  peaks(extract, szn = "2025-2026")})
names(peaks2526) <- state_list
