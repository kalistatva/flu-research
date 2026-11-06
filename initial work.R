# Install required packages if you don't have them
# install.packages(c("readr", "dplyr", "ggplot2", "usmap", "lubridate", "purrr"))

library(tidyverse)
library(ggplot2)
library(usmap)

# 1. Load the dataset from the FluSight GitHub repository
flu_data <- read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/target-data/target-hospital-admissions.csv"
)

# 2. Filter for 2026 and prepare the data
# The dataset typically uses FIPS codes in the 'location' column.
flu_2025_26 <- flu_data %>%
  # Convert date column to Date object just in case
  mutate(date = as.Date(date)) %>%
  # Filter for the year 2026 and remove the national total ("US")
  filter(year(date) == c(2025,2026), location != "US") %>%
  # usmap requires a 'fips' column to map correctly
  rename(fips = location) 

# Check if there is data for 2026
if(nrow(flu_2025_26) == 0) {
  stop("No data found for 2025-26 in the dataset yet.")
}

# 3. Determine the global min and max to lock the color scale across all maps
max_hosp <- max(flu_2025_26$value, na.rm = TRUE)
min_hosp <- min(flu_2025_26$value, na.rm = TRUE)

# 4. Create a function to generate and save a map for a specific date
create_flu_map <- function(target_date) {
  
  # Subset data for the specific date
  daily_data <- flu_2025_26 %>% filter(date == target_date)
  
  # Generate the map using usmap
  p <- plot_usmap(data = daily_data, values = "value", color = "white") +
    # Apply a continuous color scale locked to the global min and max
    scale_fill_viridis_c(
      name = "Admissions", 
      limits = c(min_hosp, max_hosp), 
      option = "plasma", # 'plasma' or 'inferno' are great for heatmaps
      na.value = "grey80" # Color for states with missing data
    ) +
    labs(
      title = "Flu Hospital Admissions by State",
      subtitle = paste("Date:", target_date)
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  
  # Save the plot (creates a sequence of PNG files in your working directory)
  filename <- paste0("flu_map_", target_date, ".png")
  ggsave(filename, plot = p, width = 8, height = 6, bg = "white")
  
  message(paste("Saved:", filename))
}

# 5. Get all unique dates in 2026 and generate maps for each
unique_dates <- unique(flu_2025_26$date)

# Loop through all dates (this will save one PNG per date)
walk(unique_dates, create_flu_map)

print("All 2025-26 maps generated successfully!")
