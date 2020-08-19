# Refresh CAN COVID-19 epi data from U of T source feed
#
# This script is intended to be run automatically daily, for instance from crontab
#
# Julien Arino
# Department of Mathematics & Data Science NEXUS
# University of Manitoba


# Libraries used
library(lubridate)

# Set directories
source(sprintf("%s/set_directories.R", here::here()))

# Today's date
date_today = as_date(Sys.Date())

# Get dates of files in the data directory. Only refresh if needed.
CAN_DATA_files = list.files(path = DIRS$COVID_DATA,
                            pattern = glob2rx("CAN_incidence_first_events_*.Rds"))
latest_CAN_DATA_file = sort(CAN_DATA_files, decreasing = TRUE)[1]
date_of_CAN_confirmed_file = as_date(substr(latest_CAN_DATA_file,28,37))
CAN_DATA_files = list.files(path = DIRS$COVID_DATA,
                            pattern = glob2rx("CAN_deaths_first_events_*.Rds"))
latest_CAN_DATA_file = sort(CAN_DATA_files, decreasing = TRUE)[1]
date_of_CAN_deaths_file = as_date(substr(latest_CAN_DATA_file,25,34))

# If this is the first run, there will not be any data files. To avoid problems, just set 
# dates to unrealistic values
date_of_CAN_confirmed_file = ymd("1970-01-01")
date_of_CAN_deaths_file = ymd("1970-01-01")


# DOWNLOAD CASES
# Post 4.0, stringsAsFactors = FALSE is default, but we keep it for late updaters
CAN_data_cases = read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv",
                          stringsAsFactors = FALSE)
# Prepare to save the data (initial, almost raw)
CAN_data_cases$date_report = dmy(CAN_data_cases$date_report)
latest_date_CAN_data_cases = as_date(sort(unique(CAN_data_cases$date_report), 
                                          decreasing = TRUE)[1])
if (latest_date_CAN_data_cases != date_of_CAN_confirmed_file) {
  # We don't have the latest data. We save what we loaded earlier then process it
  saveRDS(CAN_data_cases, 
          file = sprintf("%s/CAN_infection_data_%s.Rds", 
                         DIRS$COVID_DATA,
                         latest_date_CAN_data_cases))
  source(sprintf("%s/process_epi_data_CAN_confirmed.R", 
                 DIRS$CODE))
} else {
  writeLines(paste0("We have the most current confirmed data (dated ",
                    date_of_CAN_confirmed_file, ")"))
}


# DOWNLOAD MORTALITY
CAN_data_deaths = read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv",
                           stringsAsFactors = FALSE)
CAN_data_deaths$date_death_report = dmy(CAN_data_deaths$date_death_report)
# Prepare to save the data (initial, almost raw)
latest_date_CAN_data_deaths = as_date(sort(unique(CAN_data_deaths$date_death_report), 
                                           decreasing = TRUE)[1])
if (latest_date_CAN_data_deaths != date_of_CAN_deaths_file) {
  # We don't have the latest data. We save what we loaded earlier then process it
  saveRDS(CAN_data_deaths, 
          file = sprintf("%s/CAN_deaths_data_%s.Rds", 
                         DIRS$COVID_DATA,
                         latest_date_CAN_data_deaths))
  source(sprintf("%s/process_epi_data_CAN_deaths.R", 
                 DIRS$CODE))
} else {
  writeLines(paste0("We have the most current death data (dated ",
                    date_of_CAN_deaths_file, ")"))
}

