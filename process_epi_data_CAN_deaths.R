# Date of first events in each geographical unit

library(lubridate)
library(sqldf)
library(stringr)

# Set directories
source(sprintf("%s/set_directories.R", here::here()))

# Get Canada P/T information
PT = readRDS(sprintf("%s/DATA/CAN_PT.Rds", DIRS$CODE))

# Get latest data date
data_files = list.files(path = DIRS$COVID_DATA,
                        pattern = glob2rx("CAN_deaths_data_*.Rds"))
latest_data_file = sort(data_files, decreasing = TRUE)[1]
date_of_data_file = substr(latest_data_file,17,26)

fullDATA = readRDS(file = sprintf("%s/CAN_deaths_data_%s.Rds",
                                  DIRS$COVID_DATA,
                                  date_of_data_file))
# Make dates proper
fullDATA$date_death_report = ymd(as_date(fullDATA$date_death_report))

# Results are stored in a list, which we make extensive
DATA = list()
DATA$raw_data = fullDATA

fullDATA_out = data.frame(province = as.character(unlist(fullDATA$province)),
                          health_region = as.character(unlist(fullDATA$health_region)),
                          date_report = unlist(fullDATA$date_death_report),
                          age = unlist(fullDATA$age),
                          sex = unlist(fullDATA$sex),
                          stringsAsFactors = FALSE)

# Get P/T in standardised form
for (i in 1:dim(PT)[1]) {
  fullDATA_out$province[which(fullDATA_out$province == PT$name_in_data[i])] = PT$abbrev_long[i]
}

query = "SELECT * FROM fullDATA_out
ORDER BY province, health_region, date_report"
fullDATA = sqldf(query)

# Create list of provinces/health regions
provinces_hr = paste0(str_pad(fullDATA$province, 
                              width = (max(nchar(fullDATA$province))+1),
                              pad = " "),
                      str_pad(fullDATA$health_region, 
                              width = (max(nchar(fullDATA$health_region))+1), 
                              pad = " "))
provinces_hr = data.frame(p_hr = unique(provinces_hr),
                          stringsAsFactors = FALSE)
provinces_hr$province = str_trim(substr(provinces_hr$p_hr,
                                        1, (max(nchar(fullDATA$province))+1)))
provinces_hr$health_region = str_trim(substr(provinces_hr$p_hr, 
                                             (max(nchar(fullDATA$province))+2), 
                                             nchar(provinces_hr$p_hr)))

# Some dates are missing at the beginning. 
# We need to add them before we compute anything. First, list all dates
all_dates = seq(from = as_date(min(unique(fullDATA$date_report))),
                to = as_date(max(unique(fullDATA$date_report))),
                by = "day")
DATA$dates = all_dates
# Process provinces/health regions. New cases per day.
DATA$deaths_province_hr = mat.or.vec(nr = dim(provinces_hr)[1],
                                     nc = length(all_dates))
for (i in 1:dim(provinces_hr)[1]) {
  idx_province = which(fullDATA$province == provinces_hr$province[i])
  idx_hr = which(fullDATA$health_region == provinces_hr$health_region[i])
  idx_selected = intersect(idx_province, idx_hr)
  curr_prov_hr = fullDATA[idx_selected,]
  for (d in 1:length(all_dates)) {
    tmp = which(as_date(curr_prov_hr$date_report) == all_dates[d])
    DATA$deaths_province_hr[i, d] = length(tmp)
  }
}
DATA$deaths_province_hr = as.matrix(DATA$deaths_province_hr)
colnames(DATA$deaths_province_hr) = as.character(all_dates)
DATA$deaths_province_hr = as.data.frame(DATA$deaths_province_hr)
DATA$deaths_province_hr = cbind(provinces_hr$province,
                                provinces_hr$health_region,
                                DATA$deaths_province_hr)
colnames(DATA$deaths_province_hr)[1:2] = c("province","health_region")


# Cumulative deaths in provinces/health regions
DATA$cum_deaths_province_hr = mat.or.vec(nr = dim(provinces_hr)[1],
                                         nc = length(all_dates))
for (i in 1:dim(DATA$cum_deaths_province_hr)[1]) {
  tmp = DATA$deaths_province_hr[i,3:dim(DATA$deaths_province_hr)[2]]
  DATA$cum_deaths_province_hr[i,] = cumsum(as.numeric(tmp))
}
colnames(DATA$cum_deaths_province_hr) = as.character(all_dates)
DATA$cum_deaths_province_hr = cbind(provinces_hr$province,
                                    provinces_hr$health_region,
                                    DATA$cum_deaths_province_hr)
colnames(DATA$cum_deaths_province_hr)[1:2] = 
  c("province","health_region")

# deaths per province
PT_tmp = sort(unique(provinces_hr$province))
DATA$deaths_PT = mat.or.vec(nr = length(PT_tmp),
                            nc = length(all_dates))
for (i in 1:length(PT_tmp)) {
  idx_curr_PT = which(DATA$deaths_province_hr$province == PT_tmp[i])
  tmp = DATA$deaths_province_hr[idx_curr_PT,3:dim(DATA$deaths_province_hr)[2]]
  DATA$deaths_PT[i,] = colSums(tmp)
}
DATA$deaths_PT = as.data.frame(DATA$deaths_PT)
rownames(DATA$deaths_PT) = PT_tmp
colnames(DATA$deaths_PT) = all_dates
DATA$deaths_PT = cbind(PT_tmp,
                       DATA$deaths_PT)
colnames(DATA$deaths_PT)[1] = c("province")

# Cumulative deaths in PT
DATA$cum_deaths_PT = mat.or.vec(nr = length(PT_tmp),
                                nc = length(all_dates))
for (i in 1:dim(DATA$cum_deaths_PT)[1]) {
  tmp = DATA$deaths_PT[i,2:dim(DATA$deaths_PT)[2]]
  DATA$cum_deaths_PT[i,] = cumsum(as.numeric(tmp))
}
DATA$cum_deaths_PT = as.data.frame(DATA$cum_deaths_PT)
colnames(DATA$cum_deaths_PT) = as.character(all_dates)
DATA$cum_deaths_PT = cbind(PT_tmp,
                           DATA$cum_deaths_PT)
colnames(DATA$cum_deaths_PT)[1] = c("province")

# Create dates of first events by geography
DATA$first_event_province_hr = c()
for (i in 1:dim(DATA$cum_deaths_province_hr)[1]) {
  tmp = DATA$cum_deaths_province_hr[i,3:dim(DATA$cum_deaths_province_hr)[2]]
  idx_first = which(tmp>0)[1]
  DATA$first_event_province_hr = c(DATA$first_event_province_hr,
                                   names(tmp)[idx_first])
}
DATA$first_event_province_hr = data.frame(date = DATA$first_event_province_hr,
                                          stringsAsFactors = FALSE)
rownames(DATA$first_event_province_hr) = sprintf("%s_%s",
                                                 DATA$cum_deaths_province_hr[,1],
                                                 DATA$cum_deaths_province_hr[,2])
# For P/T, do all with NA when not present
DATA$first_event_PT = c()
for (pt in PT$abbrev_long) {
  if (!(pt %in% unique(DATA$cum_deaths_PT$province))) {
    DATA$first_event_PT = c(DATA$first_event_PT, NA)
  }
  idx_prov = which(pt == DATA$cum_deaths_PT$province)
  if (length(idx_prov)>0) {
    tmp = DATA$cum_deaths_PT[idx_prov, 2:dim(DATA$cum_deaths_PT)[2]]
    idx_first = which(tmp>0)[1]
    if (length(idx_first)>0) {
      DATA$first_event_PT = c(DATA$first_event_PT,
                              names(tmp)[idx_first])
    } else {
      DATA$first_event_PT = c(DATA$first_event_PT, NA)
    }
  }
}
DATA$first_event_PT = data.frame(date = DATA$first_event_PT,
                                 stringsAsFactors = FALSE)
rownames(DATA$first_event_PT) = PT$abbrev_long

DATA$first_event_CAN = sort(DATA$first_event_PT$date)[1]
DATA$cum_deaths_CAN = colSums(DATA$cum_deaths_PT[2:dim(DATA$cum_deaths_PT)[2]])

# Save result
saveRDS(DATA, 
        file = sprintf("%s/CAN_deaths_first_events_%s.Rds",
                       DIRS$COVID_DATA, all_dates[length(all_dates)]))


