

####################
# CA MMP Analysis
## Facility Matching
####################

# match California facilities with similar facilities in other states
# output a list of facilities by each state

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

state <- STATE
download_date <- DOWNLOAD_DATE
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR

# read data ---------------------------------------------------------------

# subset to dmr submitters
dmr_record <- read_csv(file.path(data_output_dir, paste0('dmr_eff_submission_records_', download_date, '.csv')), col_types = cols(.default = 'c')) %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(monitoring_period_end_date),
         monitoring_quarter = year_quarter,
         monitoring_month = paste0(monitoring_year, 
                                   '.', 
                                   sprintf("%02d",month(monitoring_period_end_date))),
         monitoring_month = as.numeric(monitoring_month)) 

# 1) permits 
permits <- read_csv(file.path(data_raw_dir, 'npdes', 'ICIS_PERMITS.csv'), col_types = cols(.default = 'c')) %>%
  filter(EXTERNAL_PERMIT_NMBR %in% dmr_record$EXTERNAL_PERMIT_NMBR) %>%
  # only keep individual NPDES permits
  filter(PERMIT_TYPE_CODE %in% c('NPD')) %>%
  # only keep the newest version of the permit
  filter(VERSION_NMBR == '0') %>%
  mutate(major_flag = MAJOR_MINOR_STATUS_FLAG == 'M',
         potw_flag = FACILITY_TYPE_INDICATOR == 'POTW')%>%
  distinct()

# 2) sector code
## we use sics instead of naics because sics was more widely used before naics officially replaced sics in 2006
# https://ofmpub.epa.gov/apex/guideme_ext/f?p=guideme:qa:::::qa:19-004
sics <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_SICS.csv'), col_types = cols(.default = 'c')) %>%
  mutate(EXTERNAL_PERMIT_NMBR = NPDES_ID) %>%
  filter(EXTERNAL_PERMIT_NMBR %in% dmr_record$EXTERNAL_PERMIT_NMBR) %>%
  mutate(sewerage_flag = SIC_DESC == 'Sewerage Systems') %>%
  distinct() %>%
  # if a permit is both sewerage and non sewerage: flag as sewerage
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  add_count() %>%
  filter(n == 1 | (n==2 & sewerage_flag)) %>%
  select(-n)

# 3) stormwater flag 
components <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_PERM_COMPONENTS.csv'), col_types = cols(.default = 'c')) %>%
  filter(EXTERNAL_PERMIT_NMBR %in% dmr_record$EXTERNAL_PERMIT_NMBR) %>%
  mutate(stormwater_flag = COMPONENT_TYPE_CODE %in% c('SWC', 'SWI', 'SWM', 'SWS')) %>%
  select(EXTERNAL_PERMIT_NMBR, stormwater_flag) %>%
  distinct() %>%
  # if a permit is both stormwater and non stormwater: flag as storm
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  add_count() %>%
  filter(n == 1 | (n==2 & stormwater_flag)) %>%
  select(-n)

# 1) prepare data ---------------------------------------------------------
data <- dmr_record %>%
  select(EXTERNAL_PERMIT_NMBR, monitoring_year) %>%
  distinct() %>%
  # filter to individual permits only
  inner_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'major_flag', 'potw_flag')]) %>%
  left_join(sics[, c('EXTERNAL_PERMIT_NMBR', 'sewerage_flag')]) %>%
  left_join(components) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2))

# 2) 1-to-1 matching with random sampling ---------------------------------

permit_count <- data %>%
  group_by(monitoring_year, state_code, major_flag, potw_flag, sewerage_flag, stormwater_flag) %>%
  summarise(permit_count = n()) 

ca_permit_count <- permit_count %>%
  filter(state_code == state) %>%
  ungroup() %>%
  select(-state_code) %>%
  rename(ca_permit_count = permit_count)

matched_permits <- data %>%
  filter(!(is.na(EXTERNAL_PERMIT_NMBR))) %>%
  group_by(monitoring_year, state_code, major_flag, potw_flag, sewerage_flag, stormwater_flag) %>%
  summarise(ids = list(EXTERNAL_PERMIT_NMBR)) %>%
  full_join(permit_count) %>%
  full_join(ca_permit_count) %>%
  mutate(permit_count = replace_na(permit_count, 0)) %>%
  # only keep union of matched permits
  filter(!is.na(ca_permit_count)) %>%
  # match CA permits with other states
  # if the state has more permits than CA: randomly sample equal amount of permits
  # if the staet has less permits than CA: take all permits
  mutate(matched_permits = list(sample(x = unlist(ids[[1]]), 
                                  size = ifelse(permit_count >= ca_permit_count, ca_permit_count, permit_count), 
                                  replace = FALSE))) %>%
  mutate(matched_permits = ifelse(matched_permits == 'character(0)', NA, matched_permits))

# check errors
# lapply(1:10, function(i){
#   sample(x = unlist(matched_permits$ids[i][[1]]),
#          size = ifelse(matched_permits$permit_count[i] >= matched_permits$ca_permit_count[i], matched_permits$ca_permit_count[i], matched_permits$permit_count[i]),
#          replace = FALSE)
# })

# group permits by year
matched_permits <- matched_permits %>%
  group_by(monitoring_year) %>%
  summarise(ids = list(unique(unlist(matched_permits))))

sapply(1:nrow(matched_permits), function(i){
  length(unlist(matched_permits[i, 'ids']))
})

years <- unique(matched_permits$monitoring_year)

sub_data <- lapply(years, function(year){
  sub_permits <-  matched_permits[matched_permits$monitoring_year == year, 'ids'] %>%
    unlist()
  
  data %>%
    filter(EXTERNAL_PERMIT_NMBR %in% sub_permits) %>%
    filter(monitoring_year == year)
}) %>%
  bind_rows()

write_csv(sub_data, file.path(data_output_dir, 'dmr_record_matched_permits.csv'))


# 2) weighted matching ----------------------------------------------------

# assign weights to permits based on matching with California permits

permit_count <- data %>%
  group_by(monitoring_year, state_code, major_flag, potw_flag, sewerage_flag, stormwater_flag) %>%
  summarise(permit_count = n()) 

ca_permit_count <- permit_count %>%
  filter(state_code == state) %>%
  ungroup() %>%
  select(-state_code) %>%
  rename(ca_permit_count = permit_count)

weights <- permit_count %>%
  full_join(ca_permit_count) %>%
  mutate(permit_count = replace_na(permit_count, 0),
         ca_permit_count = replace_na(ca_permit_count, 0)) %>%
  mutate(weight = ca_permit_count/permit_count) 

weighted_permits <- data %>%
  left_join(weights)

write_csv(weighted_permits, file.path(data_output_dir, 'dmr_record_weighted_permits.csv'))



