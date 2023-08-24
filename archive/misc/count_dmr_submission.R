#########
# count dmr reporters over time 
#########

# setup -------------------------------------------------------------------
# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

# global variables
download_date <- DOWNLOAD_DATE
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR

# read data ---------------------------------------------

# 1) permits 
permits <- read_csv(file.path(data_raw_dir, 'npdes', 'ICIS_PERMITS.csv'), col_types=cols(.default='c')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  # only keep individual permits because MMP only applies to individual permits in 2000
  filter(PERMIT_TYPE_CODE %in% c('NPD')) %>%
  mutate(permit_type_code = ifelse(PERMIT_TYPE_CODE == 'NPD', 'individual', 'general')) %>%
  mutate(major_minor_flag = ifelse(MAJOR_MINOR_STATUS_FLAG == 'M', 'Major', 'Minor')) %>%
  mutate(EFFECTIVE_DATE = mdy(EFFECTIVE_DATE),
         EXPIRATION_DATE = mdy(EXPIRATION_DATE),
         RETIREMENT_DATE = mdy(RETIREMENT_DATE),
         TERMINATION_DATE = mdy(TERMINATION_DATE)) 

## output of 1_output_eff_dmr_submitters.R 
dmr_submission <- read_csv(file.path(data_output_dir, paste0('dmr_value_submission_records_', download_date, '.csv')))

ca <- dmr_submission %>%
  mutate(permit_state = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  filter(permit_state == 'CA')



