##########
# output CA state level dmr
#########

## description
# output California state level dmr with option to choose which data date-folder to pull dmrs from
# joins permits, parameters, statistical base code tables

## instruction
# 1) ssh into Sherlock login node, 
# 2) go to the cloned git repo for the ca_mmp project
# 3) run script by typing Rscript 0_output_ca_dmr.R <optional: data download date>
# 4) example: Rscript 0_output_ca_dmr.R 2020-07-07_14-57-43

# setup -------------------------------------------------------------------
# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

# global variables
state <- STATE
download_date <- DOWNLOAD_DATE
figure_dir <- FIGURE_DIR
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR
data_manual_dir <- DATA_MANUAL_DIR

# read and process data ---------------------------------------------------------------

# 1) permit data
permits <- read_csv(file.path(data_raw_dir, 'npdes', 'ICIS_PERMITS.csv'), col_types=cols(.default='c')) %>%
  rename(permit_type = PERMIT_TYPE_CODE,
         facility_size = MAJOR_MINOR_STATUS_FLAG) %>%
  # subste to individual NPDES permits
  filter(permit_type %in% c('NPD')) %>%
  mutate(permit_type = 'individual',
         facility_size = ifelse(facility_size == 'M', 'major', 'minor')) %>%
  # subset to state permits
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  filter(state_code %in% state) %>%
  # look at the most recent permit record
  filter(VERSION_NMBR == '0') %>%
  # keep active permits: EFF: effective, ADC: admin contiued, EXP: expired but not terminated 
  filter(PERMIT_STATUS_CODE %in% c('EFF', 'ADC', 'EXP')) %>%
  # keep permits whose expiration dates are beyond system date - 180 days (EPA allows 180 days for permit extension application)
  #mutate(EXPIRATION_DATE = mdy(EXPIRATION_DATE)) %>%
  #filter(EXPIRATION_DATE >= Sys.Date() - 180) %>%
  # keep permits that are under RNC run: didn't result in any losses 
  filter(RNC_TRACKING_FLAG == 'Y') 

# 2) reference tables
parameters <- read_csv(file.path(data_manual_dir, 'REF_PARAMETER.csv')) %>%
  select(-c(CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE)) %>%
  mutate(PARAMETER_CODE = str_pad(PARAMETER_CODE, 5, pad = '0')) %>%
  # identify all non Type 1 and 2 pollutants to be Type 3 pollutants
  mutate(SNC_FLAG = ifelse(is.na(SNC_FLAG), 3, SNC_FLAG)) 

stats_code <- read_csv(file.path(data_manual_dir, 'REF_STATISTICAL_BASE.csv')) %>%
  select(STATISTICAL_BASE_CODE, STATISTICAL_BASE_LONG_DESC, STATISTICAL_BASE_MONTHLY_AVG)

# dmrs 
read_dmr <- function(dmr_file){
  message('==> Reading ', dmr_file)
  read_csv(dmr_file, col_types = cols_only(EXTERNAL_PERMIT_NMBR = 'c',
                                           PERM_FEATURE_ID = 'c',
                                           PERM_FEATURE_NMBR = 'c',
                                           PERM_FEATURE_TYPE_CODE = 'c',
                                           LIMIT_SET_DESIGNATOR = 'c',
                                           LIMIT_ID = 'c',
                                           PARAMETER_CODE = 'c',
                                           PARAMETER_DESC = 'c',
                                           MONITORING_LOCATION_CODE = 'c',
                                           LIMIT_UNIT_DESC = 'c',
                                           STANDARD_UNIT_DESC = 'c',
                                           LIMIT_VALUE_STANDARD_UNITS = 'd',
                                           STATISTICAL_BASE_CODE = 'c',
                                           LIMIT_VALUE_QUALIFIER_CODE = 'c',
                                           OPTIONAL_MONITORING_FLAG = 'c',
                                           VALUE_TYPE_CODE = 'c',
                                           MONITORING_PERIOD_END_DATE = 'c',
                                           DMR_VALUE_STANDARD_UNITS = 'd',
                                           VALUE_RECEIVED_DATE = 'c',
                                           DAYS_LATE = 'i', 
                                           NODI_CODE = 'c',
                                           EXCEEDENCE_PCT = 'd',
                                           VIOLATION_CODE = 'c',
                                           RNC_DETECTION_CODE = 'c',
                                           RNC_DETECTION_DATE = 'c',
                                           RNC_RESOLUTION_CODE = 'c',
                                           RNC_RESOLUTION_DATE = 'c')) %>%
    # subset to state individual permits that have dmr data 
    inner_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'permit_type', 'facility_size')], by = 'EXTERNAL_PERMIT_NMBR') %>%
    # remove optional monitoring
    filter(OPTIONAL_MONITORING_FLAG == 'N') %>%
    # limited to affirmative effluent limits 
    filter(!is.na(LIMIT_VALUE_STANDARD_UNITS) & !is.na(LIMIT_VALUE_QUALIFIER_CODE)) %>%
    # join parameter code 
    left_join(parameters, by = c("PARAMETER_CODE", "PARAMETER_DESC")) %>%
    # join monthly and nonmonthly series code
    left_join(stats_code, by = "STATISTICAL_BASE_CODE") %>%
    # create series id 
    mutate(series_id = paste0(PERM_FEATURE_TYPE_CODE,
                              '_', LIMIT_SET_DESIGNATOR,
                              '_', MONITORING_LOCATION_CODE,
                              '_', LIMIT_VALUE_STANDARD_UNITS,
                              '_', STATISTICAL_BASE_MONTHLY_AVG)) %>%
    # determine if limit condition is "less than"
    mutate(condition= ifelse(LIMIT_VALUE_QUALIFIER_CODE == '<=', 1, -1)) %>%
    # convert dates 
    mutate(MONITORING_PERIOD_END_DATE = mdy(MONITORING_PERIOD_END_DATE)) %>%
    # calculate quarter for each monitoring value
    mutate(monitoring_quarter =  quarter(MONITORING_PERIOD_END_DATE, with_year = TRUE, fiscal_start = 10)) %>%
    # input limit unit if standard unit is missing
    mutate(value_unit = ifelse(is.na(STANDARD_UNIT_DESC), LIMIT_UNIT_DESC, STANDARD_UNIT_DESC))
}

# output data -------------------------------------------------------------

dmr_files <- list.files(file.path(data_raw_dir, 'dmrs'), full.names = TRUE)
dmr <- lapply(dmr_files, read_dmr) %>%
  do.call(rbind, .) %>%
  arrange(EXTERNAL_PERMIT_NMBR, PARAMETER_CODE, series_id, MONITORING_PERIOD_END_DATE)

# save intermediate dmr for faster access later
write_csv(dmr, file.path(output_data_dir, paste0(state,'_dmrs_', download_date, '.csv')))
message('==> output finished, saved as', paste0(output_data_dir, '/', state,'_dmrs_', download_date, '.csv'))
