

############
# output DMR effluent sample submission records
###########

## description
# output permits that have effluent smaple submission records

## instruction
# 1) ssh into Sherlock login node, 
# 2) go to the cloned git repo for the ca_mmp project
# 3) run script by typing Rscript 1_output_eff_dmr_submitters.R 

# setup -------------------------------------------------------------------
# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

# global variables
download_date <- DOWNLOAD_DATE
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR

dmr_files <- list.files(file.path(data_raw_dir, 'dmrs'))

message(paste0('Processing DMRs from ', data_raw_dir))

# 1) read in dmrs and output DMR submission records (reporter level) --------------------------

read_dmr <- function(dmr_file){
  message('==> reading ', dmr_file)
  read_csv(file.path(data_raw_dir, 'dmrs', dmr_file), col_types = cols_only(EXTERNAL_PERMIT_NMBR = 'c',
                                                                            MONITORING_PERIOD_END_DATE = 'c',
                                                                            OPTIONAL_MONITORING_FLAG = 'c',
                                                                            LIMIT_VALUE_STANDARD_UNITS = 'c',
                                                                            LIMIT_VALUE_QUALIFIER_CODE = 'c')) %>%
    filter(OPTIONAL_MONITORING_FLAG == 'N') %>%
    # limited to affirmative effluent limits. We might only want to consider records with DMR values.
    filter(!is.na(LIMIT_VALUE_STANDARD_UNITS) & !is.na(LIMIT_VALUE_QUALIFIER_CODE)) %>%
    mutate(year_quarter = quarter(mdy(MONITORING_PERIOD_END_DATE), with_year = TRUE, fiscal_start = 10)) %>%
    select(EXTERNAL_PERMIT_NMBR, MONITORING_PERIOD_END_DATE, year_quarter) %>%
    distinct()
}

dmr_records <- lapply(dmr_files, read_dmr) %>%
  do.call(rbind, .)

write_csv(dmr_records, file.path(data_output_dir, paste0('dmr_eff_submission_records_', download_date, '.csv')))

message('output finished.')


# 2) read in dmrs and output submission records (value level)  ------------

read_dmr_values <- function(dmr_file){
  message('==> reading ', dmr_file)
  read_csv(file.path(data_raw_dir, 'dmrs', dmr_file), col_types = cols_only(EXTERNAL_PERMIT_NMBR = 'c',
                                                                            MONITORING_PERIOD_END_DATE = 'c',
                                                                            OPTIONAL_MONITORING_FLAG = 'c',
                                                                            LIMIT_VALUE_STANDARD_UNITS = 'c',
                                                                            LIMIT_VALUE_QUALIFIER_CODE = 'c',
                                                                            DMR_VALUE_STANDARD_UNITS = 'c')) %>%
    filter(OPTIONAL_MONITORING_FLAG == 'N') %>%
    # limited to affirmative effluent limits. We might only want to consider records with DMR values.
    filter(!is.na(LIMIT_VALUE_STANDARD_UNITS) & !is.na(LIMIT_VALUE_QUALIFIER_CODE)) %>%
    filter(!is.na(DMR_VALUE_STANDARD_UNITS)) %>%
    mutate(year_quarter = quarter(mdy(MONITORING_PERIOD_END_DATE), with_year = TRUE, fiscal_start = 10))
}

dmr_values <- lapply(dmr_files, read_dmr_values) %>%
  do.call(rbind, .)

write_csv(dmr_values, file.path(data_output_dir, paste0('dmr_value_submission_records_', download_date, '.csv')))

message('output finished.')


