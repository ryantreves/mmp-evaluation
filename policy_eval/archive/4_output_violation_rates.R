
##################
# Caifornia MMP Analysis
## Output violation rates
##################

## After we have built up the pipeline for data udpates in our AWS database, this script will become obsolete.

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

download_date <- DOWNLOAD_DATE
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR
data_manual_dir <- DATA_MANUAL_DIR

# read data ---------------------------------------------------------------

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

# 2) pollutant code
parameters <- read_csv(file.path(data_manual_dir, 'REF_PARAMETER.csv')) %>%
  select(-c(CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE)) %>%
  # pad 0 to make sure the parameter code are 5-digits to be consistent with DMR data
  mutate(PARAMETER_CODE = str_pad(PARAMETER_CODE, 5, pad = '0')) %>%
  # identify all non Type 1 and 2 pollutants to be Type 3 pollutants
  mutate(SNC_FLAG = ifelse(is.na(SNC_FLAG), 3, SNC_FLAG)) %>%
  mutate(parameter_group = SNC_FLAG)

# 3) violatoin records for all states
violation <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_EFF_VIOLATIONS.csv'), 
                      col_types = cols_only(NPDES_ID = 'c',
                                            VERSION_NMBR = 'c',
                                            VIOLATION_CODE = 'c',
                                            VIOLATION_DESC = 'c',
                                            MONITORING_PERIOD_END_DATE = 'c',
                                            NPDES_VIOLATION_ID = 'c',
                                            NODI_CODE = 'c',
                                            VIOLATION_TYPE_CODE = 'c',
                                            VIOLATION_TYPE_DESC = 'c', 
                                            RNC_DETECTION_CODE = 'c',
                                            RNC_DETECTION_DESC = 'c',
                                            RNC_RESOLUTION_CODE = 'c',
                                            RNC_RESOLUTION_DESC = 'c',
                                            DMR_VALUE_NMBR = 'c',
                                            ADJUSTED_DMR_VALUE_NMBR = 'c',
                                            PARAMETER_CODE = 'c',
                                            PARAMETER_DESC = 'c',
                                            UNIT_CODE = 'c',
                                            ADJUSTED_DMR_STANDARD_UNITS = 'c',
                                            DMR_VALUE_STANDARD_UNITS = 'c',
                                            EXCEEDENCE_PCT = 'c',
                                            STATISTICAL_BASE_CODE = 'c'
                      )) %>%
  mutate(EXTERNAL_PERMIT_NMBR = NPDES_ID,
         state_code = substr(NPDES_ID, 1, 2),
         violation_type = ifelse(VIOLATION_CODE %in% c('D80', 'D90'), 'Reporting', 'Effluent'),
         exceedence_pct = as.numeric(EXCEEDENCE_PCT)) %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(monitoring_period_end_date),
         monitoring_quarter = quarter(monitoring_period_end_date, with_year = TRUE, fiscal_start = 10),
         monitoring_month = paste0(monitoring_year, '.', 
                                   sprintf("%02d",month(monitoring_period_end_date))),
         monitoring_month = as.numeric(monitoring_month)) %>%
  # filter to effluent violations
  filter(violation_type == 'Effluent') %>%
  # filter to individual permits
  inner_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'permit_type_code')]) %>%
  # remove data errors
  filter(monitoring_period_end_date <= Sys.Date()) %>%
  # get parameter group I/II/III
  left_join(parameters[, c('PARAMETER_CODE', 'parameter_group')]) 

# 4) dmr submission record at the permit-monitoring period level: output by 1_output_eff_dmr_submitters.R
dmr_record <- read_csv(file.path(data_output_dir, paste0('dmr_eff_submission_records_', download_date, '.csv'))) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(monitoring_period_end_date),
         monitoring_quarter = year_quarter,
         monitoring_month = paste0(monitoring_year, 
                             '.', 
                             sprintf("%02d",month(monitoring_period_end_date))),
         monitoring_month = as.numeric(monitoring_month)) %>%
  # filter to individual permits
  inner_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'permit_type_code')]) 

# 5) dmr submission record at the permit-value level: output by 1_output_eff_dmr_submitters.R
dmr_submission <- read_csv(file.path(data_output_dir, paste0('dmr_value_submission_records_', download_date, '.csv'))) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(monitoring_period_end_date),
         monitoring_quarter = year_quarter,
         monitoring_month = paste0(monitoring_year, 
                                   '.', 
                                   sprintf("%02d",month(monitoring_period_end_date))),
         monitoring_month = as.numeric(monitoring_month)) %>%
  # filter to individual permits
  inner_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'permit_type_code')]) 

# 6) matched facilities by permit type: output by output_matched_permits.R
matched_permits <- read_csv(file.path(data_output_dir, 'dmr_record_matched_permits.csv'))

# 7) permits with weights: output by output_matched_permits.R
permit_weights <- read_csv(file.path(data_output_dir, 'dmr_record_weighted_permits.csv'))


# Output Violation Rates --------------------------------------------------

# 1) aggregated at three different levels: monthly, quarterly, and yearly --------

output_violation_rate <- function(violation, dmr_record, frequency){
  
  dmr_reporter_count <- dmr_record %>%
    group_by(!!as.name(frequency), state_code, EXTERNAL_PERMIT_NMBR) %>%
    # monthly repoter: dmr_report_count would be 3 if grouped by quarter
    summarise(dmr_report_count = n()) %>%
    group_by(!!as.name(frequency), state_code) %>%
    summarise(dmr_reporter_count = n())
  
  violator_count <- violation %>%
    group_by(!!as.name(frequency), state_code, EXTERNAL_PERMIT_NMBR) %>%
    summarise(violation_count = n()) %>%
    group_by(!!as.name(frequency), state_code) %>%
    summarise(violator_count = n())
  
  violation_rate <- dmr_reporter_count %>%
    left_join(violator_count) %>%
    mutate(violator_count = replace_na(violator_count, 0)) %>%
    mutate(violation_rate = violator_count/dmr_reporter_count)
  
  write_csv(violation_rate, file.path(data_output_dir, paste0('violation_rate_', frequency, '.csv')))
  
}

frequencies <- c('monitoring_month', 'monitoring_quarter', 'monitoring_year')

results <- lapply(frequencies, output_violation_rate,
       violation = violation,
       dmr_record = dmr_record)


# 2) subset to early reporters (yearly) -----------------------------------

tmp <- dmr_record %>%
  # cap post-treatment time to 2003 
  filter(monitoring_year <= 2003) %>%
  mutate(pre_2000 = monitoring_year < 2000) %>%
  group_by(EXTERNAL_PERMIT_NMBR, pre_2000) %>%
  summarise(report_count = n()) %>%
  spread(pre_2000, report_count) %>%
  filter(`TRUE` > 0 & `FALSE` > 0)

early_reporters <- unique(tmp$EXTERNAL_PERMIT_NMBR)

dmr_reporter_count <- dmr_record %>%
  # only keep early reporters
  filter(EXTERNAL_PERMIT_NMBR %in% early_reporters) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_report_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n())

violator_count <- violation %>%
  # only keep early reporters
  filter(EXTERNAL_PERMIT_NMBR %in% early_reporters) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(violator_count = n())

violation_rate <- dmr_reporter_count %>%
  left_join(violator_count) %>%
  mutate(violator_count = replace_na(violator_count, 0)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

write_csv(violation_rate, file.path(data_output_dir, 'violation_rate_early_reporter.csv'))


# 3) subset to facilities that are similar to CA permits ------------------

dmr_reporter_count <- matched_permits %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_report_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n())

violator_count <- violation %>%
  inner_join(matched_permits[, c('monitoring_year', 'EXTERNAL_PERMIT_NMBR')]) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(violator_count = n())

violation_rate <- dmr_reporter_count %>%
  left_join(violator_count) %>%
  mutate(violator_count = replace_na(violator_count, 0)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)
  
write_csv(violation_rate, file.path(data_output_dir, 'violation_rate_matched_permits.csv'))

# 4) violation rate with weighted permits ---------------------------------

dmr_reporter_count <- permit_weights %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = sum(weight, na.rm = TRUE)) %>%
  filter(dmr_reporter_count != 0)

violator_count <- violation %>%
  left_join(permit_weights[, c('monitoring_year', 'EXTERNAL_PERMIT_NMBR', 'weight')]) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR, weight) %>%
  summarise(violation_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(violator_count = sum(weight, na.rm = TRUE))

violation_rate <- dmr_reporter_count %>%
  left_join(violator_count) %>%
  mutate(violator_count = replace_na(violator_count, 0)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count) 

write_csv(violation_rate, file.path(data_output_dir, 'violation_rate_weighted_permits.csv'))
  
# 5) aggregate by MMP and non-MMP violations (yearly) ---------------------

## for now only check serious MMP because flagging chronic violations takes a lot of time (> 1 hr)

dmr_reporter_count <- dmr_record %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_report_count = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n())

## flag and count serious # and chronic MMP
violation <- violation %>%
  mutate(serious_mmp_flag = (parameter_group == 1 & exceedence_pct >= 40) |
           (parameter_group == 2 & exceedence_pct >= 20)) %>%
  mutate(serious_mmp_flag = replace_na(serious_mmp_flag, FALSE)) %>%
  arrange(EXTERNAL_PERMIT_NMBR, monitoring_period_end_date)

# count_chronic <- function(permit, violation_date){
#   violation %>%
#     filter(EXTERNAL_PERMIT_NMBR == permit &
#              monitoring_period_end_date < violation_date &
#              monitoring_period_end_date >= add_with_rollback(violation_date, months(-6))) %>%
#     nrow()
# }
# 
# # this would take a few minutes
# chronic_mmp <- violation %>%
#   arrange(EXTERNAL_PERMIT_NMBR, monitoring_period_end_date) %>%
#   group_by(EXTERNAL_PERMIT_NMBR, monitoring_period_end_date) %>%
#   mutate(chronic_violation_count = count_chronic(EXTERNAL_PERMIT_NMBR, monitoring_period_end_date),
#          chronic_mmp_flag = chronic_violation_count >= 4)
# 
# violation <- chronic_mmp %>%
#   full_join(violation)
# 
# violation <- violation %>%
#   mutate(chronic_mmp_flag = chronic_violation_count >= 4) %>%
#   # if the 4th is a serious MMP, it would be counted as serious MMP
#   mutate(chronic_mmp_flag = chronic_mmp_flag & !serious_mmp_flag) %>%
#   mutate(mmp_flag = serious_mmp_flag | chronic_mmp_flag)

violator_count <- violation %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR, serious_mmp_flag) %>%
  summarise(violation_count = n()) %>%
  group_by(monitoring_year, state_code, serious_mmp_flag) %>%
  summarise(violator_count = n())

violation_rate <- dmr_reporter_count %>%
  left_join(violator_count) %>%
  mutate(violator_count = replace_na(violator_count, 0)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)
  
write_csv(violation_rate, file.path(data_output_dir, 'violation_rate_serious_mmp.csv'))

# 6) yearly at the permit-level -------------------------------------------

dmr_submission_count <- dmr_submission %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  # monthly repoter: dmr_report_count would be 3 if grouped by quarter
  summarise(submission_count = n())

violation_count <- violation %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = n())

violation_rate <- dmr_submission_count %>%
  left_join(violation_count) %>%
  mutate(violation_count = replace_na(violation_count, 0)) %>%
  mutate(violation_rate = violation_count/submission_count) %>%
  # removing five odd cases (violation rate > 1)
  filter(violation_rate <= 1)

write_csv(violation_rate, file.path(data_output_dir, paste0('violation_rate_yearly_permit.csv')))


