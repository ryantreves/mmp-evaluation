
####################
# CA MMP Analysis
## Output state characteristics
####################

## 1. facility ratio
## 1) major
## 2) individual
## 3) sewage
## 4) private
## 5) wastewater

## 2. enforcement and inspection ratio
## 1) enforcement rate (permit level)
## 2) inspection rate (permit level)

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

download_date <- DOWNLOAD_DATE
figure_dir <- FIGURE_DIR
data_output_dir <- DATA_OUTPUT_DIR
data_raw_dir <- DATA_RAW_DIR

# read data ---------------------------------------------------------------

# subset to dmr submitters
dmr_record <- read_csv(file.path(data_output_dir, paste0('dmr_eff_submission_records_', download_date, '.csv')), col_types = cols(.default = 'c')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(monitoring_period_end_date),
         monitoring_quarter = year_quarter,
         monitoring_month = paste0(monitoring_year, 
                                   '.', 
                                   sprintf("%02d",month(monitoring_period_end_date))),
         monitoring_month = as.numeric(monitoring_month)) 

# # subset to matched permits: output from output_matched_permits.R
# sub_dmr_record <- read_csv(file.path(data_output_dir, 'dmr_record_matched_permits.csv'))

# weighted permits: output from output_matched_permits.R 
weighted_dmr_record <- read_csv(file.path(data_output_dir, 'dmr_record_weighted_permits.csv'))

# 1) permits 
permits <- read_csv(file.path(data_raw_dir, 'npdes', 'ICIS_PERMITS.csv'), col_types = cols(.default = 'c')) %>%
  filter(EXTERNAL_PERMIT_NMBR %in% dmr_record$EXTERNAL_PERMIT_NMBR) %>%
  # only keep individual NPDES permits (MMP in 2000 only applied to individual permits)
  filter(PERMIT_TYPE_CODE %in% c('NPD')) %>%
  # only keep the newest version of the permit
  filter(VERSION_NMBR == '0') %>%
  mutate(major_flag = MAJOR_MINOR_STATUS_FLAG == 'M',
         potw_flag = FACILITY_TYPE_INDICATOR == 'POTW')%>%
  distinct()

# 2) sector code
## we use sics instead of naics because sics was more widely used before naics officially replaced sics in 2006
## and our observation period is 1996-2003
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

# 3) stormwater and wastewater flag 
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

# 4) violation records
violations <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_EFF_VIOLATIONS.csv'),
                       col_types = cols_only('NPDES_ID' = col_character(),
                                             'NPDES_VIOLATION_ID' = col_character(),
                                             'VIOLATION_CODE' = col_character(),
                                             'VIOLATION_DESC' = col_character(),
                                             'MONITORING_PERIOD_END_DATE' = col_character())) %>%
  mutate(monitoring_year = year(mdy(MONITORING_PERIOD_END_DATE))) %>%
  # subset to effluent violations
  filter(VIOLATION_CODE == 'E90')

# 5) enforcement records
enforcement <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_VIOLATION_ENFORCEMENTS.csv')) %>%
  mutate(NPDES_VIOLATION_ID = as.character(NPDES_VIOLATION_ID)) %>%
  rename(enf_violation_code = VIOLATION_CODE,
         enf_violation_desc = VIOLATION_DESC)
penalty <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_FORMAL_ENFORCEMENT_ACTIONS.csv')) %>%
  filter(!is.na(FED_PENALTY_ASSESSED_AMT) | !is.na(STATE_LOCAL_PENALTY_AMT)) %>%
  mutate(penalty = (pmax(FED_PENALTY_ASSESSED_AMT, STATE_LOCAL_PENALTY_AMT, na.rm = TRUE) +
                    pmin(FED_PENALTY_ASSESSED_AMT, STATE_LOCAL_PENALTY_AMT, na.rm = TRUE))/2) %>%
  select(NPDES_ID, ENF_IDENTIFIER, AGENCY, SETTLEMENT_ENTERED_DATE, penalty)

## join violation rates
violation_enforcement <- violations %>%
  left_join(enforcement, on = 'NPDES_VIOLATION_ID') %>%
  left_join(penalty) 

# 6) inspection records
inspections <- read_csv(file.path(data_raw_dir, 'npdes', 'NPDES_INSPECTIONS.csv'),
                        col_types = cols_only('NPDES_ID' = col_character(),
                                              'ACTIVITY_TYPE_CODE' = col_character(),
                                              'ACTUAL_END_DATE' = col_character())) %>%
  mutate(monitoring_year = year(mdy(ACTUAL_END_DATE)))

# 1) all permits ---------------------------

# 1) proportion of major permits
major_ratio <- dmr_record %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'major_flag')]) %>%
  group_by(monitoring_year, state_code, major_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n(),
            major_count = sum(major_flag, na.rm = TRUE)) %>%
  mutate(major_ratio = major_count/ dmr_reporter_count) 

# 2) proportion of POTW facilities 
potw_ratio <- dmr_record %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'potw_flag')]) %>%
  group_by(monitoring_year, state_code, potw_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n(),
            potw_count = sum(potw_flag, na.rm = TRUE)) %>%
  mutate(potw_ratio = potw_count/ dmr_reporter_count) 

# 3) proportion of sewerage treatment facilities
sewerage_ratio <- dmr_record %>%
  left_join(sics[, c('EXTERNAL_PERMIT_NMBR', 'sewerage_flag')]) %>%
  group_by(monitoring_year, state_code, sewerage_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = n(),
            sewerage_count = sum(sewerage_flag, na.rm = TRUE)) %>%
  mutate(sewerage_ratio = sewerage_count/ dmr_reporter_count)

## join and output data frame
state_characteristics <- major_ratio %>%
  left_join(potw_ratio) %>%
  left_join(sewerage_ratio) %>%
  mutate_all(replace_na, 0)

write_csv(state_characteristics, file.path(data_output_dir, 'state_characteristics.csv'))

# 2) matched permits ------------------------------------------------------

# sub_state_characteristics <- sub_dmr_record %>%
#   mutate_if(is.logical, as.numeric) %>%
#   group_by(monitoring_year, state_code) %>%
#   summarise(dmr_reporter_count = sum(weight, na.rm = TRUE),
#             major_count = sum(major_flag*weight, na.rm = TRUE),
#             potw_count = sum(potw_flag*weight, na.rm = TRUE),
#             sewerage_count = sum(sewerage_flag*weight, na.rm = TRUE),
#             stormwater_count = sum(stormwater_flag*weight, na.rm = TRUE)) %>%
#   filter(dmr_reporter_count != 0) %>%
#   mutate(major_ratio = major_count/dmr_reporter_count,
#          potw_ratio = potw_count/dmr_reporter_count,
#          sewerage_ratio = sewerage_count/dmr_reporter_count,
#          stormwater_ratio = stormwater_count/dmr_reporter_count)
# 
# write_csv(sub_state_characteristics, file.path(data_output_dir, 'state_characteristics_matched_permits.csv'))

# 3) weighted permits -----------------------------------------------------

weighted_state_characteristic <- weighted_dmr_record %>%
  mutate_if(is.logical, as.numeric) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = sum(weight, na.rm = TRUE),
            major_count = sum(major_flag*weight, na.rm = TRUE),
            potw_count = sum(potw_flag*weight, na.rm = TRUE),
            sewerage_count = sum(sewerage_flag*weight, na.rm = TRUE),
            stormwater_count = sum(stormwater_flag*weight, na.rm = TRUE)) %>%
  filter(dmr_reporter_count != 0) %>%
  mutate(major_ratio = major_count/dmr_reporter_count,
         potw_ratio = potw_count/dmr_reporter_count,
         sewerage_ratio = sewerage_count/dmr_reporter_count,
         stormwater_ratio = stormwater_count/dmr_reporter_count)

write_csv(weighted_state_characteristic, file.path(data_output_dir, 'state_characteristics_weighted_permits.csv'))

# 4) weighted enforcement --------------------------------------
## effluent violation enforcement ratios
## average monetary penalty

enforcement_rate <- violation_enforcement %>%
  mutate(state_code = substr(NPDES_ID, 1, 2)) %>%
  rename(EXTERNAL_PERMIT_NMBR = NPDES_ID) %>%
  inner_join(weighted_dmr_record) %>%
  mutate(enforced = !is.na(ENF_IDENTIFIER)) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR, weight) %>%
  summarise(violation_count = n(),
            enforcement_count = sum(enforced)) %>%
  mutate(enforced = enforcement_count > 0) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(violator_count = sum(weight, na.rm = TRUE),
            enforced_count = sum(enforced*weight, na.rm = TRUE)) %>%
  mutate(enforcement_rate = enforced_count/violator_count) %>%
  mutate(enforcement_rate = replace_na(enforcement_rate, 0))

avg_penalty <- violation_enforcement %>%
  filter(!is.na(penalty)) %>%
  mutate(state_code = substr(NPDES_ID, 1, 2)) %>%
  rename(EXTERNAL_PERMIT_NMBR = NPDES_ID) %>%
  inner_join(weighted_dmr_record) %>%
  group_by(monitoring_year, state_code, EXTERNAL_PERMIT_NMBR, weight) %>%
  summarise(avg_penalty = mean(penalty)) %>%
  mutate(avg_penalty = weight*avg_penalty) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(avg_penalty = mean(avg_penalty))

enforcement_features <- enforcement_rate %>%
  left_join(avg_penalty) %>%
  mutate_all(replace_na, 0) 
  
# 5) weighted inspection  ----------------------------------------------------------
## average inspection frequency: doesn't quite make sense because pre 2000 data is not so complete anyways
## inspection ratio 

inspection_frequency <- weighted_dmr_record %>%
  rename(NPDES_ID = EXTERNAL_PERMIT_NMBR) %>%
  left_join(inspections) %>%
  group_by(monitoring_year, state_code, NPDES_ID, weight) %>%
  summarise(n = n(),
            inspection_count = sum(!is.na(ACTIVITY_TYPE_CODE))) %>%
  mutate(reporter = n > 0, 
         inspected = inspection_count > 0) %>%
  group_by(monitoring_year, state_code) %>%
  summarise(dmr_reporter_count = sum(weight*reporter),
            inspected_count = sum(weight*inspected)) %>%
  mutate(inspection_rate = inspected_count/ dmr_reporter_count) %>%
  mutate(inspection_rate = replace_na(inspection_rate, 0))

# ## placeholder: last updated here, will come back to this
# write_csv(inspection_frequency, file.path(data_output_dir, 'view.csv'))
# ggplot(inspection_frequency, aes(x = monitoring_year, y = inspection_rate)) +
#   geom_line() +
#   xlim(1996, 2003) +
#   facet_wrap(~state_code)
# ggsave(file.path(figure_dir, 'view.pdf'))
  
# save both enforcement and inspection features
enf_insp <- enforcement_features %>%
  full_join(inspection_frequency)
write_csv(enf_insp, file.path(data_output_dir, 'state_enf_insp_rates.csv'))


