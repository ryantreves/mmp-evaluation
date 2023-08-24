
####################
# CA MMP Analysis
## Synthetic Control
####################

## read in study design arguments
## set bin size and calculate violation rates
## set optimal bandwidth pre-2000 and post-2000 
## set donor states selection criteria
## set predictors for pretreatment period fitting
## output synthetic control data
## output synthetic control results
## output pretreatment balance, effect of MMP, and p value from permutation test
## output permutation test graph

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 
       'stringr', 
       'tidyverse', 
       'Synth', 
       'rdrobust', 
       'rdd')

download_date <- DOWNLOAD_DATE
data_output_dir <- DATA_OUTPUT_DIR
synth_dir <- SYNTH_DIR

run_time <- as.character(Sys.time()) %>%
  str_replace_all(., ' ', '_') %>%
  str_replace_all(., ':', '-')

# read study design -------------------------------------------------------
message('==> 1/13 Reading study design')

args <- commandArgs(trailingOnly = TRUE)
names(args) <- c('design_id',
                 'aggregation_level',
                 'bin_size',
                 'bandwidth_method',
                 'donor_state_criteria',
                 'special_criteria',
                 'pretreatment_fit_predictors')

## 1) set design id and folder
design_id <- paste0(args[['design_id']], '_', run_time)
result_dir <- file.path(synth_dir, design_id)
if (!dir.exists(result_dir)){
  dir.create(result_dir)
}

## 2) set bin size
aggregation_level <- args[['aggregation_level']]
bin_size <- args[['bin_size']]

## 3) set bandwidth_method
bandwidth_method <- args[['bandwidth_method']]

## 4) set donor state criteria
donor_state_criteria <- args[['donor_state_criteria']]

## 5) set special criteria for donor state selection if provided
special_criteria <- args[['special_criteria']]

## 6) set pretreatment fit predictors
pretreatment_fit_predictors <- args[['pretreatment_fit_predictors']]

###### set for testing
design_id <- paste0('predictor_outcome_reporting_', run_time)
# design_id <- 'main'
#design_id <- paste0('loot_none_', run_time)
#design_id <- 'predictor_main_2020-11-12_10-13-00'
result_dir <- file.path(synth_dir, design_id)
if (!dir.exists(result_dir)){
  dir.create(result_dir)
}
aggregation_level <- 'weighted_permit_yearly'
bin_size <- 1
bandwidth_method <- 'manual_1996_2003'
donor_state_criteria <- 'complete_data'
special_criteria <- 'remove_territories'
pretreatment_fit_predictors <- 'outcome_with_reporting' #'pretreatment_outcome_with_diffs_and_state_characteristics'

intervention <- 2000

# read data ---------------------------------------------------------------
message('==> 2/13 Reading violation rate data')

# 1) choose data based on aggregation level
if (aggregation_level == 'yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_monitoring_year.csv')) %>%
    mutate(time = monitoring_year) 
} else if (aggregation_level == 'early_yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_early_reporter.csv')) %>%
    mutate(time = monitoring_year)
} else if (aggregation_level == 'serious_mmp_yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_serious_mmp.csv')) %>%
    mutate(time = monitoring_year) %>%
    filter(serious_mmp_flag %in% c(NA, TRUE))
} else if (aggregation_level == 'non_serious_mmp_yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_serious_mmp.csv')) %>%
    mutate(time = monitoring_year) %>%
    filter(serious_mmp_flag %in% c(NA, FALSE))
} else if (aggregation_level == 'matched_permit_yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_matched_permits.csv')) %>%
    mutate(time = monitoring_year)
} else if (aggregation_level == 'weighted_permit_yearly'){
  data <- read_csv(file.path(data_output_dir, 'violation_rate_weighted_permits.csv')) %>%
    mutate(time = monitoring_year)
} else {
  message('Warning: invalid aggregation level input, choose from monthly, quarterly, or yearly\n
          Using weighted permit yearly by default.')
  data <- read_csv(file.path(data_output_dir, 'violation_rate_weighted_permits.csv'))%>%
    mutate(time = monitoring_year) 
}

data <- data %>%
  mutate(post_intervention = time >= intervention) %>%
  arrange(state_code, time) %>%
  group_by(state_code) %>%
  mutate(violation_rate_lag = violation_rate - lag(violation_rate),
         violation_rate_lag_pct = violation_rate_lag/lag(violation_rate)) %>%
  filter(!(is.na(violation_rate_lag) | is.infinite(violation_rate_lag_pct) | is.na(violation_rate_lag_pct)))

california_data <- data %>%
  filter(state_code == 'CA')

# 2) state characteristics
enf_insp <- read_csv(file.path(data_output_dir, 'state_enf_insp_rates.csv')) %>%
  select(monitoring_year, state_code, enforcement_rate, avg_penalty, inspection_rate)
facility_ratio <- read_csv(file.path(data_output_dir, 'state_characteristics_weighted_permits.csv'))
state_features <- facility_ratio %>%
  left_join(enf_insp) %>%
  mutate_all(replace_na, 0) %>%
  rename(time = monitoring_year) %>%
  select(time, 
         state_code, 
         major_ratio, 
         potw_ratio, 
         sewerage_ratio, 
         enforcement_rate, 
         avg_penalty, 
         inspection_rate)

# # if state characteristics are used: read state characteristics data
# if (grepl('raw_ratio', design_id, fixed = TRUE)){
#   state_characteristics <- read_csv(file.path(data_output_dir, 'state_characteristics.csv'))
#   synth_data <- synth_data %>%
#     left_join(state_characteristics[, c('monitoring_year', 'state_code', 'major_ratio', 'individual_ratio', 'private_ratio', 'sewerage_ratio')])
# } else if (grepl('state_characteristics', pretreatment_fit_predictors, fixed = TRUE) & !(aggregation_level %in% c('matched_permit_yearly', 'weighted_permit_yearly'))){
#   state_characteristics <- read_csv(file.path(data_output_dir, 'state_characteristics.csv'))
#   synth_data <- synth_data %>%
#     left_join(state_characteristics[, c('monitoring_year', 'state_code', 'major_ratio', 'individual_ratio', 'private_ratio', 'sewerage_ratio')])
# } else if (grepl('state_characteristics', pretreatment_fit_predictors, fixed = TRUE) & aggregation_level == 'matched_permit_yearly'){
#   state_characteristics <- read_csv(file.path(data_output_dir, 'state_characteristics_matched_permits.csv'))
#   synth_data <- synth_data %>%
#     left_join(state_characteristics[, c('monitoring_year', 'state_code', 'major_ratio', 'private_ratio', 'sewerage_ratio')])
# } else if (grepl('state_characteristics', pretreatment_fit_predictors, fixed = TRUE) & aggregation_level == 'weighted_permit_yearly'){
#   state_characteristics <- read_csv(file.path(data_output_dir, 'state_characteristics_weighted_permits.csv'))
#   synth_data <- synth_data %>%
#     left_join(state_characteristics[, c('monitoring_year', 'state_code', 'major_ratio', 'private_ratio', 'sewerage_ratio')])
# } else if (grepl('enforcement_inspection', pretreatment_fit_predictors, fixed = TRUE)) {
#   enf_insp <- read_csv(file.path(data_output_dir, 'state_enf_insp_rates.csv')) %>%
#     select(monitoring_year, state_code, enforcement_rate, avg_penalty, inspection_rate)
#   synth_data <- synth_data %>%
#     left_join(enf_insp)
# }

# 3) annual dmr records
dmr_reporter_count <- read_csv(file.path(data_output_dir, 'violation_rate_monitoring_year.csv')) %>%
  select(monitoring_year, state_code, dmr_reporter_count) %>%
  rename(time = monitoring_year)

# Analysis ----------------------------------------------------------------

# 1) Plot violation rate and dmr reporter count by state ------------------
message('==> 3/13 Ploting violation rate and dmr reporter count by state')

## a) dmr reporter count
ggplot(data, aes(x = time, y = dmr_reporter_count)) +
  geom_line() +
  geom_vline(xintercept = intervention, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Period',
       y = 'Permit Count',
       title = paste0('Count of DMR Reporter Overtime By State: ', aggregation_level)) + 
  facet_wrap(~state_code, scales = 'free') +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(result_dir, 'dmr_reporter_count_by_state.pdf'),
       width = 15, height = 10)

## b) violation rate
ggplot(data, aes(x = time, y = violation_rate)) +
  geom_line() +
  ylim(0,1) +
  geom_vline(xintercept = intervention, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Period',
       y = 'Proportion of Permit with at least One Effluent Violation',
       title = paste0('Violation Rate Overtime By State: ', aggregation_level)) +
  facet_wrap(~state_code, scales = 'free') +
  theme_classic()
ggsave(file.path(result_dir, 'violation_rate_by_state.pdf'),
       width = 15, height = 10)

# 2) Determine Bandwidth based on California data--------------------------------------------------
message('==> 4/13 Determining bandwidth')

if (bandwidth_method == 'rdbwselect'){
  out <- rdbwselect(y = california_data$violation_rate,
                    x = california_data$time,
                    c = intervention,
                    bwselect = 'mserd')
  h <- floor(out$bws[1])
  bandwidth_pre <- h
  bandwidth_post <- h
} else if (bandwidth_method == 'manual_1996_2004'){
  # based on data quality in pre-2000 period and contextual knowledge about MMP post-2000
  bandwidth_pre <- 4
  bandwidth_post <- 4
} else if (bandwidth_method == 'manual_1996_2003'){
  bandwidth_pre <- 4
  bandwidth_post <- 3
} else if (bandwidth_method == 'manual_1989_2003'){
  bandwidth_pre <- 11 
  bandwidth_post <- 3
} else if (bandwidth_method == 'manual_1989_2004'){
  bandwidth_pre <- 11
  bandwidth_post <- 4
} else if (bandwidth_method == 'manual_1997_2003'){
  bandwidth_pre <- 3
  bandwidth_post <- 3
} else if (bandwidth_method == 'manual_1996_1999'){
  bandwidth_pre <- 3
  bandwidth_post <- 0
}

window <- seq(intervention - bandwidth_pre,
              intervention + bandwidth_post,
              1)

# 3) Compare California vs the average of the rest of US ------------------
message('==> 5/13 Ploting California vs the average of the rest of US')

plot_df <- data %>%
  filter(time %in% window) %>%
  mutate(rest_of_us = state_code != 'CA') %>%
  group_by(rest_of_us, time) %>%
  summarise(avg_violation_rate = mean(violation_rate, na.rm = TRUE))

ggplot(plot_df, aes(x = time, y = avg_violation_rate, group = rest_of_us, linetype = rest_of_us)) +
  geom_line() +
  geom_vline(xintercept = intervention,
             linetype = 'dashed',
             color = '#F8766D') +
  annotate('text',
           x = 1998.7,
           y = 0.9,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Period',
       y = 'Proportion of Permit with At Least One Effluent Violation',
       title = 'Effluent Violation Rate Trend: California vs. Rest of US') +
  scale_linetype_discrete(name = '',
                          labels = c('California',
                                     'Rest of US')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(result_dir, 'ca_us_violation_rate.pdf'),
       width = 6, height = 4)

# 4) Select Donor States ---------------------------------------------------
message('==> 6/13 Selecting donor states')

## a) all donor states should have data within the observation window
leftout_window <- data %>%
  filter(time %in% window) %>%
  group_by(state_code, time) %>%
  summarise(n = n()) %>%
  group_by(state_code) %>%
  summarise(time_count = n()) %>%
  filter(time_count != length(window))
write_csv(leftout_window, file.path(result_dir, 'leftout_state_window.csv'))

### b) manually remove GM and GE
# because ICIS violation records doesn't seem to contain GM and GE data
leftout_manual <- c('GM', 'GE', 'NN', 'SR', 'MW')

### c) option to remove non-state territories and leave one state out
if (special_criteria == 'remove_territories'){
  leftout_manual <-  c(leftout_manual, 'AS', 'GU', 'MP', 'PR', 'VI', 'UM')
} else if (special_criteria == 'remove_territories_and_NH'){
  leftout_manual <- c(leftout_manual, 'AS', 'GU', 'MP', 'PR', 'VI', 'UM', 'NH')
} else if (special_criteria == 'remove_territories_and_OK'){
  leftout_manual <- c(leftout_manual, 'AS', 'GU', 'MP', 'PR', 'VI', 'UM', 'OK')
} else if (special_criteria == 'remove_territories_and_RI'){
  leftout_manual <- c(leftout_manual, 'AS', 'GU', 'MP', 'PR', 'VI', 'UM', 'RI')
} else if (special_criteria == 'remove_territories_and_UT'){
  leftout_manual <- c(leftout_manual, 'AS', 'GU', 'MP', 'PR', 'VI', 'UM', 'UT')
}

### d1) consistent reporting around 2000
#### slope of increase at any time point within the window < 2 (no doubling over a year)
remove_nonconsistent_reporting <- function(df, column_name){
  reporting_change <- df %>%
    filter(time %in% window) %>%
    arrange(state_code, time) %>%
    mutate(diff = lead(dmr_reporter_count) - dmr_reporter_count) %>%
    mutate(pct_diff = diff/dmr_reporter_count)
  leftout_reporting <- reporting_change %>%
    group_by(state_code) %>%
    summarise(doubling_count = sum(pct_diff > 2, na.rm = TRUE)) %>%
    filter(doubling_count > 0) 
  leftout_reporting[,column_name] <- leftout_reporting$doubling_count
  leftout_reporting <- leftout_reporting %>%
    select(-doubling_count)
  
  return(leftout_reporting)
}

# apply non-consistent reporting criteria before weighted violation rates
leftout_reporting_both <- remove_nonconsistent_reporting(dmr_reporter_count, 'before_doubling_count') %>%
  full_join(remove_nonconsistent_reporting(data, 'after_doubling_count'))

write_csv(leftout_reporting_both, file.path(result_dir, 'leftout_state_repoorting_change.csv'))

### d2) consistent reporting around 2000
#### minimum number of DMR reporters
# remove states that have less than 20 permits in 2000 for now: 20 is an arbiturary number
leftout_permit_before <- dmr_reporter_count %>%
  filter(time == intervention & dmr_reporter_count < 20) %>%
  select(state_code, dmr_reporter_count) %>%
  rename(before_dmr_reporter_count = dmr_reporter_count)
leftout_permit_after <- data %>%
  filter(time == intervention & dmr_reporter_count < 20) %>%
  select(state_code, dmr_reporter_count) %>%
  rename(after_dmr_reporter_count = dmr_reporter_count)
leftout_permit_both <- leftout_permit_before %>%
  full_join(leftout_permit_after)
write_csv(leftout_permit_both, file.path(result_dir, 'leftout_state_minimum_reporter.csv'))

## apply criteria based on model design
if (donor_state_criteria == 'complete_data'){
  leftout <- c(leftout_window$state_code, 
               leftout_manual) %>%
    unique()
} else if (donor_state_criteria == 'consistent_reporting'){
  leftout <- c(leftout_window$state_code, 
               leftout_reporting_both$state_code, 
               leftout_permit_both$state_code,
               leftout_manual) %>%
    unique()
} else if (donor_state_criteria == 'no_doubling_reporting'){
  leftout <- c(leftout_window$state_code, 
               leftout_reporting_both$state_code, 
               leftout_manual) %>%
    unique()
} else if (donor_state_criteria == 'minimum_reporting'){
  leftout <- c(leftout_window$state_code, 
               leftout_permit_both$state_code,
               leftout_manual) %>%
    unique()
}

## making sure CA is not in part of the leftout states
if ('CA' %in% leftout){
  message('Warning: CA is part of the leftout states. Force add CA back to the synthetic control data. Please evaluate criteria.')
  leftout <- leftout[leftout != 'CA']
}

## generate synthetic data based on donor pool
synth_data <- data %>%
  ungroup(state_code) %>%
  filter(time %in% window) %>%
  filter(!(state_code %in% leftout)) %>%
  # create numeric unit id
  mutate(unit_id = as.factor(state_code)) %>%
  mutate(unit_id = as.numeric(unit_id)) %>%
  data.frame()

synth_data <- synth_data %>%
  left_join(state_features) 

write_csv(synth_data, file.path(result_dir, 'synthetic_control_data.csv'))

# # check state characteristics plot 
# plot_df <- synth_data %>%
#   mutate(ca = state_code == 'CA') %>%
#   gather(type, ratio, 
#          # enforcement_rate, 
#          # inspection_rate, 
#          major_ratio, 
#          potw_ratio, 
#          sewerage_ratio
#          )
#   
# ggplot(plot_df, aes(x = time, y = ratio, color = ca, group = state_code)) +
#   geom_line() + 
#   scale_color_manual(values = alpha(c('grey70', 'red'), c(0.5,1))) +
#   labs(x = 'Monitoring Year (1996-2000)', y = '%',
#        title = 'California (red) vs 24 Control States') + 
#   facet_wrap(~type) + 
#   theme_bw() +
#   theme(legend.position = 'none')


# enforcement_rate: try before 2000
# inspection_rate: california has always been an outlier
# major_ratio: 1996 closer to mean
# potw_ratio: 1996 closer to the mean
# sewerage_ratio: 1996 closer to the mean

# 5) Select pretreatment fitting predictors -------------------------------
message('==> 7/13 Selecting pretreatment fitting predictors')

if (pretreatment_fit_predictors == 'pretreatment_outcome_with_state_characteristics' & (bandwidth_pre == 4 | bandwidth_method == 'rdbwselect')){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_lags_with_state_characteristics' & (bandwidth_pre == 4 | bandwidth_method == 'rdbwselect')){
  predictors <- list(
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_and_lags_with_state_characteristics' & (bandwidth_pre == 4 | bandwidth_method == 'rdbwselect')){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 2000, 'mean'),
    # list('violation_rate', 1997, 'mean'),
    # list('violation_rate', 1998, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    #list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('major_ratio', 1999, 'mean'),
    list('private_ratio', 1999, 'mean'),
    list('sewerage_ratio', 1999, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_state_characteristics' & bandwidth_pre == 11){
  predictors <- list(
    list('violation_rate', 1989, 'mean'),
    list('violation_rate', 1990, 'mean'),
    list('violation_rate', 1991, 'mean'),
    list('violation_rate', 1992, 'mean'),
    list('violation_rate', 1993, 'mean'),
    list('violation_rate', 1994, 'mean'),
    list('violation_rate', 1995, 'mean'),
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_state_characteristics' & bandwidth_pre == 3){
  predictors <- list(
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_reporter_count' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('dmr_reporter_count', 2000, 'mean'),
    list('dmr_reporter_count', 1998, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_3_years_with_state_characteristics' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_2_years_with_state_characteristics' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_average_with_state_characteristics' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1996:1998, 'mean'),
    list('violation_rate', 1998:2000, 'mean'),
    list('major_ratio', 2000, 'mean'),
    list('private_ratio', 2000, 'mean'),
    list('sewerage_ratio', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_and_lags_with_enforcement_inspection' & bandwidth_pre == 4){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 2000, 'mean'),
    # list('violation_rate', 1997, 'mean'),
    # list('violation_rate', 1998, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    #list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('enforcement_rate', 1996, 'mean'),
    list('enforcement_rate', 2000, 'mean'),
    list('avg_penalty', 2000, 'mean'),
    list('inspection_rate', 1996, 'mean'),
    list('inspection_rate', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_diffs_and_state_characteristics' & intervention == 2000){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('violation_rate', 2000, 'mean'),
    #list('enforcement_rate', 1996:2000, 'mean'),
    #list('avg_penalty', 2000, 'mean'),
    list('inspection_rate', 1996:2000, 'mean'),
    list('major_ratio', 1996:2000, 'mean'),
    list('potw_ratio', 1996:2000, 'mean'),
    list('sewerage_ratio', 1996:2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_diffs' & intervention == 2000){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('violation_rate', 2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_and_state_characteristics' & intervention == 2000){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate', 1997, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('violation_rate', 2000, 'mean'),
    list('inspection_rate', 1996:2000, 'mean'),
    list('major_ratio', 1996:2000, 'mean'),
    list('potw_ratio', 1996:2000, 'mean'),
    list('sewerage_ratio', 1996:2000, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_diffs_and_state_characteristics' & intervention == 1999){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('major_ratio', 1996:1999, 'mean'),
    list('potw_ratio', 1996:1999, 'mean'),
    list('sewerage_ratio', 1996:1999, 'mean')
  )
} else if (pretreatment_fit_predictors == 'pretreatment_outcome_with_diffs_and_state_characteristics' & intervention == 2001){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate_lag', 2000, 'mean'),
    list('violation_rate_lag', 2001, 'mean'),
    list('violation_rate', 2001, 'mean'),
    list('major_ratio', 1996:2001, 'mean'),
    list('potw_ratio', 1996:2001, 'mean'),
    list('sewerage_ratio', 1996:2001, 'mean')
  )
} else if (pretreatment_fit_predictors == '2000_as_post_treatment' & intervention == 2000){
    predictors <- list(
      list('violation_rate', 1996, 'mean'),
      list('violation_rate_lag', 1997, 'mean'),
      list('violation_rate_lag', 1998, 'mean'),
      list('violation_rate_lag', 1999, 'mean'),
      list('violation_rate', 1999, 'mean'),
      list('inspection_rate', 1996:1999, 'mean'),
      list('major_ratio', 1996:1999, 'mean'),
      list('potw_ratio', 1996:1999, 'mean'),
      list('sewerage_ratio', 1996:1999, 'mean')
  )
} else if (pretreatment_fit_predictors == '2000_as_post_treatment' & intervention == 1999){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate', 1998, 'mean'),
    list('inspection_rate', 1996:1998, 'mean'),
    list('major_ratio', 1996:1998, 'mean'),
    list('potw_ratio', 1996:1998, 'mean'),
    list('sewerage_ratio', 1996:1998, 'mean')
  )
} else if (pretreatment_fit_predictors == 'outcome_with_reporting' & intervention == 2000){
  predictors <- list(
    list('violation_rate', 1996, 'mean'),
    list('violation_rate_lag', 1997, 'mean'),
    list('violation_rate_lag', 1998, 'mean'),
    list('violation_rate_lag', 1999, 'mean'),
    list('violation_rate', 1999, 'mean'),
    list('dmr_reporter_count', 1996, 'mean'),
    list('dmr_reporter_count', 1997, 'mean'),
    list('dmr_reporter_count', 1998, 'mean'),
    list('dmr_reporter_count', 1999, 'mean'),
    list('inspection_rate', 1996:1999, 'mean'),
    list('major_ratio', 1996:1999, 'mean'),
    list('potw_ratio', 1996:1999, 'mean'),
    list('sewerage_ratio', 1996:1999, 'mean')
  )
}

# 6) Fitting pre-intervention trend ---------------------------------------
message('==> 8/13 Selecting pre-intervention trend')

run_synthetic_control <- function(synth_data, treatment_unit){
  print(treatment_unit)
  
  units <- unique(synth_data$unit_id)
  control_units <- units[units != treatment_unit]
  
  synth_data_prep <- dataprep(foo = synth_data,
                              special.predictors = predictors,
                              time.predictors.prior = min(synth_data$time):intervention,
                              dependent = 'violation_rate',
                              unit.variable = 'unit_id',
                              unit.names.variable = 'state_code',
                              time.variable = 'time',
                              treatment.identifier = treatment_unit,
                              controls.identifier = control_units,
                              time.optimize.ssr = min(synth_data$time):intervention,
                              time.plot = min(synth_data$time):max(synth_data$time))
  
  # # inverse variances as predictor weights
  # inverse_variance <- sapply(seq(1:length(predictors)), function(x){
  #   1/var(c(synth_data_prep$X0[x,], synth_data_prep$X1[x,]))
  # })
  # inverse_variance <- inverse_variance/sum(inverse_variance)
  
  # minimize loss functions with both outcome variables and covariates (this could be done by changing the Synth and fn.V functions)
  
  synth_out <- synth(synth_data_prep,
                     #custom.v = rep(1/length(predictors), length(predictors)), # uniform weights
                     #custom.v = c(rep(0.192, 5), rep(0.01, 4)), # manual weights
                     #custom.v = inverse_variance, # inverse variance as weights
                     verbose = TRUE)
  synth_tables <- synth.tab(dataprep.res = synth_data_prep,
                            synth.res = synth_out)
  results <- list(synth_data_prep, synth_out, synth_tables)
  names(results) <- c('data_prep', 'output', 'tables')
  
  # for debugging
  #print(treatment_unit)
  return(results)
}

treatment_unit <- synth_data$unit_id[synth_data$state_code == 'CA'] %>%
  unique()

# debug exploding weights
# control_units_parent <- unique(synth_data$unit_id)[-treatment_unit]
# 
# test <- lapply(control_units_parent, function(x){
#   new_data <- synth_data %>%
#     filter(unit_id != x)
#   
#   tryCatch({
#     results <- run_synthetic_control(new_data, treatment_unit)
#     results <- x
#   }, error = function(e){
#     NULL
#   })
# })
# 
# tmp <- synth_data %>%
#   filter(unit_id %in% c(27, 32, 35, 1 , 2, 4, 5)) 
# 
# test_matrix <- matrix(tmp[tmp$time == 1996, ]$violation_rate) %>%
#   cbind(tmp[tmp$time == 2000, ]$violation_rate) %>%
#   cbind(tmp[tmp$time == 1997, ]$violation_rate_lag) %>%
#   cbind(tmp[tmp$time == 1999, ]$violation_rate_lag) %>%
#   cbind(tmp[tmp$time == 2000, ]$major_ratio) %>%
#   cbind(tmp[tmp$time == 2000, ]$private_ratio) %>%
#   cbind(tmp[tmp$time == 2000, ]$sewerage_ratio) %>%
#   t()
# test_matrix
# rankMatrix(test_matrix)
# f <- function(m) class(try(solve(m),silent=T))=="matrix"
# f(test_matrix)
# 
# # list('violation_rate', 1996, 'mean'),
# # list('violation_rate', 2000, 'mean'),
# # list('violation_rate_lag', 1997, 'mean'),
# # #list('violation_rate_lag', 1998, 'mean'),
# # list('violation_rate_lag', 1999, 'mean'),
# # #list('violation_rate_lag', 2000, 'mean'),
# # list('major_ratio', 2000, 'mean'),
# # list('private_ratio', 2000, 'mean'),
# # list('sewerage_ratio', 2000, 'mean')
# 
# ggplot(tmp, aes(x = time, y = violation_rate)) +
#   geom_line() +
#   facet_wrap(~state_code)
# 
# synth_data <- synth_data %>%
#   filter(unit_id != 35)

results <- run_synthetic_control(synth_data, treatment_unit)

## saving results
saveRDS(results, file.path(result_dir, 'synthetic_control_results.rds'))

# use write.csv instead of write_csv to keep row names
write.csv(data.frame(results$tables$tab.pred),
          file.path(result_dir, 'synthetic_control_balance.csv'))
write.csv(data.frame(results$tables$tab.w),
          file.path(result_dir, 'synthetic_control_state_weights.csv'))
## for predictor weights table: change format for csv output
tmp <- results$tables$tab.v
write.csv(data.frame(predictor = row.names(tmp),
                     v.weights = unlist(c(tmp))),
          file.path(result_dir, 'synthetic_control_predictor_weights.csv'))

# 7) Plot violation rate by donor states ----------------------------------
message('==> 9/13 Plotting violation rates by states in the donor pool')

plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, NA)) %>%
  #mutate(w.weights = round(w.weights, 2)) %>%
  mutate(zero_weight_state = w.weights == 0) %>%
  mutate(label = paste0(state_code, ': ', w.weights))

nonzero_count <- length(unique(plot_df[plot_df$w.weights > 0,]$state_code))
control_count <- length(unique(plot_df$state_code)) - 1 # remove CA
nonzero_ratio <- round(mean(plot_df$w.weights > 0, na.rm = TRUE)*100)

ggplot(plot_df, aes(x = time, y = violation_rate, color = zero_weight_state)) +
  geom_line() +
  geom_point(aes(size = dmr_reporter_count)) +
  geom_vline(xintercept = intervention, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Period',
       y = 'Proportion of Permit with At Least One Effluent Violation',
       title = 'Violation Rate by Donor Pool States',
       subtitle = paste0('Non-zero Weight States Count: ', nonzero_count, ' out of ', control_count, ' control states (', nonzero_ratio, '%)',ifelse(nonzero_ratio > 50, ' - Risk of Overfitting', ''))) +
  facet_wrap(~label) +
  scale_size_continuous(name = 'DMR Reporter Count') +
  scale_color_discrete(name = '',
                       labels = c('Non-Zero Weight State',
                                  'Zero Weight State')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(result_dir, 'donor_state_violation_rate.pdf'),
       width = 12, height = 7)

# 8) Plot treatment and synthetic control comparison ----------------------
message('==> 10/13 Plotting california and synthetic california comparison')

## a) time series trend
plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(control = state_code != 'CA') %>%
  group_by(control, time) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE))
ggplot(plot_df, aes(x = time, y = avg_violation_rate, group = control, linetype = control)) +
  geom_line() +
  geom_vline(xintercept = intervention,
             linetype = 'dashed',
             color = '#F8766D') +
  annotate('text',
           x = 1998.7,
           y = 0.9,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Period',
       y = 'Proportion of Permit with At Least One Effluent Violation',
       title = 'Effluent Violation Rate Trend: California vs. Synthetic California') +
  scale_linetype_discrete(name = '',
                          labels = c('California',
                                     'Synthetic')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(result_dir, 'ca_synthetic_violation_rate.pdf'),
       width = 6, height = 4)

## a) gaps between california and synthetic california

plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(control = state_code != 'CA') %>%
  group_by(control, time) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
  spread(control, avg_violation_rate) %>%
  mutate(gap = `FALSE` -`TRUE`)

ggplot(plot_df, aes(x = time, y = gap)) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 'dotted') +
  geom_vline(xintercept = intervention,
             linetype = 'dashed',
             color = '#F8766D') +
  annotate('text',
           x = 1998.7,
           y = 0.15,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  ylim(-0.4, 0.4) +
  labs(x = 'Monitoring Period',
       y = 'Gap in Proportion of Permit with At Least One Effluent Violation',
       title = 'Effluent Violation Rate Gap: California vs. Synthetic California') +
  theme_classic()
ggsave(file.path(result_dir, 'ca_synthetic_gap.pdf'),
       width = 6, height = 4)

# 9) Permutation test -----------------------------------------------------
message('==> 11/13 Running permutation test')

## a) run synthetic control for each of states in the donor pool
treatment_units <- unique(synth_data$unit_id)

permutation_test <- lapply(treatment_units, function(x){
  tryCatch({
    run_synthetic_control(synth_data, x)
  }, error = function(e){
    NULL
  })
})

errors <- sapply(treatment_units, function(x){
  length(permutation_test[[x]]) == 0
})
message(paste0(sum(errors), ' errors: unit id ', treatment_units[errors]))

names(permutation_test) <- treatment_units

saveRDS(permutation_test, file.path(result_dir, 'permutation_test.rds'))

## b) generate pretreatment loss for each state=

pretreatment_loss <- lapply(as.character(treatment_units), function(i) permutation_test[[i]]$output$loss.w) %>% 
  do.call(rbind, .) %>%
  data.frame()
treatment_units <- treatment_units[!errors]
pretreatment_loss$unit_id <- treatment_units
pretreatment_loss <- pretreatment_loss %>%
  left_join(synth_data[, c('state_code', 'unit_id')]) %>%
  distinct()

saveRDS(pretreatment_loss, file.path(result_dir, 'pretreatment_loss.rds'))

# 10) Formalize and output effect and significance ------------------------------------
message('==> 12/13 Calculating and outputing effect and significance results')

effect_results <- data.frame(design_id = design_id,
                             aggregation_level = aggregation_level,
                             bin_size = bin_size,
                             bandwidth_method = bandwidth_method,
                             window_start = min(window),
                             window_end = max(window),
                             donor_state_criteria = donor_state_criteria,
                             special_criteria = special_criteria,
                             donor_states = paste(unique(synth_data$state_code[synth_data$state_code != 'CA']), collapse = ','),
                             donor_state_count = length(unique(synth_data$state_code[synth_data$state_code != 'CA'])),
                             pretreatment_fit_predictors = pretreatment_fit_predictors,
                             pretreatment_fit_predictor_count = length(predictors))

## a) non zero weight states
tmp <- results$tables$tab.w %>%
  filter(w.weights != 0) %>%
  arrange(-w.weights)
effect_results$nonzero_weight_states <- paste(tmp$unit.names, collapse = ',')
effect_results$nonzero_weight_states_count <- length(tmp$unit.names)
effect_results$weights <- paste(as.character(tmp$w.weights), collapse = ',')
effect_results$nonzero_ratio <- length(tmp$unit.names)/effect_results$donor_state_count

## b) pretreatment RMS
effect_results$pretreatment_loss_w <- results$output$loss.w
effect_results$pretreatment_loss_v <- results$output$loss.v

## c) effects: 
### i) by each time point
# tmp <- synth_data %>%
#   left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
#   mutate(w.weights = replace_na(w.weights, 1)) %>%
#   filter(w.weights > 0) %>%
#   mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
#   mutate(california = state_code == 'CA') %>%
#   group_by(california, post_intervention, time) %>%
#   summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
#   spread(california, avg_violation_rate) %>%
#   mutate(effect = `TRUE` - `FALSE`) %>%
#   filter(post_intervention) %>%
#   select(time, effect) %>%
#   spread(time, effect)

### ii) by averages: difference in differences

tmp <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(california = state_code == 'CA') %>%
  group_by(california, post_intervention, time) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
  group_by(post_intervention, california) %>%
  summarise(avg_violation_rate = mean(avg_violation_rate))

effect_results$effect_did <- (tmp$avg_violation_rate[tmp$california == TRUE & tmp$post_intervention == TRUE] - tmp$avg_violation_rate[tmp$california == FALSE & tmp$post_intervention == TRUE]) -
  (tmp$avg_violation_rate[tmp$california == TRUE & tmp$post_intervention == FALSE] - tmp$avg_violation_rate[tmp$california == FALSE & tmp$post_intervention == FALSE])

## d) p-value: in-sample exact test

### i) base on effects
make_synth_effect_table <- function(synth_data, permutation_test, treatment_unit){
  tmp <- synth_data %>%
    left_join(permutation_test[[treatment_unit]]$tables$tab.w[, c('w.weights', 'unit.names')],
              by = c('state_code' = 'unit.names')) %>%
    mutate(w.weights = replace_na(w.weights, 1)) %>%
    filter(w.weights > 0) %>%
    mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
    mutate(control = unit_id != treatment_unit) %>%
    mutate(post_intervention = monitoring_year >= intervention) 
  
  effect_df <- tmp %>%
    group_by(post_intervention, control, monitoring_year) %>%
    summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
    group_by(post_intervention, control) %>%
    summarise(avg_violation_rate = mean(avg_violation_rate))
  effect <- (effect_df$avg_violation_rate[effect_df$control == FALSE & effect_df$post_intervention == TRUE] - effect_df$avg_violation_rate[effect_df$control == TRUE & effect_df$post_intervention == TRUE]) -
    (effect_df$avg_violation_rate[effect_df$control == FALSE & effect_df$post_intervention == FALSE] - effect_df$avg_violation_rate[effect_df$control == TRUE & effect_df$post_intervention == FALSE])
  
  mspe_df <- tmp %>%
    group_by(control, post_intervention, time) %>%
    summarise(adjusted_violation_rate = sum(adjusted_violation_rate)) %>%
    spread(control, adjusted_violation_rate) %>%
    mutate(squared_error = (`TRUE` - `FALSE`)^2) %>%
    group_by(post_intervention) %>%
    summarise(mspe = mean(squared_error))
  mspe_ratio <- mspe_df$mspe[mspe_df$post_intervention]/mspe_df$mspe[!mspe_df$post_intervention]
  
  df <- data.frame(unit_id = treatment_unit,
                   effect = effect,
                   pre_mspe = mspe_df$mspe[!mspe_df$post_intervention],
                   post_mspe = mspe_df$mspe[mspe_df$post_intervention],
                   mspe_ratio = mspe_ratio)
  df
}

treatment_units <- as.character(treatment_units)

permutation_test_effects <- lapply(treatment_units, make_synth_effect_table,
                                   synth_data = synth_data,
                                   permutation_test = permutation_test) %>%
  bind_rows() %>%
  mutate(unit_id = as.numeric(unit_id)) %>%
  left_join(synth_data[, c('unit_id', 'state_code')])

## remove states whose pretreatment loss is larger than 5, 10, 20 times California
return_control_states <- function(pretreatment_loss, scale){
  if (scale == 'all'){
    states <- pretreatment_loss$state_code
  } else{
    rms_threshold <- pretreatment_loss$w.weight[pretreatment_loss$state_code == 'CA'] * scale
    states <- pretreatment_loss$state_code[pretreatment_loss$w.weight <= rms_threshold]
  }
  return(states)
}

approximate_p_value <- function(permutation_test_effects, kept_states){
  
  tmp <- permutation_test_effects %>%
    filter(state_code %in% kept_states) %>%
    mutate(larger_effect = effect <= effect_results$effect_did)
  
  sum(tmp$larger_effect)/nrow(tmp)
}
### a) all
kept_states <- return_control_states(pretreatment_loss, 'all')
effect_results$p_mspe_all <- approximate_p_value(permutation_test_effects, kept_states)
effect_results$control_state_count_mspe_all <- length(kept_states)
### b) 20 times
kept_states <- return_control_states(pretreatment_loss, 20)
effect_results$p_mspe_threshold_20 <- approximate_p_value(permutation_test_effects, kept_states)
effect_results$control_state_count_mspe_threshold_20 <- length(kept_states)
### c) 10 times 
kept_states <- return_control_states(pretreatment_loss, 10)
effect_results$p_mspe_threshold_10 <- approximate_p_value(permutation_test_effects, kept_states)
effect_results$control_state_count_mspe_threshold_10 <- length(kept_states)
### d) 5 times
kept_states <- return_control_states(pretreatment_loss, 5)
effect_results$p_mspe_threshold_5 <- approximate_p_value(permutation_test_effects, kept_states)
effect_results$control_state_count_mspe_threshold_5 <- length(kept_states)

write_csv(effect_results, file.path(result_dir, 'main_results.csv'))


# 11) plot gaps distribution ----------------------------------------------
message('==> 13/13 Plot permutation test distributions')

make_synth_gap_table <- function(permutation_test, treatment_unit){
  synth_data %>%
    mutate(unit_id = as.character(unit_id)) %>%
    left_join(permutation_test[[treatment_unit]]$tables$tab.w[, c('w.weights', 'unit.names')],
              by = c('state_code' = 'unit.names')) %>%
    mutate(w.weights = replace_na(w.weights, 1)) %>%
    filter(w.weights > 0) %>%
    mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
    mutate(control = unit_id != treatment_unit) %>%
    group_by(control, time) %>%
    summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
    spread(control, avg_violation_rate) %>%
    mutate(gap = `FALSE` -`TRUE`) %>%
    mutate(treatment_id = treatment_unit)
}

plot_df <- lapply(as.character(treatment_units), make_synth_gap_table, permutation_test = permutation_test) %>%
  do.call(rbind, .) %>%
  mutate(treatment_id = as.numeric(treatment_id)) %>%
  left_join(synth_data[, c('state_code', 'unit_id')], by = c('treatment_id' = 'unit_id')) %>%
  distinct() %>%
  mutate(california = state_code == 'CA')

saveRDS(plot_df, file.path(result_dir, 'permutation_plot_df.rds'))

### plot based on pretreatment balance 

plot_permutation_test <- function(pretreatment_loss, plot_df, threshold_scale, title){
  p <- ifelse(threshold_scale == 'all', effect_results$p_mspe_all,
              ifelse(threshold_scale == 20, effect_results$p_mspe_threshold_20,
                     ifelse(threshold_scale == 10, effect_results$p_mspe_threshold_10, 
                            ifelse(threshold_scale == 5, effect_results$p_mspe_threshold_5, 'NA'))))
  
  kept_states <- return_control_states(pretreatment_loss, threshold_scale)
  
  sub_plot_df <- plot_df %>%
    filter(state_code %in% kept_states)
  
  ggplot() +
    geom_line(data = sub_plot_df[sub_plot_df$state_code != 'CA',], aes(x = time, y = gap, group = treatment_id, color = california)) +
    geom_line(data = sub_plot_df[sub_plot_df$state_code == 'CA',], aes(x = time, y = gap, group = treatment_id, color = california)) +
    geom_hline(yintercept = 0,
               linetype = 'dotted') +
    geom_vline(xintercept = intervention,
               linetype = 'dashed',
               color = '#F8766D') +
    annotate('text',
             x = 1998.7,
             y = 0.15,
             label = 'MMP Started \nFor Effluent Violations \nIn California',
             color = '#F8766D') +
    ylim(-0.4, 0.4) +
    scale_color_manual(name = '',
                       labels = c('Control States', 'California'),
                       values = c('grey80', 'black')) +
    labs(x = 'Monitoring Period',
         y = 'Gap in Proportion of Permit with At Least One Effluent Violation', 
         title = paste0('Permutation Test: California vs. Control States ', title),
         subtitle = paste0('Effect: ', round(effect_results$effect_did, 2), '; ', 
                           'P: ', round(p, 2), '; ', 
                           'Control States Count: ', length(kept_states) - 1 )) + # remove CA from counting control states
    theme_classic() +
    theme(legend.position = 'none',
          plot.subtitle = element_text(color = 'grey40', size = 9),
          plot.title = element_text(size = 11))
  ggsave(file.path(result_dir, paste0('permutation_test_gap_', title,'.pdf')),
         width = 6, height = 4)
}

### graph all donor state s
plot_permutation_test(pretreatment_loss, plot_df, 'all', 'all')

### remove states with MSEP 10 times higher than California's
plot_permutation_test(pretreatment_loss, plot_df, 20, 'MSPE_threshold_20')

### remove states with MSEP 5 times higher than California's
plot_permutation_test(pretreatment_loss, plot_df, 10, 'MSPE_threshold_10')

### remove states with MSEP 2 times higher than California's
plot_permutation_test(pretreatment_loss, plot_df, 5, 'MSPE_threshold_5')

message(paste0('==> Analysis Finished. Output in ', result_dir))

