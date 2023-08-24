
####################
# CA MMP Analysis
## analyze observation windows and donor pool criteria
####################

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse', 'rdrobust', 'rdd')

download_date <- DOWNLOAD_DATE
analysis_dir <- ANALYSIS_DIR
output_dir <- OUTPUT_DIR
figure_dir <- FIGURE_DIR 
data_dir_raw <- DATA_DIR_RAW
data_dir_manual <- DATA_DIR_MANUAL

# read data ---------------------------------------------------------------

# 1) permits 
permits <- read_csv(file.path(data_dir_raw, 'npdes', 'ICIS_PERMITS.csv'), col_types=cols(.default='c')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  filter(PERMIT_TYPE_CODE %in% c('NPD', 'GPC')) %>%
  mutate(permit_type_code = ifelse(PERMIT_TYPE_CODE == 'NPD', 'individual', 'general')) %>%
  mutate(major_minor_flag = ifelse(MAJOR_MINOR_STATUS_FLAG == 'M', 'Major', 'Minor')) %>%
  mutate(EFFECTIVE_DATE = mdy(EFFECTIVE_DATE),
         EXPIRATION_DATE = mdy(EXPIRATION_DATE),
         RETIREMENT_DATE = mdy(RETIREMENT_DATE),
         TERMINATION_DATE = mdy(TERMINATION_DATE)) 

individual_permits <- unique(permits$EXTERNAL_PERMIT_NMBR[permits$permit_type_code == 'individual'])

# 2) violatoin records for all states
violation <- read_csv(file.path(data_dir_raw, 'npdes', 'NPDES_EFF_VIOLATIONS.csv'),
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
                                            PARAMETER_DESC = 'c',
                                            UNIT_CODE = 'c',
                                            ADJUSTED_DMR_STANDARD_UNITS = 'c',
                                            DMR_VALUE_STANDARD_UNITS = 'c',
                                            STATISTICAL_BASE_CODE = 'c'
                      )) %>%
  mutate(EXTERNAL_PERMIT_NMBR = NPDES_ID,
         MONITORING_PERIOD_END_DATE = mdy(MONITORING_PERIOD_END_DATE),
         monitoring_year = year(MONITORING_PERIOD_END_DATE),
         year_quarter = quarter(MONITORING_PERIOD_END_DATE, with_year = TRUE, fiscal_start = 10),
         state_code = substr(NPDES_ID, 1, 2),
         violation_type = ifelse(VIOLATION_CODE %in% c('D80', 'D90'), 'Reporting', 'Effluent')) %>%
  #left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'major_minor_flag', 'permit_type_code', 'FACILITY_TYPE_INDICATOR')]) %>%
  # remove data errors
  filter(MONITORING_PERIOD_END_DATE <= Sys.Date()) %>%
  filter(EXTERNAL_PERMIT_NMBR %in% individual_permits)

# 3) CA DMR: output by output_ca_dmr.R
dmr <- read_csv(file.path(output_dir, paste0('CA_dmrs_', download_date, '.csv')), 
                col_type = cols(.default = 'c')) %>%
  mutate(monitoring_end_date = ymd(MONITORING_PERIOD_END_DATE)) %>%
  mutate(monitoring_year = year(monitoring_end_date)) %>%
  mutate(year_month = paste0(year(monitoring_end_date), 
                             '.', 
                             sprintf("%02d",month(monitoring_end_date)))) %>%
  mutate(year_month = as.numeric(year_month)) %>%
  mutate(parameter_group = as.numeric(SNC_FLAG)) %>%
  mutate(exceedence_pct  = as.numeric(EXCEEDENCE_PCT)) %>%
  mutate(violation_type = ifelse(VIOLATION_CODE == 'E90', 'Effluent', 'Reporting')) %>%
  mutate(monitoring_quarter = as.numeric(monitoring_quarter)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'major_minor_flag', 'FACILITY_TYPE_INDICATOR')]) %>%
  mutate(facility_size = major_minor_flag) %>%
  distinct()

# dmr submission record: output by output_eff_dmr_submitters.R
dmr_record <- read_csv(file.path(output_dir,  paste0('dmr_eff_submission_records_', download_date, '.csv'))) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'major_minor_flag')]) %>%
  distinct() %>%
  mutate(monitoring_period_end_date = mdy(MONITORING_PERIOD_END_DATE)) %>%
  mutate(monitoring_year = year(monitoring_period_end_date)) %>%
  mutate(year_month = paste0(year(monitoring_period_end_date), 
                             '.', 
                             sprintf("%02d",month(monitoring_period_end_date)))) %>%
  mutate(year_month = as.numeric(year_month))


# Analysis ----------------------------------------------------------------

# 1) plot count of dmr reporters over time by state --------------------------------

dmr_reporter_count <- dmr_record %>%
  group_by(state_code, year_quarter, major_minor_flag,  EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(state_code, year_quarter, major_minor_flag) %>%
  summarise(permit_count = n()) 

## subset to states that have data since 1996
tmp <- dmr_reporter_count %>%
  group_by(state_code) %>%
  summarise(earliest_quarter = min(year_quarter, na.rm = TRUE)) %>%
  filter(earliest_quarter < 1996)
early_states <- tmp$state_code
plot_df <- dmr_reporter_count %>%
  filter(state_code %in% early_states)

ggplot(plot_df, aes(x = year_quarter, y = permit_count, group = major_minor_flag, color = major_minor_flag)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Permit Count',
       title = 'Count of DMR Reporter Overtime By State') + 
  facet_wrap(~state_code, scales = 'free') +
  theme_classic() +
  scale_color_discrete(name = 'Facility',
                       labels = c('Major', 'Minor', 'Unknown')) +
  theme(legend.position = 'top')
ggsave(file.path(figure_dir, 'dmr_reporter_count_by_state.pdf'),
       width = 15, height = 10)

# 2) plot time series of effluent violation rate by state -----------------
tmp <- dmr_reporter_count %>%
  group_by(state_code, year_quarter) %>%
  summarise(dmr_reporter_count = sum(permit_count))
violation_rate <- violation %>%
  filter(violation_type == 'Effluent') %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'PERMIT_TYPE_CODE')], by = c('NPDES_ID' = 'EXTERNAL_PERMIT_NMBR')) %>%
  group_by(state_code, year_quarter, NPDES_ID) %>%
  summarise(n = n()) %>%
  group_by(state_code, year_quarter) %>%
  summarise(violator_count = n()) %>%
  full_join(tmp) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

plot_df <- violation_rate %>%
  filter(state_code %in% early_states)

tmp <- plot_df %>%
  filter(state_code == 'CA') %>%
  rename(ca_violation_rate = violation_rate) %>%
  ungroup() %>%
  select(year_quarter, ca_violation_rate)
plot_df <- plot_df %>%
  full_join(tmp) %>%
  filter(state_code != 'CA')

ggplot(plot_df, aes(x = year_quarter)) +
  geom_line(aes(y = violation_rate), color = '#F8766D', show.legend = TRUE) +
  geom_line(aes(y = ca_violation_rate), color = '#00BFC4', show.legend = TRUE) +
  annotate('text', x = 2015, y = 0.9, label = 'California', color = '#00BFC4') +
  annotate('text', x = 2015, y = 0.7, label = 'State', color = '#F8766D') +
  ylim(0,1) +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with at least 1 violation') +
  facet_wrap(~state_code, scales = 'free') +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure_dir, 'violation_rate_comparison_by_state.pdf'),
       width = 15, height = 10)

# 3) determine pre-treatment window ---------------------------------------

ca_reporter_count <- dmr_record %>%
  filter(state_code == 'CA') %>%
  group_by(state_code, year_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(state_code, year_quarter) %>%
  summarise(permit_count = n()) 

## a) plot the number of DMR reporters in CA before 2000 

plot_df <- ca_reporter_count %>%
  filter(year_quarter < 2000.2)

ggplot(plot_df, aes(x = year_quarter, y = permit_count)) +
  geom_line() +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Permit Count',
       title = 'California DMR Reporter Count Pre-2000') +
  theme_classic()
ggsave(file.path(figure_dir, 'ca_dmr_reporter_count_pre2000.pdf'),
       width = 6, height = 4)
## ==> CA had more than 10 reporters starting in 1989
## ==> there is an abnormal dip of dmr reporter in 1995 Q4 and 1996 Q1
## ==> we can either only consider post post 1995 data or remove 1995 Q4 and 1996 Q1 from the pretreatment window

## b) plot rate of change in the number of DMR repoters before 2000

plot_df <- ca_reporter_count %>%
  filter(year_quarter < 2000.2) %>%
  arrange(year_quarter) %>%
  mutate(diff = lead(permit_count) - permit_count) %>%
  mutate(pct_diff = diff/permit_count)

ggplot(plot_df, aes(x = year_quarter, y = diff)) +
  geom_line()
## ==> rate of change remains pretty consistent except for 1995 Q4 and 1996 Q1

## c) plot the number of states having data before a certain year

plot_df <- dmr_record %>%
  filter(year_quarter < 2000.2) %>%
  group_by(year_quarter, state_code) %>%
  summarise(permit_count = n()) %>%
  group_by(year_quarter) %>%
  summarise(state_count = n())

ggplot(plot_df, aes(x = year_quarter, y = state_count)) +
  geom_bar(stat = 'identity') 
## ==> we see a good amount of states reporting starting from 1989
## ==> so if we want to extend the pretreatment trend further back, the earliest year should be 1989

## ==> ways forward
## 1) 1996-2000
## 2) 1989-2000 with 1995 Q4 and 1996 Q1 removed

# 4) optimal bin size for pretreatment observation in California -----------

## a) aggregate violation rate based on different frequencies

## plot violation rate at different frequencies: monthly, quarterly, yearly
plot_violation_rate <- function(dmr, frequency){
  violation_rate <- dmr %>%
    group_by(!!as.name(frequency), EXTERNAL_PERMIT_NMBR) %>%
    summarise(measurement_count = n(),
              violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
    group_by(!!as.name(frequency)) %>%
    summarise(dmr_reporter_count = n(),
              violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
    mutate(violation_rate = violator_count/dmr_reporter_count) %>%
    mutate(intervention = !!as.name(frequency) > 2000) %>%
    filter(!!as.name(frequency) > 1989 & !!as.name(frequency) < 2020) 
  
  frequency_text <- ifelse(frequency == 'year_month', 'Monitoring Period End Date (Monthly)',
                           ifelse(frequency == 'monitoring_quarter', 'Monitoring Quarter (Fiscal)',
                                  'Monitoring Year'))
  
  ggplot(violation_rate, aes(x = !!as.name(frequency), y = violation_rate, group = intervention, color = intervention)) +
    geom_point(aes(size = dmr_reporter_count)) +
    geom_line() +
    geom_vline(xintercept = 2000.01, linetype = 'dashed', color = 'grey40') +
    geom_vline(xintercept = 2004.01, linetype = 'dashed', color = 'grey40') +
    labs(x = frequency_text,
         y = 'Proportion of Permit with At Least 1 Violation',
         title = 'Permit Violation Rate (DMR)') +
    ylim(0, 1) +
    scale_color_discrete(name = '', labels = c('Before MMP', 'After MMP')) +
    scale_size_continuous(name = 'DMR Reporter Count') +
    theme_classic() +
    theme(legend.position = 'top')
  ggsave(file.path(figure_dir, paste0('ca_permit_violation_rate_', frequency, '.pdf')),
         width = 6, height = 4) 
}

plot_violation_rate(dmr, 'monitoring_year')
plot_violation_rate(dmr, 'monitoring_quarter')
plot_violation_rate(dmr, 'year_month')

## ==> monthly and quarterly violation rates plagued by seasonality
## ==> yearly violation rates would lower sample size (power matters here?)

# 5) optimal bandwidth ----------------------------------------------------

plot_violation_rate_bw <- function(dmr, frequency){

  violation_rate <- dmr %>%
    group_by(!!as.name(frequency), EXTERNAL_PERMIT_NMBR) %>%
    summarise(measurement_count = n(),
              violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
    group_by(!!as.name(frequency)) %>%
    summarise(dmr_reporter_count = n(),
              violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
    mutate(violation_rate = violator_count/dmr_reporter_count) %>%
    mutate(intervention = !!as.name(frequency) > 2000.2) 
  
  out <- rdbwselect(y = violation_rate$violation_rate,
                    x = violation_rate[[frequency]],
                    c = 2000.2, 
                    bwselect = 'mserd')
  # out <- IKbandwidth(X = violation.rate[[frequency]],
  #                    Y = violation.rate$violation_rate,
  #                    cutpoint = 2000.2, 
  #                    verbose = TRUE,
  #                    kernel = 'cosine')
  summary(out) # 4 years before and after
  
  h <- floor(out$bws[1])
  
  violation_rate <- violation_rate %>%
    filter(!!as.name(frequency) >= 2000-h & !!as.name(frequency) <= 2000+h) %>%
    filter(dmr_reporter_count > 10)
  
  frequency_text <- ifelse(frequency == 'year_month', 'Monitoring Period End Date (Monthly)',
                           ifelse(frequency == 'monitoring_quarter', 'Monitoring Quarter (Fiscal)',
                                  'Monitoring Year'))
  
  ggplot(violation_rate, aes(x = !!as.name(frequency), y = violation_rate, group = intervention, color = intervention)) +
    geom_point(aes(size = dmr_reporter_count)) +
    geom_line() +
    geom_vline(xintercept = 2000.1, linetype = 'dashed', color = 'grey40') +
    labs(x = frequency_text,
         y = 'Proportion of Permit with At Least 1 Violation',
         title = 'Permit Violation Rate (DMR)') +
    ylim(0, 1) +
    scale_color_discrete(name = '', labels = c('Before MMP', 'After MMP')) +
    scale_size_continuous(name = 'DMR Reporter Count') +
    theme_classic() +
    theme(legend.position = 'top')
  ggsave(file.path(figure_dir, paste0('bandwidth_', frequency, '.pdf')),
         width = 8, height = 6) 
}

## a) using quarterly violation rate
plot_violation_rate_bw(dmr, 'monitoring_quarter')

## b) using yearly violation rate
plot_violation_rate_bw(dmr, 'monitoring_year')

## c) using monthly violation rate
plot_violation_rate_bw(dmr, 'year_month')

## ==> best bandwidths: 1996 - 2004, but MMP for reporting violations was implemented in 2004. We will cap at 2003. 

# 6) build rules for donor pool units selection ---------------------------
## Criteria for donor pool states
# a) has data from 1996 to 2004
# b) consistent reporting around 2000 
# c) no major policies around 2000
# d) selected predictors

## a) has data within observation window
window <- seq(1996, 2003, 1)

leftout_window <- dmr_record %>%
  filter(monitoring_year %in% window) %>%
  group_by(state_code, monitoring_year) %>%
  summarise(n = n()) %>%
  group_by(state_code) %>%
  summarise(year_count = n()) %>%
  filter(year_count != length(window)) 
leftout_window # 7 states removed: KS, MP, NC, ND, SR, VT, WY

## b) consistent reporting around 2000 
### i) slope of increase
### ii) minimum number of DMR reporters
reporting_change <- dmr_record %>%
  filter(monitoring_year %in% window) %>%
  group_by(state_code, monitoring_year, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(state_code, monitoring_year) %>%
  summarise(permit_count = n()) %>%
  arrange(state_code, monitoring_year) %>%
  mutate(diff = lead(permit_count) - permit_count) %>%
  mutate(pct_diff = diff/permit_count)

tmp <- reporting_change %>%
  group_by(state_code) %>%
  summarise(pct_diff_avg = mean(pct_diff, na.rm = TRUE),
            pct_diff_2000 = nth(pct_diff, ceiling(length(window)/2)))
summary(tmp$pct_diff_avg)
summary(tmp$pct_diff_2000)

# take the median of pct difference averaged over the observation window and the median of pct difference in 2000
threshold_avg <- quantile(summary(tmp$pct_diff_avg), 0.50) # 0.4285055
threshold_2000 <- quantile(summary(tmp$pct_diff_2000), 0.50) # 0.4618338

leftout_reporting <- tmp %>%
  filter(pct_diff_avg > threshold_avg | pct_diff_2000 > threshold_2000)
leftout_reporting # 19 states removed

# remove states that have less than 20 permits in 2000 for now: 20 is an arbiturary number
leftout_permit <- reporting_change$state_code[reporting_change$permit_count < 20 & reporting_change$monitoring_year == 2000]
# 5 removed

leftout <- c(leftout_window$state_code, leftout_reporting$state_code, leftout_permit) %>%
  unique()

reporting_change <- reporting_change %>%
  mutate(donor_unit = !(state_code %in% leftout))

# plot permit pct change by selected donor units
ggplot(reporting_change, aes(x = monitoring_year, y = pct_diff, color = donor_unit)) +
  geom_line() +
  geom_vline(xintercept = 2000.1, linetype = 'dashed', color = 'grey40') +
  facet_wrap(~state_code, scales = 'free') 

# output donor pool violation rate 
violation_rate <- violation %>%
  filter(monitoring_year %in% window) %>%
  filter(violation_type == 'Effluent') %>%
  group_by(state_code, monitoring_year, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = n()) %>%
  group_by(state_code, monitoring_year) %>%
  summarise(violator_count = n())

synth_data <- violation_rate %>%
  left_join(reporting_change) %>%
  mutate(violation_rate = violator_count/ permit_count) 
length(unique(synth_data$state_code)) 
length(unique(synth_data$state_code[synth_data$donor_unit])) # 22 donor states

write_csv(synth_data, file.path(output_dir, 'test_synthetic_control_data.csv'))

