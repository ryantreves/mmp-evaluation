

####################
# CA MMP Analysis
## Outcome variable analysis
####################

# Previously, we use permit-quarter aggregate violation rate to compare data between ICIS and CA data 
# We observed a substantial drop in violation rate around 2000.
# But MMP is for single violations. We want to have a more granular analysis of MMP effect on violation count overall.
# We also want to evaluate heterogeneity in the effects of different violation types 
## 1) MMP violation vs non-MMP violation
## 2) serious MMP violation vs chronic MMP violation
## 3) Group I vs Group II vs Group III pollutants

# set up ------------------------------------------------------------------

OAK_DIR <- Sys.getenv('OAK')
setwd(file.path(OAK_DIR, 'EPA'))

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 'lubridate', 'stringr', 'tidyverse')

# data folder
download_date <- '2020-07-07_14-57-43'
data.dir.raw <- file.path('Data', 'raw', download_date)
data.dir.manual <- file.path('Data', 'manual')

# analysis folder
analysis.dir <- file.path('Analysis', 'ca_mmp')
figure.dir <- file.path(analysis.dir, 'figures')
output.dir <- file.path(analysis.dir, 'output')

# Read data ---------------------------------------------------------------

# permits 
permits <- read_csv(file.path(data.dir.raw, 'npdes', 'ICIS_PERMITS.csv'), col_types=cols(.default='c')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  filter(PERMIT_TYPE_CODE %in% c('NPD', 'GPC')) %>%
  mutate(PERMIT_TYPE_CODE = ifelse(PERMIT_TYPE_CODE == 'NPD', 'individual', 'general')) %>%
  mutate(MAJOR_MINOR_STATUS_FLAG = ifelse(MAJOR_MINOR_STATUS_FLAG == 'M', 'Major', 'Minor')) %>%
  mutate(EFFECTIVE_DATE = mdy(EFFECTIVE_DATE),
         EXPIRATION_DATE = mdy(EXPIRATION_DATE),
         RETIREMENT_DATE = mdy(RETIREMENT_DATE),
         TERMINATION_DATE = mdy(TERMINATION_DATE)) 

# CA DMR data: output using output_ca_dmr.R
dmr <- read_csv(file.path(output.dir, 'CA_dmrs_2020-07-07_14-57-43.csv'), 
                col_type = cols(.default = 'c')) %>%
  mutate(monitoring_end_date = ymd(MONITORING_PERIOD_END_DATE)) %>%
  mutate(parameter_group = as.numeric(SNC_FLAG)) %>%
  mutate(exceedence_pct  = as.numeric(EXCEEDENCE_PCT)) %>%
  mutate(violation_type = ifelse(VIOLATION_CODE == 'E90', 'Effluent', 'Reporting')) %>%
  mutate(monitoring_quarter = as.numeric(monitoring_quarter)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'MAJOR_MINOR_STATUS_FLAG', 'FACILITY_TYPE_INDICATOR')]) %>%
  mutate(facility_size = MAJOR_MINOR_STATUS_FLAG) %>%
  distinct()

# CA NPDES permit data
ca.permits <- read_csv(file.path(data.dir.manual, 'california', 'ca_npdes_data.csv'))

# Analysis ----------------------------------------------------------------


# 1) violation rate at the permit level  ----------------------------------

violation.rate <- dmr %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(measurement_count = n(),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = n(),
            violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1)

## a) plot violation rate over time
ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate)) +
  geom_line(color = '#F8766D') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Permit Violation Rate (DMR)') +
  theme_classic()
ggsave(file.path(figure.dir, 'dmr_permit_level_violation_rate.pdf'),
       width = 6, height = 4)

## b) plot dmr reporter count over time
ggplot(violation.rate, aes(x = monitoring_quarter, y = dmr_reporter_count)) +
  geom_line(color = '#00BFC4') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Reporter Count',
       title = 'Count of DMR Reporters Over Time') +
  theme_classic()
ggsave(file.path(figure.dir, 'dmr_reporter_count.pdf'),
       width = 6, height = 4)


# 2) violation rate at the measurement level (instead of permit) ---------

violation.rate <- dmr %>%
  group_by(monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(violation_rate = violation_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.1)
# write_csv(violation.rate, file.path(output.dir, 'dmr_value_level_violation_rate.csv'))
# violation.rate <- read_csv(file.path(output.dir, 'dmr_value_level_violation_rate.csv'))

## a) plot violation rate
ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate)) +
  geom_line(color = '#F8766D') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Reported Values With Exceedance',
       title = 'Effluent Violation Rate At the Value Level (DMR)') +
  theme_classic()
ggsave(file.path(figure.dir, 'dmr_value_level_violation_rate.pdf'),
       width = 6, height = 4)

## b) plot number of reported value over time
ggplot(violation.rate, aes(x = monitoring_quarter, y = reported_value_count)) +
  geom_line(color = '#00BFC4') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Value Count',
       title = 'Count of Reported DMR Values Over Time') +
  theme_classic()
ggsave(file.path(figure.dir, 'dmr_reported_value_count.pdf'),
       width = 6, height = 4)

## ==> violation rate at the value level doesn't show a clear effect of MMP..
## ==> there is a drop of values reported around 2000, could it be that the database migration lost only violation records? 
## ==> if this is true, we should be worried about the violation rate at the permit level
## ==> it could also be because of outliers... facilities that have a high number of violations due to many reason

# 3) decompose violation rate by consistent/new reporting ----------------

tmp <- dmr %>%
  filter(monitoring_quarter < 2000.2) 
early.measurement <- unique(tmp$EXTERNAL_PERMIT_NMBR)

violation.rate <- dmr %>%
  mutate(early_measurement = EXTERNAL_PERMIT_NMBR %in% early.measurement) %>%
  group_by(early_measurement, monitoring_quarter) %>%
  summarise(reported_value_count = n(),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(violation_rate = violation_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.2)

## a) plot measurement count for permit-pollutant and see if they are stable

ggplot(violation.rate, aes(x = monitoring_quarter, y = reported_value_count, source = early_measurement, color = early_measurement)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  theme_classic() +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Reported Value Count',
       title = 'Reported Value Count by Early/New Measurement around 2000') +
  scale_color_discrete(name = 'Measurement',
                       labels = c('Started After 2000',
                                  'Started Before 2000')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_reported_value_count_by_measurement_time.pdf'),
       width = 6, height = 4)

## b) plot violation rate by measurement time

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = early_measurement, color = early_measurement)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Reported Values in Violation',
       title = 'Decomposed Violation Rate (Value Level)') +
  theme_classic() + 
  scale_color_discrete(name = 'Measurement',
                       labels = c('Started After 2000',
                                  'Started Before 2000')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_value_violation_rate_by_measurement_time.pdf'),
       width = 6, height = 4)

## ==> No obvious MMP effect even for early measurements…
## ==> Try finding outliers

# 3) see if there are outliers: histogram of number of violations per permits overtime ------------------------------

violation.rate <- dmr %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count) %>%
  filter(monitoring_quarter >= 1996.2 &
           monitoring_quarter <= 2004.2)

## a) histogram of number of reproted values overtime 
ggplot(violation.rate, aes(x = reported_value_count)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter)

## b) histogram of violation count overtimes
plot.df <- violation.rate %>%
  filter(exceedence_count != 0)
ggplot(plot.df, aes(x = exceedence_count)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter) +
  labs(x = 'Violation Count',
       y = 'Permit Count',
       title = 'Histogram of Violation Count (1996-2004)') +
  theme_classic()
ggsave(file.path(figure.dir, 'violation_count_histogram.pdf'),
       width = 7, height = 6)

## c) histogram of violation rate overtimes
ggplot(plot.df, aes(x = effluent_violation_rate)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter)

# restrict to early reporters 
tmp <- violation.rate %>%
  mutate(pre_2000 = monitoring_quarter <= 2000.1,
         post_2000 = monitoring_quarter > 2000.1) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  summarise(pre_2000_quarter_count = sum(pre_2000, na.rm = TRUE),
         post_2000_quarter_count = sum(post_2000, na.rm = TRUE)) %>%
  filter(pre_2000_quarter_count > 0 & post_2000_quarter_count > 0)

early.reporters <- tmp$EXTERNAL_PERMIT_NMBR

plot.df <- violation.rate %>%
  filter(EXTERNAL_PERMIT_NMBR %in% early.reporters) %>%
  filter(exceedence_count != 0)

ggplot(plot.df, aes(x = effluent_violation_rate)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter)
ggplot(plot.df, aes(x = exceedence_count)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter)
ggplot(plot.df, aes(x = reported_value_count)) +
  geom_histogram(bins = 50) +
  facet_wrap(~monitoring_quarter)

## ==> There are some obvious outliers when we look at violation count post-2000 (among all permits)
## ==> How much is the violation rate affected by these outliers?

# 4) identify and analyze outliers around 2000 ---------------------------------------

violation.rate <- dmr %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.1 &
           monitoring_quarter < 2004.2) 

# by eyeballing these permits look especially bad
bad.apples <- c('CA0108928', 'CA0053813', 'CA0048160', 'CA0107417') 

## a) plot violation rate decomposed
violation.rate <- dmr %>%
  #filter(EXTERNAL_PERMIT_NMBR != 'CA0108928') %>%
  mutate(bad_apple_flag = EXTERNAL_PERMIT_NMBR %in% bad.apples) %>%
  group_by(monitoring_quarter, bad_apple_flag) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.2)

violation.rate.agg <- dmr %>%
  group_by(monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.2)

ggplot() +
  geom_line(data = violation.rate, aes(x = monitoring_quarter, y = effluent_violation_rate, group = bad_apple_flag, color = bad_apple_flag)) +
  geom_line(data = violation.rate.agg, 
            aes(x = monitoring_quarter, y = effluent_violation_rate), 
            linetype = 'dashed',
            color = '#F8766D') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Reported Values With Exceedance',
       title = 'Effluent Violation Rates At the Value Level (DMR) Decomposed') +
  scale_color_manual(name = '',
                       labels = c('With Outliers Removed', paste0(length(bad.apples), ' Outliers')),
                       values = alpha(c('#F8766D', '#00BFC4'),c(1, 0.4))) +
  theme_classic()+
  theme(legend.position = 'top') 
ggsave(file.path(figure.dir, 'dmr_value_level_violation_rate_outlier.pdf'),
       width = 6, height = 4)

## ==> violation rate at the value level is very sensitive to outliers
## ==> investigate ways forward: 
## 1) data error/ not --> we should use violation rate at the permit level because it smoothes out these outliers
## 2) are there some commonalities between the outliers --> differential effect of MMP? 

## b) check data for outliers
violation.rate <- dmr %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count) %>%
  filter(monitoring_quarter > 1996.1 &
           monitoring_quarter < 2004.2)
# check violation rate

plot.df <- violation.rate %>%
  filter(EXTERNAL_PERMIT_NMBR %in% bad.apples)
ggplot(plot.df, aes(x = monitoring_quarter, y = reported_value_count, source = EXTERNAL_PERMIT_NMBR, color = EXTERNAL_PERMIT_NMBR)) +
  geom_line()
ggplot(plot.df, aes(x = monitoring_quarter, y = exceedence_count, source = EXTERNAL_PERMIT_NMBR, color = EXTERNAL_PERMIT_NMBR)) +
  geom_line()
ggplot(plot.df, aes(x = monitoring_quarter, y = effluent_violation_rate, source = EXTERNAL_PERMIT_NMBR, color = EXTERNAL_PERMIT_NMBR)) +
  geom_line()

## what are these outliers?
tmp <- dmr %>%
  filter(EXTERNAL_PERMIT_NMBR %in% c('CA0053813'))
## ==> Potential data errors: Most of the violations in 2000.3 were resolved manually by the EPA/ state.
tmp <- dmr %>%
  filter(EXTERNAL_PERMIT_NMBR %in% c('CA0048160'))
view <- tmp %>%
  group_by(monitoring_quarter, POLLUTANT_CODE) %>%
  summarise(n = n()) %>%
  group_by(monitoring_quarter) %>%
  summarise(n = n()) 
## ==> Most of the violations early on resulted from pollutants that it is required to measure yearly (end of year). Most of earlier values were violations but after 2000: NODI B: Below Detection Limit/No Detection 
## ==> Could MMP increase strategic nonreporting? 
tmp <- dmr %>%
  filter(EXTERNAL_PERMIT_NMBR %in% c('CA0107417'))
tmp <- dmr %>%
  filter(EXTERNAL_PERMIT_NMBR %in% c('CA0108928'))

## do they share any characteristics?
tmp <- permits %>%
  filter(EXTERNAL_PERMIT_NMBR %in% bad.apples) 
table(tmp$MAJOR_MINOR_STATUS_FLAG)
table(tmp$FACILITY_TYPE_INDICATOR)
## ==> they are all major facilities
## ==> they are all federal or public: can look into differential effect of MMP for different facility types
## perhaps MMP is not that useful for intransigent violators

# 5) experiment: identify and remove outliers methodoligically around 2000 ----------------------------------------------------
## note this is problematic: By construction, many outliers were determined based on post-2000 data
## just for internal experiment

violation.rate <- dmr %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_quarter) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count)

exceedence.counts <- violation.rate$exceedence_count[violation.rate$exceedence_count > 0]

q1 <- quantile(exceedence.counts, 0.25)
q3 <- quantile(exceedence.counts, 0.75)
iqr <- q3 - q1

# I am not sure if this is a rigorous way to find outliers
extreme.threshold <- iqr * 6 + q3

tmp <- violation.rate %>%
  filter(exceedence_count > extreme.threshold) 

nrow(tmp)
length(unique(tmp$EXTERNAL_PERMIT_NMBR))
unique(tmp$EXTERNAL_PERMIT_NMBR)

bad.apples <- unique(tmp$EXTERNAL_PERMIT_NMBR)

## a) make sure they are not data errors
plot.df <- violation.rate %>%
  filter(EXTERNAL_PERMIT_NMBR %in% bad.apples)
ggplot(plot.df, aes(x = monitoring_quarter, y = reported_value_count, source = EXTERNAL_PERMIT_NMBR, color = EXTERNAL_PERMIT_NMBR)) +
  geom_line()
## ==> CA0108928 for some reason only had 10 reported values on 2000.3 
## ==> most permits have a drop of reported values around 2000.3 strangely, could be a seasonal effect though

tmp <- permits %>%
  filter(EXTERNAL_PERMIT_NMBR %in% bad.apples) 
table(tmp$MAJOR_MINOR_STATUS_FLAG)
table(tmp$FACILITY_TYPE_INDICATOR)

table(permits$FACILITY_TYPE_INDICATOR[permits$state_code == 'CA' & permits$PERMIT_TYPE_CODE == 'individual'])

## b) plot violation rate decomposed
violation.rate <- dmr %>%
  #filter(EXTERNAL_PERMIT_NMBR != 'CA0108928') %>%
  mutate(bad_apple_flag = EXTERNAL_PERMIT_NMBR %in% bad.apples) %>%
  group_by(monitoring_quarter, bad_apple_flag) %>%
  summarise(expected_value_count = sum(!is.na(LIMIT_VALUE_STANDARD_UNITS)),
            reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS)),
            exceedence_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(effluent_violation_rate = exceedence_count/reported_value_count)

plot.df <- violation.rate %>%
  filter(monitoring_quarter > 1996.2)

ggplot(plot.df, aes(x = monitoring_quarter, y = effluent_violation_rate, group = bad_apple_flag, color = bad_apple_flag)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 2004.2, y = 0.09, label = 'MMP Program Started\nin California') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Reported Values With Exceedance',
       title = 'Effluent Violation Rates At the Value Level (DMR)') +
  scale_color_discrete(name = 'Outlier',
                       labels = c('Remaining Permits', paste0(length(bad.apples), ' Outliers'))) +
  theme_classic()+
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_value_level_violation_rate_adjusted.pdf'),
       width = 6, height = 4)

## c) plot reported value count decomposed
ggplot(plot.df, aes(x = monitoring_quarter, y = reported_value_count, group = bad_apple_flag, color = bad_apple_flag)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 2006, y = 15000, label = 'MMP Program Started\nin California') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Value Count',
       title = 'Count of Reported DMR Values Over Time') +
  scale_color_discrete(name = 'Outlier',
                       labels = c('Remaining Permits', paste0(length(bad.apples), ' Outliers'))) +
  theme_classic()+
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_reported_value_count_adjusted.pdf'),
       width = 6, height = 4)
## ==> violation rates 

# 6) identify violation type: MMP (serious and chronic) vs non-MMP ----------------------------------

# MMP
## serious MMP: Group I by 40% or more, or Group II by 20% or more
## chronic MMP: any effluent violations four or more times in any period of six consecutive months
# nonMMP
## anything else

## flag and count serious and chronic MMP
dmr <- dmr %>%
  mutate(serious_mmp_flag = ifelse((parameter_group == 1 & exceedence_pct >= 40 & violation_type == 'Effluent') |
                                     (parameter_group == 2 & exceedence_pct >= 20 & violation_type == 'Effluent'), TRUE, FALSE))


tmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  arrange(EXTERNAL_PERMIT_NMBR, monitoring_end_date) 

count_chronic <- function(permit, violation_date){
  tmp %>%
    filter(EXTERNAL_PERMIT_NMBR == permit & 
             monitoring_end_date < violation_date & 
             monitoring_end_date >= add_with_rollback(violation_date, months(-6))) %>%
    nrow()
}

# this would take a few minutes
chronic.mmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  arrange(EXTERNAL_PERMIT_NMBR, monitoring_end_date) %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_end_date) %>%
  mutate(chronic_violation_count = count_chronic(EXTERNAL_PERMIT_NMBR, monitoring_end_date),
         chronic_mmp_flag = chronic_violation_count >= 4)

dmr <- chronic.mmp %>%
  full_join(dmr) 

dmr <- dmr %>%
  mutate(chronic_mmp_flag = chronic_violation_count >= 4) %>%
  # if the 4th is a serious MMP, it would be counted as serious MMP
  mutate(chronic_mmp_flag = chronic_mmp_flag & !serious_mmp_flag) %>%
  mutate(mmp_flag = serious_mmp_flag | chronic_mmp_flag)


# 7) differential effect: MMP vs non-MMP ----------------------------------

violation.rate <- dmr %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(value_count = n(),
            mmp_violation_count = sum(mmp_flag, na.rm = TRUE),
            non_mmp_violation_count = sum(violation_type == 'Effluent' & !mmp_flag, na.rm = TRUE)) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = sum(value_count > 0, na.rm = TRUE),
            mmp_violator_count = sum(mmp_violation_count > 0, na.rm = TRUE),
            non_mmp_violator_count = sum(non_mmp_violation_count > 0, na.rm = TRUE)) %>%
  mutate(mmp_violation_rate = mmp_violator_count/dmr_reporter_count,
         non_mmp_violation_rate = non_mmp_violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1)
  
plot.df <- violation.rate %>%
  gather(type, violation_rate, c(mmp_violation_rate, non_mmp_violation_rate))

ggplot(plot.df, aes(x = monitoring_quarter, y = violation_rate, group = type, color = type)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rates By Violation Type') +
  scale_color_discrete(name = 'Violation Type',
                       labels = c('MMP', 'Non-MMP')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_permit_violation_rate_mmp_vs_nonmmp.pdf'),
       width = 6, height = 4)
## ==> There is a clear differential effect of MMP for MMP and non-MMP violations

# 8) differential effect: serious MMP vs chronic MMP  ---------------------

violation.rate <- dmr %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(value_count = n(),
            serious_mmp_count = sum(serious_mmp_flag, na.rm = TRUE),
            chronic_mmp_count = sum(chronic_mmp_flag, na.rm = TRUE)) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = sum(value_count > 0, na.rm = TRUE),
            serious_mmp_violator_count = sum(serious_mmp_count > 0, na.rm = TRUE),
            chronic_mmp_violator_count = sum(chronic_mmp_count > 0, na.rm = TRUE)) %>%
  mutate(serious_mmp_rate = serious_mmp_violator_count/dmr_reporter_count,
         chronic_mmp_rate = chronic_mmp_violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1)

plot.df <- violation.rate %>%
  gather(type, violation_rate, c(serious_mmp_rate, chronic_mmp_rate))

ggplot(plot.df, aes(x = monitoring_quarter, y = violation_rate, group = type, color = type)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rates By MMP Violation Type') +
  scale_color_discrete(name = 'Violation Type',
                       labels = c('Chronic MMP', 'Serious MMP')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_permit_violation_rate_serious_chronic_mmp.pdf'),
       width = 6, height = 4)
## ==> thee thrends track each other pretty well: no stronger effect on chronic MMP

# 9) differential effect: parameter group ---------------------------------

dmr.reporter.count <- dmr %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = n())

violation.rate <- dmr %>%
  mutate(parameter_group = ifelse(parameter_group == 1, 'Group I', 
                                  ifelse(parameter_group == 2, 'Group II', 'Group III'))) %>%
  group_by(monitoring_quarter, parameter_group, mmp_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, parameter_group, mmp_flag) %>%
  summarise(violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  full_join(dmr.reporter.count) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1) %>%
  filter(!is.na(mmp_flag))

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, source = mmp_flag, color = mmp_flag)) +
  geom_line() +
  facet_wrap(~parameter_group) +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rates By Parameter Group') +
  scale_color_discrete(name = 'Violation Type',
                       labels = c('Non-MMP', 'MMP')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'dmr_permit_violation_rate_parameter_group.pdf'),
       width = 10, height = 4)

## ==> stronger effect on MMP violations for group I and group II 
## ==> increase violation rate for non-MMP violation for group I and group II?
## ==> the effect didn't last though...

# 10) differential effect: specific pollutant --------------------------------------------------

## In our converation with California's Erin, she mentioned that violations of some pollutants like coliform
## might have even experienced an increase because they are not counted in MMP 

## a) chlorine
## permit level
violation.rate <- dmr %>%
  filter(grepl('chlorine', PARAMETER_DESC, ignore.case = TRUE)) %>%
  group_by(monitoring_quarter, mmp_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, mmp_flag) %>%
  summarise(violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  full_join(dmr.reporter.count) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = mmp_flag, color = mmp_flag)) +
  geom_line() +
  labs(title = 'Chlorine')

## value level (with outliers removed)
violation.rate <- dmr %>%
  filter(!(EXTERNAL_PERMIT_NMBR %in% bad.apples)) %>%
  filter(grepl('chlorine', PARAMETER_DESC, ignore.case = TRUE)) %>%
  group_by(monitoring_quarter) %>%
  summarise(reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS), na.rm = TRUE),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(violation_rate = violation_count/reported_value_count)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate)) +
  geom_line() +
  labs(title = 'Chlorine')

ggplot(violation.rate, aes(x = monitoring_quarter, y = reported_value_count)) +
  geom_line() +
  labs(title = 'Chlorine')


## b) coliform 
## permit level
violation.rate <- dmr %>%
  filter(grepl('coliform', PARAMETER_DESC, ignore.case = TRUE)) %>%
  group_by(monitoring_quarter, mmp_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, mmp_flag) %>%
  summarise(violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  full_join(dmr.reporter.count) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = mmp_flag, color = mmp_flag)) +
  geom_line() +
  labs(title = 'Coliform')

## value level
violation.rate <- dmr %>%
  filter(!(EXTERNAL_PERMIT_NMBR %in% bad.apples)) %>%
  filter(grepl('coliform', PARAMETER_DESC, ignore.case = TRUE)) %>%
  group_by(monitoring_quarter) %>%
  summarise(reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS), na.rm = TRUE),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(violation_rate = violation_count/reported_value_count)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate)) +
  geom_line() +
  labs(title = 'Coliform')

ggplot(violation.rate, aes(x = monitoring_quarter, y = reported_value_count)) +
  geom_line() +
  labs(title = 'Coliform')


## ==> There is no clear differential effect


# 11) differential effect: public vs non-public ---------------------------

# public:
## POTW: publicly owned treatment works 
## FEDERAL
# private:
## non-POTW

violation.rate <- dmr %>%
  mutate(public_facility_flag = FACILITY_TYPE_INDICATOR %in% c('POTW', 'FEDERAL')) %>%
  group_by(monitoring_quarter, public_facility_flag, EXTERNAL_PERMIT_NMBR) %>%
  summarise(value_count = n(),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, public_facility_flag) %>%
  summarise(reporter_count = sum(value_count > 0, na.rm = TRUE),
            violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = violator_count/reporter_count) %>%
  filter(monitoring_quarter > 1996.1)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = public_facility_flag, color = public_facility_flag)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rates By Facility Type') +
  scale_color_discrete(name = 'Facility',
                       labels = c('Private', 'Public')) +
  theme_classic() +
  theme(legend.position = 'top')

ggplot(violation.rate, aes(x = monitoring_quarter, y = reporter_count, group = public_facility_flag, color = public_facility_flag)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Facility Count',
       title = 'DMR Reporter Count By Facility Type') +
  scale_color_discrete(name = 'Facility',
                       labels = c('Private', 'Public')) +
  theme_classic() +
  theme(legend.position = 'top')

## ==> there isn't an obvious signal for differential effect...


# 12) differential effect: intransigent violator based on pre-2000 data ------------------------------
## there are many potential definitions here:
## by average violation rate over time
## by porportion of quarters in violation 

## a) average violation rate over time 
tmp <- dmr %>%
  filter(monitoring_quarter < 2000.2) %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(reported_value_count = sum(!is.na(DMR_VALUE_STANDARD_UNITS), na.rm = TRUE),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  mutate(violation_rate = violation_count/reported_value_count) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  summarise(avg_violation_rate = mean(violation_rate, na.rm = TRUE))

## flag the first 25% as intransigent and the rest as normal
tmp <- tmp %>%
  mutate(median = quantile(avg_violation_rate, 0.25, na.rm = TRUE)) %>%
  mutate(intransigent_flag = avg_violation_rate >= median)

violation.rate <- dmr %>%
  right_join(tmp[, c('EXTERNAL_PERMIT_NMBR', 'intransigent_flag')]) %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR, intransigent_flag) %>%
  summarise(measurement_count = n(),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, intransigent_flag) %>%
  summarise(dmr_reporter_count = n(),
            violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = intransigent_flag, color = intransigent_flag)) +
  geom_line()
## ==> MMP effect obvious for permits that had higher violation rates pre-2000 

## b) proportion of quarters in violation
tmp <- dmr %>%
  filter(monitoring_quarter < 2000.2) %>%
  group_by(EXTERNAL_PERMIT_NMBR, monitoring_quarter) %>%
  summarise(violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  summarise(quarter_total = n(),
            quarter_in_violation = sum(violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = quarter_in_violation/quarter_total)

tmp <- tmp %>%
  mutate(median = quantile(violation_rate, 0.25, na.rm = TRUE)) %>%
  mutate(intransigent_flag = violation_rate >= median)

violation.rate <- dmr %>%
  right_join(tmp[, c('EXTERNAL_PERMIT_NMBR', 'intransigent_flag')]) %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR, intransigent_flag) %>%
  summarise(measurement_count = n(),
            violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, intransigent_flag) %>%
  summarise(dmr_reporter_count = n(),
            violator_count = sum(violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter > 1996.1)

ggplot(violation.rate, aes(x = monitoring_quarter, y = violation_rate, group = intransigent_flag, color = intransigent_flag)) +
  geom_line()
## ==> similarly MMP effect obvious for permits that had higher violation rates pre-2000
## ==> but we only have 42 permits that reported DMR before 2000: this is a very small number of permits to compare with

