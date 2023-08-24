

##################
# using ICIS data to evaluate the impact of MMP program in California
##################

# Note:
# previously we generated some analysis using data from the California state water board solely (see notebook: /notebook/reports/ca_minimum_mandatory_penalty.ipynb)

# set up ------------------------------------------------------------------

OAK_DIR <- Sys.getenv('OAK')
HOME_DIR <- Sys.getenv('HOME')
setwd(file.path(OAK_DIR, 'EPA'))

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 'lubridate', 'stringr', 'tidyverse')

# analysis folder
analysis.dir <- file.path(figure.dir, 'ca_mmp')
figure.dir <- file.path(analysis.dir, 'figures')
output.dir <- file.path(analysis.dir, 'output')

# read data ---------------------------------------------------------------
download_date <- '2020-07-07_14-57-43'
data.dir.raw <- file.path('Data', 'raw', download_date)
data.dir.manual <- file.path('Data', 'manual')

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

# facilities
facilities <- read_csv(file.path(data.dir.raw, 'npdes', 'ICIS_FACILITIES.csv'), col_types=cols(.default='c'))

# facility sectors 
sectors <- read_csv(file.path('Data','processed', '2019-12-23', 'prediction_20191', '2020-04-22_11-03-45', 'data.csv'),
                    col_type = cols_only(EXTERNAL_PERMIT_NMBR = col_character(),
                                         STATE_CODE = col_character(),
                                         sector = col_character())) %>%
  filter(STATE_CODE == 'CA') 

# reference tables
states.code <- read_csv(file.path(data.dir.manual, 'REF_STATISTICAL_BASE.csv'), col_types = cols(.default='c'))
parameters <- read_csv(file.path(data.dir.manual, 'REF_PARAMETER.csv')) %>%
  select(-c(CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE)) %>%
  mutate(PARAMETER_CODE = str_pad(PARAMETER_CODE, 5, pad = '0')) %>%
  # identify all non Type 1 and 2 pollutants to be Type 3 pollutants
  mutate(SNC_FLAG = ifelse(is.na(SNC_FLAG), 3, SNC_FLAG)) 

# violation data 
# from ICIS
violation <- read_csv(file.path(data.dir.raw, 'npdes', 'NPDES_EFF_VIOLATIONS.csv'),
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
  mutate(MONITORING_PERIOD_END_DATE = mdy(MONITORING_PERIOD_END_DATE),
         year = year(MONITORING_PERIOD_END_DATE),
         year_quarter = quarter(MONITORING_PERIOD_END_DATE, with_year = TRUE, fiscal_start = 10),
         state_code = substr(NPDES_ID, 1, 2),
         violation_type = ifelse(VIOLATION_CODE %in% c('D80', 'D90'), 'Reporting', 'Effluent')) %>%
  left_join(states.code) %>%
  # remove data errors
  filter(MONITORING_PERIOD_END_DATE <= Sys.Date())

# from California Waterboard (Erin)
ca.violation <- read_csv(file.path(output.dir, 'Erin Mustain - violations_export.csv'),
                         col_type = cols(.default = 'c')) %>%
  rename(EXTERNAL_PERMIT_NMBR = `NPDES# CA#`,  
         violation_status = `STATUS.1`,
         individual_general_flag = `INDIVIDUAL/GENERAL`) %>%
  mutate(npdes_flag = ifelse(`PROGRAM CATEGORY` %in% c('NPDESWW', 'NPDESSW'), TRUE, FALSE)) %>%
  mutate(individual_general_flag = ifelse(individual_general_flag == 'I', 'individual', 'general'), 
         `OCCURRED ON` = mdy(`OCCURRED ON`),
         `DISCOVERY DATE` = mdy(`DISCOVERY DATE`)) %>%
  mutate(`OCCURRED ON` = replace_na(`OCCURRED ON`, mdy('01/01/1000')),
         `DISCOVERY DATE` = replace_na(`DISCOVERY DATE`, mdy('01/01/1000'))) %>%
  mutate(year_quarter = as.character(quarter(`OCCURRED ON`, with_year = TRUE, fiscal_start = 10))) %>%
  # group_by(`VIOLATION ID (VID)`) %>%
  # mutate(record_date = max(`OCCURRED ON`, `DISCOVERY DATE`)) %>%
  # mutate(year_quarter = as.character(quarter(`record_date`, with_year = TRUE, fiscal_start = 10))) %>%
  # # remove problematic dates
  filter(year_quarter <= quarter(Sys.Date(), with_year = TRUE, fiscal_start = 10))

# analysis -------------------------------------------------

# 1) count of active permits for each quarter and state
tmp <- permits %>%
  group_by(state_code, EXTERNAL_PERMIT_NMBR, VERSION_NMBR, EFFECTIVE_DATE, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
  # use the maximum of expiration/ retirement/ termination date because permits can be admin continued even if it is expired
  mutate(effective_end_date = max(EXPIRATION_DATE, RETIREMENT_DATE, TERMINATION_DATE, na.rm = TRUE)) %>%
  mutate(effective_start_year_quarter = quarter(EFFECTIVE_DATE, with_year = TRUE, fiscal_start = 10),
         effective_end_year_quarter = quarter(effective_end_date, with_year = TRUE, fiscal_start = 10))

quarters <- c(seq(quarter(mdy('10/01/1980'), with_year = TRUE, fiscal_start = 10), 
                  quarter(mdy('10/01/2019'), with_year = TRUE, fiscal_start = 10), 
                  1),
              seq(quarter(mdy('01/01/1981'), with_year = TRUE, fiscal_start = 10), 
                  quarter(mdy('01/01/2020'), with_year = TRUE, fiscal_start = 10), 
                  1),
              seq(quarter(mdy('04/01/1981'), with_year = TRUE, fiscal_start = 10), 
                  quarter(mdy('04/01/2020'), with_year = TRUE, fiscal_start = 10), 
                  1),
              seq(quarter(mdy('07/01/1981'), with_year = TRUE, fiscal_start = 10), 
                  quarter(mdy('07/01/2020'), with_year = TRUE, fiscal_start = 10), 
                  1))

count_active_permits <- function(df, quarter){
  message(quarter)
  df %>%
    filter(effective_start_year_quarter <= quarter &
             (effective_end_year_quarter >= quarter | is.na(effective_end_year_quarter))) %>%
    mutate(year_quarter = quarter) %>%
    group_by(year_quarter, state_code, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
    summarise(active_permit_count = n())
}

# test
# count_active_permits(df = tmp, quarter = 2010.1)

permit.count <- lapply(quarters, count_active_permits, df = tmp) %>%
  do.call(rbind, .)
# the numbers largely match with the California data

#write_csv(permit.count, file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

permit.count <- read_csv(file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

plot.df <- permit.count %>%
  filter(state_code == 'CA') 
ggplot(plot.df, aes(x = year_quarter, y = active_permit_count, group = MAJOR_MINOR_STATUS_FLAG, color = MAJOR_MINOR_STATUS_FLAG)) +
  geom_line() +
  facet_wrap(~PERMIT_TYPE_CODE, scales = 'free')
  
# 2) count of permits in violation (by type) in each quater and state

violation.count <- violation %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'PERMIT_TYPE_CODE', 'MAJOR_MINOR_STATUS_FLAG')], by = c('NPDES_ID' = 'EXTERNAL_PERMIT_NMBR')) %>%
  group_by(year_quarter, state_code, violation_type, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG, NPDES_ID) %>%
  summarise(n = n()) %>%
  group_by(year_quarter, state_code, violation_type, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
  summarise(permit_count = n()) %>%
  spread(violation_type, permit_count)

#write_csv(violation.count, file.path(output.dir, 'violator_count_by_quarter_state.csv'))

violation.count <- read_csv(file.path(output.dir, 'violator_count_by_quarter_state.csv'))

# 3) calculate the violation rates for each quarter and state

violation.rate <- violation.count %>% 
  right_join(permit.count) %>%
  select(quarter, state_code, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG, active_permit_count, effluent, reporting) %>%
  mutate_all(replace_na, 0) %>%
  mutate(violation_count = effluent + reporting,
         effluent_rate = effluent/active_permit_count,
         reporting_rate = reporting/active_permit_count,
         violation_rate = violation_count/active_permit_count) 

#write_csv(violation.rate, file.path(output.dir, 'violation_rate_by_quarter_state.csv'))

violation.rate <- read_csv(file.path(output.dir, 'violation_rate_by_quarter_state.csv'))


# plot violation rate diff-in-diff --------------------------------------------------------------------

# remove odd rates: those bigger than 1
# this might be because violation data is more complete than permit data 
# but need to check this 

states <- c('CA')

plot.df <- violation.rate %>%
  filter(effluent_rate <= 1) %>%
  filter(state_code %in% states) %>%
  filter(PERMIT_TYPE_CODE == 'individual')

ggplot(plot.df, aes(x = quarter, y = effluent_rate, color = state_code, group = state_code)) +
  geom_line()
# ==> there is a huge discrepency between the plot produced by Vincent, shown in the "ca_minimum_mandatory_penalty.ipynb" notebook
# ==> check if the ICIS violation data for California is complete

# data completeness -------------------------------------------------------

# 1) does the California data match with the ICIS data? 
# ICIS
icis.ca.violation <- violation %>%
  filter(state_code == 'CA')
#write_csv(icis.ca.violation, file.path(output.dir, 'ca_icis_violations.csv'))
icis.ca.violation <- read_csv(file.path(output.dir, 'ca_icis_violations.csv'),
                              col_types = cols(.default ='c')) %>%
  mutate(EXTERNAL_PERMIT_NMBR = NPDES_ID) %>%
  left_join(permits)

# California Erin
ca.violation.filt <- ca.violation %>%
  #filter(violation_status == 'Violation') %>%
  filter(npdes_flag) %>%
  filter(`VIOLATION TYPE` %in% c('Effluent', 'Reporting')) %>%
  # remove SEVs because it has been reported that California doesn't submit SEVs to ICIS
  # state review framework 2011 & 2016: https://echo.epa.gov/oversight/state-review-framework/tracker-recommendations/results?round=&state=CA&completion_status=&region=&media=&print=detail&element=&format=html
  filter(`Violation Source` %in% c('eSMR', 'Report'))

plot.df <- ca.violation %>%
  filter(violation_status == 'Violation') %>%
  filter(npdes_flag) %>%
  filter(`VIOLATION TYPE` %in% c('Effluent', 'Reporting')) %>%
  mutate(violation_source = ifelse(`Violation Source` %in% c('eSMR', 'Report'), 'DMR', 'SEV')) %>%
  group_by(year_quarter, violation_source) %>%
  summarise(n = n())
ggplot(plot.df, aes(x=year_quarter, y=n, group=violation_source, color=violation_source)) +
  geom_line()
plot.df %>%
  group_by(violation_source) %>%
  summarise(n = sum(n))
# 5527/(5527+86544) = 6% of records

# 2) plot violation rate comparison
ca.violation.count <- ca.violation.filt %>%
  rename(violation_type = `VIOLATION TYPE`,
         PERMIT_TYPE_CODE = individual_general_flag) %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE) %>%
  summarise(ca = n()) 

icis.ca.violation.count <- icis.ca.violation %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE) %>%
  summarise(icis = n())

icis.ca.comparison <- icis.ca.violation.count %>%
  full_join(ca.violation.count) %>%
  gather(data_source, violation_count, -c(year_quarter:PERMIT_TYPE_CODE))

#write_csv(icis.ca.comparison, file.path(output.dir, 'ca_icis_violation_comparison.csv'))

icis.ca.comparison <- read_csv(file.path(output.dir, 'ca_icis_violation_comparison.csv'))

# plot record count comparison
ggplot(icis.ca.comparison, aes(x = year_quarter, y = violation_count, group = data_source, color = data_source)) +
  geom_line()+
  facet_wrap(~violation_type+PERMIT_TYPE_CODE, scales = 'free') +
  theme_classic() +
  scale_color_discrete(name = 'Data Source', labels = c('CA Waterboard Erin', 'ICIS Public Violation Data')) +
  theme(legend.position = 'top') +
  labs( x = 'Quarter', y = 'Violation Count', title = 'California NPDES Violation Count By Different Data Sources')
ggsave(file.path(figure.dir, 'ca_npdes_violation_count_by_data_sources_using_report_date.png'), width = 7, height = 5)

# ==> there seems to be a huge discrepancy between CA data and ICIS data...
# ==> Effluent violaiton records largely match for different data sources, but only barely
# ==> it is possible that the unit of violation records is different for the California data and ICIS data
# ==> we will plot violation rate instead

# plot violation rate comparsion 
ca.violation.permit.count <- ca.violation.filt %>%
  rename(violation_type = `VIOLATION TYPE`,
         PERMIT_TYPE_CODE = individual_general_flag) %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE) %>%
  summarise(ca = n()) 

icis.ca.violation.permit.count <- icis.ca.violation %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'PERMIT_TYPE_CODE')]) %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(year_quarter, violation_type, PERMIT_TYPE_CODE) %>%
  summarise(icis = n())

tmp <- icis.ca.violation.permit.count %>%
  full_join(ca.violation.permit.count) %>%
  gather(data_source, violation_permit_count, -c(year_quarter:PERMIT_TYPE_CODE))

icis.ca.comparison.permit <- permit.count %>%
  filter(state_code == 'CA') %>%
  mutate(year_quarter = as.character(quarter)) %>%
  right_join(tmp) %>%
  mutate(violation_rate = violation_permit_count/active_permit_count)

#write_csv(icis.ca.comparison.permit, file.path(output.dir, 'icis_ca_violation_rate_comparison.csv'))

icis.ca.comparison.permit <- read_csv(file.path(output.dir, 'icis_ca_violation_rate_comparison.csv'))
  
plot.df <- icis.ca.comparison.permit %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  mutate(violation_type = paste0(violation_type, ' Violation'))
ggplot(plot.df, aes(x = year_quarter, y = violation_rate, group = data_source, color = data_source)) +
  geom_line()+
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 2008, y = 0.9, label = 'MMP Program Started\nin California') +
  facet_wrap(~violation_type, scales = 'free') +
  theme_classic() +
  scale_color_discrete(name = 'Data Source', labels = c('CA Waterboard (Erin)', 'ICIS Public Violation Data')) +
  theme(legend.position = 'top',
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,1) +
  labs( x = 'Quarter', 
        y = 'Percentage of Permits with at least 1 Violation', 
        title = 'California Individual NPDES Permit Violation Rate By Different Data Sources')
ggsave(file.path(figure.dir, 'ca_icis_violation_rate_comparison_using_reported_date.pdf'), width = 9, height = 5)

## check icis data only and see what data is submitted 

## parameters
plot.df <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  left_join(parameters) %>%
  mutate(year_quarter = as.numeric(year_quarter),
         SNC_FLAG = as.character(SNC_FLAG)) %>%
  group_by(year_quarter, SNC_FLAG) %>%
  summarise(violation_count = n())
ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = SNC_FLAG, color = SNC_FLAG)) +
  geom_line() +
  theme_classic() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.2, y = 250, label = 'California Data\nStarted') +
  scale_color_discrete(name = 'Pollutant Group', labels = c('I', 'II', 'III')) +
  theme(legend.position = 'top') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Violation Count')
ggsave(file.path(figure.dir, 'ca_icis_violation_volumn_by_pollutant_group.pdf'),
       width = 6, height = 4)
  
## major vs minor
plot.df <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, MAJOR_MINOR_STATUS_FLAG) %>%
  summarise(violation_count = n())
ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = MAJOR_MINOR_STATUS_FLAG, color = MAJOR_MINOR_STATUS_FLAG)) +
  geom_line() +
  theme_classic() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.2, y = 350, label = 'California Data\nStarted') +
  scale_color_discrete(name = 'Facility') +
  theme(legend.position = 'top') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Violation Count')
ggsave(file.path(figure.dir, 'ca_icis_violation_volumn_by_facility_size.pdf'),
       width = 6, height = 4)
### but what subgroup of majors?

## facility type
plot.df <- icis.ca.violation %>%
  left_join(sectors) %>%
  filter(violation_type == 'Effluent') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'Major') %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, FACILITY_TYPE_INDICATOR) %>%
  summarise(violation_count = n())
ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = FACILITY_TYPE_INDICATOR, color = FACILITY_TYPE_INDICATOR)) +
  geom_line() +
  theme_classic() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.2, y = 350, label = 'California Data\nStarted') +
  scale_color_discrete(name = 'Facility') +
  theme(legend.position = 'top') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Violation Count')
ggsave(file.path(figure.dir, 'ca_icis_violation_volumn_by_facility_size.pdf'),
       width = 6, height = 4)

# only above a certain threshold? 
tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(year_quarter < 2000)
table(tmp$RNC_DETECTION_CODE)
table(tmp$RNC_RESOLUTION_CODE)
table(tmp$STATE_WATER_BODY_NAME)
table(tmp$PRETREATMENT_INDICATOR_CODE)

# major vs minor
icis.tmp <- icis.ca.violation %>%
  rename(facility_size = MAJOR_MINOR_STATUS_FLAG, 
         individual_general_flag = PERMIT_TYPE_CODE, 
         npdes_id = NPDES_ID) %>%
  mutate(facility_size = ifelse(facility_size == 'M', 'Major', 'Minor')) %>%
  filter(violation_type %in% c('Effluent')) %>%
  filter(individual_general_flag == 'individual') %>%
  group_by(year_quarter, facility_size) %>%
  summarise(icis_count = n())
ca.tmp <- ca.violation.filt %>%
  rename(facility_size = `MAJOR-MINOR`,
         npdes_id = EXTERNAL_PERMIT_NMBR, 
         violation_type = `VIOLATION TYPE`) %>%
  filter(violation_type %in% c('Effluent')) %>%
  filter(individual_general_flag == 'individual') %>%
  group_by(year_quarter, facility_size) %>%
  summarise(ca_count = n())
tmp <- icis.tmp %>%
  full_join(ca.tmp) %>%
  filter(!is.na(facility_size)) 
write_csv(tmp, file.path(output.dir, 'view.csv'))

tmp <- read_csv(file.path(output.dir, 'view.csv'))

plot.df <- tmp %>%
  gather(source, permit_count, -c(year_quarter:facility_size))

ggplot(plot.df, aes(x = year_quarter, y = permit_count, group = source, color = source)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1994.2, y = 900, label = 'California Data\nStarted') +
  facet_wrap(~facility_size) +
  theme_classic() +
  labs(x = 'Quarter', y = 'Count', title = 'Effluent Violation Count by Facility Type') +
  scale_color_discrete(name = 'Data Source', labels = c('California Waterboard (Erin)', 'ICIS Public Data')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'ca_icis_violation_record_comparison_major_or_minor_using_report_dates.pdf'),
       width = 7, height = 4)

# the gap ... is it pollutant-based? permit-based? violation_type-based?

# within the invidual majors that have data
# check what pollutants are submitted more consistently

# subset to pollutants that had records pre-2000: there are about 80 of them (out of about 250)
icis.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(year_quarter < 2000.2) 
length(unique(icis.tmp$PARAMETER_DESC)) # 82

old.pollutant <- unique(icis.tmp$PARAMETER_DESC)

subset.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(PARAMETER_DESC %in% old.pollutant) %>%
  group_by(year_quarter) %>%
  summarise(subset_count = n()) 

full.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual')
length(unique(full.tmp$PARAMETER_DESC)) # 246

full.tmp <- full.tmp %>%
  group_by(year_quarter) %>%
  summarise(full_count = n())

tmp <- subset.tmp %>%
  full_join(full.tmp)
write_csv(tmp, file.path(output.dir, 'view.csv'))

tmp <- read_csv(file.path(output.dir, 'view.csv'))

plot.df <- tmp %>%
  gather(source, violation_count, -c(year_quarter))

ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = source, color = source)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.3, y = 350, label = 'California Data\nStarted') +
  theme_classic() + 
  labs( x = 'Quarter', y = 'Count', title = 'Effluent Violation Count of a Subgroup of Pollutants VS All Pollutants (Individual Majors)') +
  scale_color_discrete(name = 'Group', labels = c('All 246 Pollutants', '82 Pollutants that Had Pre-2000 Data')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'ca_icis_violation_pollutant_subset_using_report_dates.pdf'),
       width = 7, height = 4)
# these old pollutants are common pollutants measured consistently
# but there is still a surge of these records around 2015, so consistent reporting is not dependent on pollutant

## is it a subset of permits then?

icis.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(year_quarter < 2000.2) 
old.permits <- unique(icis.tmp$NPDES_ID)
length(old.permits) # there are 40 of them (out of all about 240 of them)

subset.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(NPDES_ID %in% old.permits) %>%
  group_by(year_quarter) %>%
  summarise(subset_count = n()) 

full.tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual')
length(unique(full.tmp$NPDES_ID)) # 241

full.tmp <- full.tmp %>%
  group_by(year_quarter) %>%
  summarise(full_count = n())

tmp <- subset.tmp %>%
  full_join(full.tmp)
write_csv(tmp, file.path(output.dir, 'view.csv'))

tmp <- read_csv(file.path(output.dir, 'view.csv'))

plot.df <- tmp %>%
  gather(source, violation_count, -c(year_quarter))
  
ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = source, color = source)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.3, y = 350, label = 'California Data\nStarted') +
  theme_classic() + 
  labs( x = 'Quarter', y = 'Count', title = 'Effluent Violation Count of a Subgroup of Permits VS All Permits (Individual Majors)') +
  scale_color_discrete(name = 'Group', labels = c('All 241 Permits', '40 Permits that Had Pre-2000 Data')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'ca_icis_violation_permit_subset_uing_report_date.pdf'),
       width = 7, height = 4)
## violation counts for the subset of permits are pretty consistent before and after 2000 
## check with California data! 

icis.tmp <-icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(MAJOR_MINOR_STATUS_FLAG == 'M') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(NPDES_ID %in% old.permits) %>%
  group_by(year_quarter) %>%
  summarise(icis_count = n()) 
ca.tmp <- ca.violation.filt %>%
  filter(`VIOLATION TYPE` == 'Effluent') %>%
  filter(`MAJOR-MINOR` == 'Major') %>%
  filter(individual_general_flag == 'individual') %>%
  filter(EXTERNAL_PERMIT_NMBR %in% old.permits) %>%
  group_by(year_quarter) %>%
  summarise(ca_count = n()) 
tmp <- icis.tmp %>%
  full_join(ca.tmp)
write_csv(tmp, file.path(output.dir, 'view.csv'))

tmp <- read_csv(file.path(output.dir, 'view.csv'))

plot.df <- tmp %>%
  gather(source, violation_count, -c(year_quarter))

ggplot(plot.df, aes(x = year_quarter, y = violation_count, group = source, color = source)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') + 
  annotate('text', x = 1996.3, y = 250, label = 'California Data\nStarted') +
  theme_classic() + 
  labs( x = 'Quarter', y = 'Count', title = 'Effluent Violation Count Comparison for a Subgroup of Individual Permits (Majors)') +
  scale_color_discrete(name = 'Data Source', labels = c('California Waterboard (Erin)', 'ICIS Public Record')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'ca_icis_violation_comparison_permit_subset_with_report_date.pdf'),
       width = 7, height = 4)
## There is some discrepency still.. CA has more violation records on 2000 Q2 but ICIS has more from 2001 to about 2012
## Is it because a delay of violation records submission? 

tmp <- ca.violation.filt %>%
  filter(`VIOLATION TYPE` == 'Effluent') %>%
  filter(`MAJOR-MINOR` == 'Major') %>%
  filter(individual_general_flag == 'individual') %>%
  filter(EXTERNAL_PERMIT_NMBR %in% old.permits) 
write_csv(tmp, file.path(output.dir, 'view.csv'))

tmp <- read_csv(file.path(output.dir, 'view.csv'))

table(tmp$`PROGRAM CATEGORY`) # NPDESWW, but most permits are in this program...
table(tmp$`Violation Source`)

df <- tmp %>%
  filter(year_quarter == 2000.2)
nrow(df)

# Discrepany remains --> Use DMR data instead --------------------------------------------------

dmr <- read_csv(file.path(output.dir, 'CA_dmrs.csv'), 
                col_type = cols(.default = 'c')) %>%
  mutate(violation_type = ifelse(VIOLATION_CODE == 'E90', 'Effluent', 'Reporting')) %>%
  mutate(monitoring_quarter = as.numeric(monitoring_quarter)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'MAJOR_MINOR_STATUS_FLAG')]) %>%
  mutate(facility_size = MAJOR_MINOR_STATUS_FLAG) %>%
  distinct()

dmr.count <- dmr %>%
  group_by(monitoring_quarter, facility_size, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_quarter, facility_size) %>%
  summarise(dmr_reporter_count = n())
write.csv(dmr.count, file.path(output.dir, 'ca_dmr_submitter_count.csv'))
dmr.count <- read_csv(file.path(output.dir, 'ca_dmr_submitter_count.csv'))

plot.df <- permit.count %>%
  filter(state_code == 'CA') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  mutate(facility_size = MAJOR_MINOR_STATUS_FLAG)

ggplot() +
  geom_bar(data = dmr.count, aes(x = monitoring_quarter, y = dmr_reporter_count, fill = facility_size), position = 'stack', stat = 'identity') +
  scale_fill_discrete(name = 'Facility') +
  geom_line(data = plot.df, aes(x = year_quarter, y = active_permit_count, source = facility_size, color = facility_size)) +
  facet_wrap(~facility_size) +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Count',
       title = 'Count of Active Permits and DMR Reporters Among Individual Permits in California',
       subtitle = 'Line: Active Permit Count\nBar: DMR Reporter Count') +
  theme_classic() +
  theme(legend.position = 'top') +
  guides(color = FALSE)
ggsave(file.path(figure.dir, 'ca_dmr_reporter_permit_count.pdf'),
       width = 6, height = 4)

plot.df <- dmr.count %>%
  filter(monitoring_quarter < 2020.2)

ggplot(plot.df, aes(x = monitoring_quarter, y = dmr_reporter_count, color = facility_size)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Facility Count', 
       title = 'Count of DMR Reporter Among Individual Permits Overtime') + 
  theme(legend.position = 'top') +
  scale_color_discrete(name = 'Facility') +
  geom_vline(xintercept = c(2000.2, 2004.2, 2016.4), linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 1996.3, y = 100, size = 3, label = '2000 Jan\nMMP Program Started\n For Effluent Violations') +
  annotate('text', x = 2008, y = 125, size = 3, label = '2004 Jan\nMMP Program Started\n For Reporting Violations') +
  annotate('text', x = 2013.2, y = 70, size = 3, label = '2016 Dec\nNPDES E-Reporting \nRule Phase I \nImplemented')
ggsave(file.path(figure.dir, 'ca_dmr_reporter_count.pdf'),
                 width = 8, height = 4)

dmr.violator.count <- dmr %>%
  filter(!(is.na(violation_type))) %>%
  group_by(monitoring_quarter, violation_type, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_violation_count = n()) %>%
  group_by(monitoring_quarter, violation_type) %>%
  summarise(dmr_violator_count = n()) 

# only keep records of DMR submitters in California data
tmp <- dmr %>%
  select(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  distinct() 

ca.violator.count <- ca.violation.filt %>%
  rename(monitoring_quarter = year_quarter) %>%
  mutate(monitoring_quarter = as.numeric(monitoring_quarter)) %>%
  inner_join(tmp) %>%
  mutate(violation_type = `VIOLATION TYPE`) %>%
  group_by(monitoring_quarter, violation_type, EXTERNAL_PERMIT_NMBR) %>%
  summarise(ca_violation_count = n()) %>%
  group_by(monitoring_quarter, violation_type) %>%
  summarise(ca_violator_count = n())

tmp <- dmr.count %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = sum(dmr_reporter_count))

plot.df <- dmr.violator.count %>%
  full_join(ca.violator.count) %>%
  right_join(tmp) %>%
  gather(data_source, violation_count, c(dmr_violator_count, ca_violator_count)) %>%
  mutate(violation_rate = violation_count/dmr_reporter_count) %>%
  filter(!is.na(violation_type))

write_csv(plot.df, file.path(output.dir, 'view.csv'))
plot.df <- read_csv(file.path(output.dir, 'view.csv'))

intercept <- data.frame(violation_type = c(rep('Effluent', 3), rep('Reporting', 3)),
                        int = rep(c(2000.2, 2004.2, 2008.2), 2))
eff.text <- data.frame(monitoring_quarter = 1995, violation_rate = 0.9, 
                       data_source = 'ca',
                       violation_type = factor('Effluent', levels = c('Effluent', 'Reporting')))
report.text <- data.frame(monitoring_quarter = c(1998, 2015), violation_rate = rep(0.85, 2),
                          data_source = rep('ca', 2),
                          violation_type = rep(factor('Reporting', levels = c('Effluent', 'Reporting')), 2))

ggplot(plot.df, aes(x = monitoring_quarter, y = violation_rate, group = data_source, color = data_source)) +
  geom_line()+
  geom_vline(aes(xintercept = int), intercept, linetype = 'dashed', color = c('grey40', rep('grey80',3), rep('grey40', 2))) + 
  geom_text(data = eff.text, color = 'black', size = 2.5,
            label = '2000\nMMP Started For\nEffluent Violations') +
  geom_text(data = report.text, color = 'black', size = 2.5,
            label = c('2004\nMMP Started For\nReporting Violations\n(not strictly enforced)',
                      '2008\nMMP Strictly Enforced\n For Reporting Violations')) +
  #annotate('text', x = 2008, y = 0.9, label = 'MMP Program Started\nin California') +
  facet_wrap(~violation_type, scales = 'free') +
  theme_classic() +
  scale_color_discrete(name = 'Data Source', labels = c('CA Waterboard (Erin)', 'ICIS Public DMR Data')) +
  theme(legend.position = 'top',
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,1) +
  labs( x = 'Monitoring Quarter (Fiscal)', 
        y = 'Percentage of Permits with at least 1 Violation', 
        title = 'California Individual NPDES Permit Violation Rate By Different Data Sources')
ggsave(file.path(figure.dir, 'ca_dmr_violation_rate_comparison.pdf'),
       width = 7, height = 4)

## --> violation rates from different sources match well when we use DMR reporter counts
## decompose violation rates between 1995-2004
tmp <- dmr %>%
  filter(monitoring_quarter > 1995 & monitoring_quarter < 2004 ) %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  mutate(period = ifelse(monitoring_quarter < 2000, 'pre-2000', 'post-2000')) %>%
  group_by(EXTERNAL_PERMIT_NMBR, period) %>%
  summarise(n = n()) 
early.reporter <- tmp$EXTERNAL_PERMIT_NMBR[tmp$period == 'pre-2000']
tmp <- tmp %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) 
constant.reporter <- tmp$EXTERNAL_PERMIT_NMBR[tmp$n == 2]

plot.df <- dmr %>%
  filter(monitoring_quarter >= 1996.2 & monitoring_quarter < 2004.2) %>%
  mutate(permit_group = ifelse(EXTERNAL_PERMIT_NMBR %in% constant.reporter, 'group 1', 
                               ifelse(EXTERNAL_PERMIT_NMBR %in% early.reporter, 'early', 'group 2'))) %>%
  filter(permit_group != 'early') %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR, permit_group) %>%
  summarise(measurement_count = n(),
            effluent_violation_count = sum(violation_type == 'Effluent', na.rm = TRUE)) %>%
  group_by(monitoring_quarter, permit_group) %>%
  summarise(dmr_reporter_count = n(),
            effluent_violator_count = sum(effluent_violation_count > 0, na.rm = TRUE)) %>%
  mutate(violation_rate = effluent_violator_count/dmr_reporter_count)
write_csv(plot.df, file.path(output.dir, 'view.csv'))
plot.df <- read_csv( file.path(output.dir, 'view.csv'))

tmp <- dmr %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n=n()) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_reporter_count = n())
plot.df.agg <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n =n()) %>%
  group_by(monitoring_quarter) %>%
  summarise(dmr_violator_count = n()) %>%
  right_join(tmp) %>%
  mutate(violation_rate = dmr_violator_count/dmr_reporter_count) %>%
  filter(monitoring_quarter >= 2000.2 & monitoring_quarter < 2004.2)
write_csv(plot.df.agg, file.path(output.dir, 'view.csv'))
plot.df.agg <- read_csv( file.path(output.dir, 'view.csv'))

ggplot() + 
  geom_line(data = plot.df, aes(x = monitoring_quarter, y = violation_rate, group = permit_group, color = permit_group)) +
  geom_line(data = plot.df.agg, aes(x = monitoring_quarter, y = violation_rate, color = 'z'),
            linetype = 'dotted') + 
  ylim(0, 1) +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permits with At Least 1 Violation',
       title = 'Effluent Violation Rate Decomposed (1996 - 2004)') +
  theme_classic() +
  scale_color_discrete(name = 'Permit', labels = c('Permits Reported \nAt Least Once \nBefore And After 2000', 
                                                   'Permits Reported \nOnly After 2000', 
                                                   'All Permits')) +
  theme(legend.position = 'top') +
  geom_vline(xintercept = 2000.2, color = 'grey40', linetype = 'dashed') +
  annotate('text', x = 1998.8, y = 0.9, label = 'MMP Program Started\n For Effluent Violations')
ggsave(file.path(figure.dir, 'ca_violation_rate_decomposed.pdf'),
       width = 6, height = 4)



## check remaining discrepancy:
ca.violation.count <- ca.violation.filt %>%
  mutate(monitoring_quarter = as.numeric(year_quarter),
         violation_type = `VIOLATION TYPE`) %>%
  group_by(monitoring_quarter, EXTERNAL_PERMIT_NMBR, violation_type) %>%
  summarise(ca_violation_count = n())

dmr.violation.count <- dmr %>%
  filter(!(is.na(violation_type))) %>%
  group_by(monitoring_quarter, violation_type, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_violation_count = n()) 

tmp <- ca.violation.count %>%
  full_join(dmr.violation.count) %>%
  filter(violation_type == 'Effluent') %>%
  filter(monitoring_quarter >= 2000.2)

write_csv(tmp, file.path(output.dir, 'view.csv'))
tmp <- read_csv( file.path(output.dir, 'view.csv'))

nrow(tmp)
plot.df <- tmp %>%
  filter(is.na(ca_violation_count) & !is.na(dmr_violation_count)) %>%
  group_by(monitoring_quarter) %>%
  summarise(n = n())
plot.df <- tmp %>%
  filter(!is.na(ca_violation_count) & is.na(dmr_violation_count)) %>%
  group_by(monitoring_quarter) %>%
  summarise(n = n())

ggplot(plot.df, aes(x = monitoring_quarter, y = n)) +
  geom_line()

ca.violation.filt.2 <- ca.violation %>%
  right_join(tmp %>%
               filter(is.na(ca_violation_count) & !is.na(dmr_violation_count))) 
write_csv(ca.violation.filt.2, file.path(output.dir, 'view.csv'))

  

# compare active permit counts and counts of DMR submitters
plot.df <- permit.count %>%
  filter(state_code == 'CA') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  rename(monitoring_quarter = year_quarter,
         facility_size = MAJOR_MINOR_STATUS_FLAG) %>%
  left_join(dmr.count[, c('monitoring_quarter', 'facility_size', 'dmr_reporter_count')])

write_csv(plot.df, file.path(output.dir, 'view.csv'))
plot.df <- read_csv(file.path(output.dir, 'view.csv'))

ggplot(plot.df, aes(x = monitoring_quarter, y = active_permit_count, fill = facility_size)) +
  geom_line() +
  facet_wrap(~facility_size) +
  theme_classic() +
  theme(legend.position = 'top') +
  scale_color_discrete(name='', labels = c('ICIS Active Permit Count', 'DMR Reporter Count')) +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Count',
       title = 'California ICIS NPDES Active Individual Permit Count and DMR Reporter Count')


# CA pre-2000 data --------------------------------------------------------

ca.violation.post2000 <- read_csv(file.path(output.dir, 'Erin Mustain - violations_export.csv'),
                         col_type = cols(.default = 'c')) %>%
  mutate(EXTERNAL_PERMIT_NMBR = `NPDES# CA#`,  
         violation_status = `STATUS.1`,
         individual_general_flag = `INDIVIDUAL/GENERAL`,
         violation_type = `VIOLATION TYPE`,
         violation_subtype = `VIOLATION SUBTYPE`,
         violation_description = `VIOLATION DESCRIPTION`,
         violation_comment = `VIOLATION COMMENTS`,
         program = `PROGRAM`,
         program_category = `PROGRAM CATEGORY`,
         violation_source = `Violation Source`,
         place_type = `PLACE SUBTYPE`,
         place_type_parent = `PLACE TYPE`,
         reg_meas_type = `REG MEAS TYPE`,
         major_minor_flag = `MAJOR-MINOR`) %>%
  mutate(npdes_flag = ifelse(program_category %in% c('NPDESWW', 'NPDESSW'), TRUE, FALSE)) %>%
  mutate(individual_general_flag = ifelse(individual_general_flag == 'I', 'individual', 'general'), 
         occurrence_date = mdy(`OCCURRED ON`),
         discovery_date = mdy(`DISCOVERY DATE`)) %>%
  mutate(year_quarter = as.character(quarter(occurrence_date, with_year = TRUE, fiscal_start = 10))) %>%
  # # remove problematic dates
  filter(year_quarter <= quarter(Sys.Date(), with_year = TRUE, fiscal_start = 10) &
           year_quarter >= 1972.1) %>%
  select(EXTERNAL_PERMIT_NMBR, npdes_flag, individual_general_flag, major_minor_flag, program, program_category, place_type, place_type_parent,
         reg_meas_type, violation_status, violation_type, violation_subtype, violation_description, violation_comment, violation_source, 
         occurrence_date, discovery_date, year_quarter)

ca.violation.pre2000 <- read.delim(file.path(data.dir.manual, 'california', 'violations_before_2000.txt'),
                                   colClasses = "character") %>%
  mutate(EXTERNAL_PERMIT_NMBR = `NPDES_NUMBER`,  
         violation_status = `VIOLATION_STATUS`,
         individual_general_flag = `INDIVIDUAL_GENERAL_FLAG`,
         violation_type = `VIOLATION_TYPE`,
         violation_subtype = `VIOLATION_SUB_TYPE`,
         violation_description = `DESCRIPTION`,
         violation_comment = `COMMENTS`,
         program = `PROGRAM`,
         program_category = `PROGRAM_CATEGORY`,
         violation_source = `SOURCE`,
         place_type = `PLACE_TYPE`,
         place_type_parent = `PLACE_TYPE_PARENT`,
         reg_meas_type = `REG_MEAS_TYPE`,
         major_minor_flag = `MAJOR_MINOR`) %>%
  mutate(npdes_flag = ifelse(program_category %in% c('NPDESWW', 'NPDESSW'), TRUE, FALSE)) %>%
  mutate(individual_general_flag = ifelse(individual_general_flag == 'I', 'individual', 'general'), 
         occurrence_date = mdy(`OCCURRENCE_DATE`),
         discovery_date = mdy(`DISCOVERY_DATE`)) %>%
  mutate(year_quarter = as.character(quarter(occurrence_date, with_year = TRUE, fiscal_start = 10))) %>%
  # group_by(`VIOLATION ID (VID)`) %>%
  # mutate(record_date = max(`OCCURRED ON`, `DISCOVERY DATE`)) %>%
  # mutate(year_quarter = as.character(quarter(`record_date`, with_year = TRUE, fiscal_start = 10))) %>%
  # # remove problematic dates
  filter(year_quarter <= quarter(Sys.Date(), with_year = TRUE, fiscal_start = 10) &
           year_quarter >= 1972.1) %>%
  select(EXTERNAL_PERMIT_NMBR, npdes_flag, individual_general_flag, major_minor_flag, program, program_category, place_type, place_type_parent,
         reg_meas_type, violation_status, violation_type, violation_subtype, violation_description, violation_comment, violation_source, 
         occurrence_date, discovery_date, year_quarter)

ca.violation.full <- ca.violation.pre2000 %>%
  rbind(., ca.violation.post2000) %>%
  filter(!is.na(EXTERNAL_PERMIT_NMBR)) %>%
  filter(EXTERNAL_PERMIT_NMBR != '') %>%
  filter(npdes_flag) %>%
  filter(violation_type %in% c('Effluent')) %>%
  filter(violation_status == 'Violation') %>%
  filter(individual_general_flag == 'individual') %>%
  filter(violation_source != 'Inspection')

# ,
# 'Basin Plan Prohibition',
# 'Calculation',
# 'Order Conditions',
# 'Other Codes',
# 'Other Requirement',
# 'Pretreatment',
# 'Receiving Water',
# 'Unauthorized Discharge',
# 'Water Quality',
# 'Monitoring'

# dmr.count <- read_csv(file.path(output.dir, 'ca_dmr_submitter_count.csv')) %>%
#   rename(year_quarter = monitoring_quarter,
#          major_minor_flag = facility_size)
# 
# tmp <- dmr %>%
#   select(monitoring_quarter, MAJOR_MINOR_STATUS_FLAG, EXTERNAL_PERMIT_NMBR) %>%
#   distinct() %>%
#   rename(year_quarter = monitoring_quarter)
# 
# ca.violation.rate <- ca.violation.full %>%
#   mutate(year_quarter = as.numeric(year_quarter)) %>%
#   group_by(year_quarter, major_minor_flag, EXTERNAL_PERMIT_NMBR) %>%
#   summarise(n = n()) %>%
#   inner_join(tmp) %>%
#   group_by(year_quarter, major_minor_flag) %>%
#   summarise(violator_count = n()) %>%
#   right_join(dmr.count[, c('year_quarter', 'major_minor_flag', 'dmr_reporter_count')]) %>%
#   mutate(violator_count = replace_na(violator_count, 0)) %>%
#   mutate(violation_rate = violator_count/dmr_reporter_count)
# 
# write_csv(ca.violation.rate, file.path(output.dir, 'ca_data_violation_rate.csv'))
# ca.violation.rate <- read_csv(file.path(output.dir, 'ca_data_violation_rate.csv'))
# 
# ggplot(ca.violation.rate, aes(x = year_quarter, y = violation_rate, source = major_minor_flag, color = major_minor_flag)) +
#   geom_line() +
#   geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
#   annotate('text', x = 2006, y = 0.5, label = 'MMP Program Started\nin California') +
#   theme_classic() +
#   scale_color_discrete(name = 'Facility') +
#   theme(legend.position = 'top') +
#   labs(x = 'Monitoring Quarter (Fiscal)',
#        y = 'Proportion of Permits with At Least 1 Violation',
#        title = 'Effluent Violation Rate Among Individual Permits (CA Waterboard Data)')


dmr.count <- read_csv(file.path(output.dir, 'ca_dmr_submitter_count.csv')) %>%
  rename(year_quarter = monitoring_quarter,
         major_minor_flag = facility_size) %>%
  group_by(year_quarter) %>%
  summarise(dmr_reporter_count = sum(dmr_reporter_count))

tmp <- dmr %>%
  select(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  distinct() %>%
  rename(year_quarter = monitoring_quarter) %>%
  mutate(submitted_dmr = TRUE)

ca.violator.count <- ca.violation.full %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  full_join(tmp) %>%
  group_by(year_quarter) %>%
  summarise(ca_violator_count = n()) 

dmr.violator.count <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  rename(year_quarter = monitoring_quarter) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(dmr_violation_count = n()) %>%
  group_by(year_quarter) %>%
  summarise(dmr_violator_count = n()) 

violation.rate <- ca.violator.count %>%
  full_join(dmr.violator.count) %>%
  right_join(dmr.count[, c('year_quarter', 'dmr_reporter_count')]) %>%
  gather(source, violator_count, c('ca_violator_count', 'dmr_violator_count')) %>%
  #mutate(violator_count = replace_na(violator_count, 0)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

write_csv(violation.rate, file.path(output.dir, 'ca_eff_violation_rate.csv'))
violation.rate <- read_csv(file.path(output.dir, 'ca_eff_violation_rate.csv')) 

ggplot(violation.rate, aes(x = year_quarter, y = violation_rate, group = source, color = source)) +
  geom_line() +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 2006, y = 0.75, label = 'MMP Program Started\nin California') +
  theme_classic() +
  ylim(0,1) +
  scale_color_discrete(name = 'Data Source',
                       labels = c('CA Waterboard (Erin)', 'ICIS Public Violation Data')) +
  theme(legend.position = 'top') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permits with At Least 1 Violation',
       title = 'Effluent Violation Rate Among Individual Permits')
ggsave(file.path(figure.dir, 'ca_icis_eff_violation_rate_comparison.pdf'),
       width = 6, height = 4)

## check sample discrepancy before 2000
## check permits that were included in the DMRs but not in the CA data

dmr.tmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  filter(monitoring_quarter < 2000.2) %>%
  rename(year_quarter = monitoring_quarter) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR, violation_type) %>%
  summarise(dmr_violation_count = n()) 

ca.tmp <- ca.violation.pre2000 %>%
  filter(!is.na(EXTERNAL_PERMIT_NMBR)) %>%
  filter(EXTERNAL_PERMIT_NMBR != '') %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR, violation_type) %>%
  summarise(ca_violation_count = n()) 

discrepancy <- dmr.tmp %>%
  full_join(ca.tmp)
write_csv(discrepancy, file.path(output.dir, 'ca_dmr_pre2000_data_discrepancy.csv'))
discrepancy <- read_csv(file.path(output.dir, 'ca_dmr_pre2000_data_discrepancy.csv'))

permit <- 'CA0048160'
quarter <- '1998.1'
dmr.tmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  filter(EXTERNAL_PERMIT_NMBR == permit) %>%
  filter(monitoring_quarter == quarter)
write_csv(dmr.tmp, file.path(output.dir, 'dmr_discrepancy_sample.csv'))
ca.tmp <- ca.violation.pre2000 %>%
  filter(EXTERNAL_PERMIT_NMBR == permit) 
write_csv(ca.tmp, file.path(output.dir, 'ca_discrepancy_sample.csv'))

ca.tmp <- read_csv(file.path(output.dir, 'ca_discrepancy_sample.csv'), col_types=cols(.default='c'))
dmr.tmp <- read_csv(file.path(output.dir, 'dmr_discrepancy_sample.csv'))

## are all discrepancies because of values received late? 
dmr <- dmr %>%
  mutate(monitoring_period_end_date = ymd(MONITORING_PERIOD_END_DATE),
         value_received_date = mdy(VALUE_RECEIVED_DATE)) %>%
  mutate(calculated_days_late = value_received_date - monitoring_period_end_date) %>%
  mutate(late_reporting = calculated_days_late>0) 

dmr.tmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  filter(monitoring_quarter < 2000.2) %>%
  rename(year_quarter = monitoring_quarter) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR, violation_type, late_reporting) %>%
  summarise(dmr_violation_count = n()) 

ca.tmp <- ca.violation.pre2000 %>%
  filter(!is.na(EXTERNAL_PERMIT_NMBR)) %>%
  filter(EXTERNAL_PERMIT_NMBR != '') %>%
  filter(individual_general_flag == 'individual') %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR, violation_type) %>%
  summarise(ca_violation_count = n()) 

discrepancy <- dmr.tmp %>%
  full_join(ca.tmp)
write_csv(discrepancy, file.path(output.dir, 'ca_dmr_pre2000_data_discrepancy.csv'))
discrepancy <- read_csv(file.path(output.dir, 'ca_dmr_pre2000_data_discrepancy.csv'))
## most discrepancies were late DMR values! There were a few cases where the discrepancies weren't late reported. Could ask Erin/Beth about those.
## calculate new violation rates with ca violation count adjusted 

dmr.violation.count <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  rename(year_quarter = monitoring_quarter) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR, late_reporting) %>%
  summarise(dmr_violation_count = n())

tmp <- dmr %>%
  select(monitoring_quarter, EXTERNAL_PERMIT_NMBR) %>%
  distinct() %>%
  rename(year_quarter = monitoring_quarter)

ca.violation.count <- ca.violation.full %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(year_quarter, EXTERNAL_PERMIT_NMBR) %>%
  summarise(ca_violation_count = n()) %>%
  right_join(tmp)

violation.count <- dmr.violation.count %>%
  full_join(ca.violation.count) %>%
  mutate(adjusted_ca_violation_count = ifelse(!is.na(dmr_violation_count) & 
                                                is.na(ca_violation_count) &
                                                late_reporting, 1, ca_violation_count))
write_csv(violation.count, file.path(output.dir, 'view.csv'))
violation.count <- read_csv(file.path(output.dir, 'view.csv'))

violation.rate <- violation.count %>%
  group_by(year_quarter) %>%
  summarise(dmr_reporter_count = n(),
            dmr_violator_count = sum(!is.na(dmr_violation_count)),
            ca_violator_count = sum(!is.na(ca_violation_count)),
            adjusted_ca_violator_count = sum(!is.na(adjusted_ca_violation_count))) %>%
  gather(source, violator_count, c(dmr_violator_count, ca_violator_count, adjusted_ca_violator_count)) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)

plot.df1 <- violation.rate %>%
  filter(year_quarter > 1996.1) %>%
  filter(source %in% c('adjusted_ca_violator_count', 'dmr_violator_count'))

plot.df2 <- violation.rate %>%
  filter(year_quarter > 1996.1) %>%
  filter(source == 'ca_violator_count')

ggplot() +
  geom_line(data = plot.df1, aes(x = year_quarter, y = violation_rate, group = source, color = source)) +
  geom_line(data = plot.df2, aes(x = year_quarter, y = violation_rate, group = source, color = source),
            linetype = 'dashed') +
  geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
  annotate('text', x = 2007, y = 0.9, label = 'MMP Program Started\nin California') +
  labs(x = 'Monitoring Quarter (Fiscal)',
       y = 'Proportion of Permits with At Least 1 Violation',
       title = 'Comparison of Effluent Violation Rates') +
  theme_classic() +
  scale_color_manual(#scale_name = 'Data Source',
                     labels = c('Adjusted CA Waterboard',
                                'CA Waterboard',
                                'ICIS DMRs'),
                     values = c('#F8766D', 'grey40', '#00BFC4')) +
  theme(legend.position = 'top')
ggsave(file.path(figure.dir, 'ca_dmr_violation_rate_comparison_adjusted.pdf'),
       width = 6, height = 4)
  
# Control group -----------------------------------------------------------

# 1) plot count of dmr reporters over time
dmr.record <- read_csv(file.path(output.dir, 'dmr_eff_submission_records_2020-07-20_11-52-43.csv')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'MAJOR_MINOR_STATUS_FLAG')]) %>%
  rename(major_minor_flag = MAJOR_MINOR_STATUS_FLAG) %>%
  distinct()

dmr.reporter.count <- dmr.record %>%
  group_by(state_code, year_quarter, major_minor_flag) %>%
  summarise(permit_count = n()) 
write_csv(dmr.reporter.count, file.path(output.dir, 'dmr_eff_reporter_count.csv'))
dmr.reporter.count <- read_csv(file.path(output.dir, 'dmr_eff_reporter_count.csv'))

## subset to states that have data since 1996
tmp <- dmr.reporter.count %>%
  group_by(state_code) %>%
  summarise(earliest_quarter = min(year_quarter, na.rm = TRUE)) %>%
  filter(earliest_quarter < 1996)
early.states <- tmp$state_code
plot.df <- dmr.reporter.count %>%
  filter(state_code %in% early.states)

ggplot(plot.df, aes(x = year_quarter, y = permit_count, group = major_minor_flag, color = major_minor_flag)) +
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
ggsave(file.path(figure.dir, 'dmr_reporter_count_by_state.pdf'),
       width = 15, height = 10)

# 2) plot time series of effluent violation rate by state
tmp <- dmr.reporter.count %>%
  group_by(state_code, year_quarter) %>%
  summarise(dmr_reporter_count = sum(permit_count))
violation.rate <- violation %>%
  filter(violation_type == 'Effluent') %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'PERMIT_TYPE_CODE')], by = c('NPDES_ID' = 'EXTERNAL_PERMIT_NMBR')) %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  group_by(state_code, year_quarter, NPDES_ID) %>%
  summarise(n = n()) %>%
  group_by(state_code, year_quarter) %>%
  summarise(violator_count = n()) %>%
  full_join(tmp) %>%
  mutate(violation_rate = violator_count/dmr_reporter_count)
write_csv(violation.rate, file.path(output.dir, 'eff_violation_rate_by_state.csv'))
violation.rate <- read_csv(file.path(output.dir, 'eff_violation_rate_by_state.csv'))

plot.df <- violation.rate %>%
  filter(state_code %in% early.states)

tmp <- plot.df %>%
  filter(state_code == 'CA') %>%
  rename(ca_violation_rate = violation_rate) %>%
  select(year_quarter, ca_violation_rate)
plot.df <- plot.df %>%
  full_join(tmp) %>%
  filter(state_code != 'CA')
  
ggplot(plot.df, aes(x = year_quarter)) +
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
ggsave(file.path(figure.dir, 'violation_rate_comparison_by_state.pdf'),
       width = 15, height = 10)




# Preliminary Diff-in-Diff ----------------------------------------------------
 
# 1) which states have similar trend with California pre-2000 but diverged after 2000? 

run_did <- function(df, states){
  # create data for diff and diff regression
  # use three years before and after 2000 
  did.estimate <- c(NA, NA)
  tryCatch({
    did.data <- df %>%
      filter(state_code %in% states) %>%
      filter(quarter >= 2000.1 - 3 & quarter <= 2000.1 + 3) %>%
      mutate(treatment = ifelse(state_code == 'CA', 1, 0)) %>%
      mutate(intervention = ifelse(quarter > 2000.1, 1, 0)) 
    
    did <- lm(effluent_rate ~ treatment*intervention, data = did.data) %>%
      summary()
    did.estimate <- did$coefficients['treatment:intervention', c('Estimate', 'Pr(>|t|)')] %>%
      round(., 3) %>%
      as.numeric()
  }, error = function(e){
    e
  })
  did.estimate
}

all.states <- unique(violation.rate$state_code) 
state.pairs <- list()
for (i in 1:length(all.states)){
  state.pairs[[i]] <- c('CA', all.states[i])
}
did.results <- lapply(state.pairs, run_did, df = violation.rate) %>%
  do.call(rbind, .) %>%
  data.frame()
names(did.results) <- c('did_estimate', 'p')
did.results$state_code <- all.states
did.results <- did.results %>%
  select(state_code, did_estimate, p)

# select state candidate: significant (at the 0.05 level) negative effect
did.results.present <- did.results %>%
  filter(did_estimate < 0 & p < 0.05) %>%
  arrange(did_estimate) 
did.candidates <- did.results.present$state_code

# plot candidates
plot_violation_rate_comparison <- function(df, states){
  message(states)
  
  tryCatch({
    did.estimate <- run_did(df, states)
    
    plot.df <- df %>%
      filter(state_code %in% states) %>%
      filter(PERMIT_TYPE_CODE == 'individual') %>%
      mutate(color = ifelse(state_code == 'CA', '0', '1'))
    
    save.folder <- file.path(figure.dir, 'violation_rate_comparison')
    if (!(file.exists(save.folder))){
      dir.create(save.folder)
    }
    
    ggplot(plot.df, aes(x = quarter, y = effluent_rate, group = state_code, color = color)) +
      geom_line() +
      geom_vline(xintercept = 2000.2, linetype = 'dashed', color = 'grey40') +
      annotate('text', x = 2007, y = 0.9, label = 'MMP Program Started\nin California') +
      theme_classic() +
      scale_color_discrete(name = 'State', labels = states) +
      theme(legend.position = 'top',
            plot.title = element_text(hjust = 0.5)) +
      ylim(0, 1) +
      labs(x = 'Quarter', 
           y = 'Percentage of Permits with at least 1 Violation', 
           title = 'Individual NPDES Permit Effluent Violation Rate By State',
           subtitle = paste0('Differences-in-Differences Estimate: ', did.estimate[1], ' (p value = ', did.estimate[2], ')'))
    ggsave(file.path(save.folder, paste0(paste(states, collapse = '_'),  '_violation_rate_comparison.pdf')),
           width = 6, height = 4)
  }, error = function(e){
    e
  })
  
}

# test
plot_violation_rate_comparison(violation.rate, c('CA','TX'))

# generate comparison graphs for candidate states vs CA
# add on "similar" states: geographically close or EPA with more capacity 
did.candidates <- c(did.candidates, 'NY', 'NV', 'AZ', 'OR')
state.pairs <- list()
for (i in 1:length(did.candidates)){
  state.pairs[[i]] <- c('CA', did.candidates[i])
}

lapply(state.pairs, plot_violation_rate_comparison, df = violation.rate)

# 2) which candidates are more similar with CA? 
tmp <- violation.rate %>%
  filter(state_code %in% c('CA', did.candidates)) %>%
  filter(quarter == 2000.2) %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  left_join(did.results.present) %>%
  arrange(active_permit_count)
write_csv(tmp, file.path(output.dir, 'view.csv'))

















