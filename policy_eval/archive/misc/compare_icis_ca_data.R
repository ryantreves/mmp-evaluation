
####################
# CA MMP Analysis
## Violation rate comparison between ICIS data and California data
####################

# compare violation records between ICIS and California data and resolve any discrepancies

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

# read data ---------------------------------------------------------------

# 1) ICIS
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

# active permit count: output using output_eff_dmr_submitters.R
permit.count <- read_csv(file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

# reference tables
states.code <- read_csv(file.path(data.dir.manual, 'REF_STATISTICAL_BASE.csv'), col_types = cols(.default='c'))
parameters <- read_csv(file.path(data.dir.manual, 'REF_PARAMETER.csv')) %>%
  select(-c(CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE)) %>%
  mutate(PARAMETER_CODE = str_pad(PARAMETER_CODE, 5, pad = '0')) %>%
  # identify all non Type 1 and 2 pollutants to be Type 3 pollutants
  mutate(SNC_FLAG = ifelse(is.na(SNC_FLAG), 3, SNC_FLAG)) 

# violation records
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
         violation_type = ifelse(VIOLATION_CODE %in% c('D80', 'D90'), 'Reporting', 'Effluent')) %>%
  left_join(states.code) %>%
  # remove data errors
  filter(MONITORING_PERIOD_END_DATE <= Sys.Date())

# CA DMR data: output using output_ca_dmr.R
dmr <- read_csv(file.path(output.dir, 'CA_dmrs_2020-07-07_14-57-43.csv'), 
                col_type = cols(.default = 'c')) %>%
  mutate(violation_type = ifelse(VIOLATION_CODE == 'E90', 'Effluent', 'Reporting')) %>%
  mutate(monitoring_quarter = as.numeric(monitoring_quarter)) %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'MAJOR_MINOR_STATUS_FLAG')]) %>%
  mutate(facility_size = MAJOR_MINOR_STATUS_FLAG) %>%
  distinct()

# 2) California data
# raw post-2000 data (received 06/18/2020)
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

ca.violation.filt <- ca.violation %>%
  #filter(violation_status == 'Violation') %>%
  filter(npdes_flag) %>%
  filter(`VIOLATION TYPE` %in% c('Effluent', 'Reporting')) %>%
  # remove SEVs because it has been reported that California doesn't submit SEVs to ICIS
  # state review framework 2011 & 2016: https://echo.epa.gov/oversight/state-review-framework/tracker-recommendations/results?round=&state=CA&completion_status=&region=&media=&print=detail&element=&format=html
  filter(`Violation Source` %in% c('eSMR', 'Report'))

# joining pre-2000 and post-2000 data (pre-2000 data received 07/13/2020)
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

# post-2000: output from Vincent's notebook (savings in output folder because I (Nicole) have no permission to write the california data folder)
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

# join pre-2000 and post-2000
ca.violation.full <- ca.violation.pre2000 %>%
  rbind(., ca.violation.post2000) %>%
  # subset to NPDES individual permits only
  filter(!is.na(EXTERNAL_PERMIT_NMBR)) %>%
  filter(EXTERNAL_PERMIT_NMBR != '') %>%
  filter(npdes_flag) %>%
  filter(individual_general_flag == 'individual') %>%
  # subset to effluent violations only
  filter(violation_type %in% c('Effluent')) %>%
  filter(violation_status == 'Violation') %>%
  # remove violations discovered from inspections (SEVs) 
  # remove SEVs because it has been reported that California doesn't submit SEVs to ICIS
  # state review framework 2011 & 2016: https://echo.epa.gov/oversight/state-review-framework/tracker-recommendations/results?round=&state=CA&completion_status=&region=&media=&print=detail&element=&format=html
  filter(violation_source != 'Inspection')

# Analysis ------------------------------------------------------------------

# 1) check icis data using permit count as denominators -------------------

violation.count <- violation %>%
  left_join(permits[, c('EXTERNAL_PERMIT_NMBR', 'PERMIT_TYPE_CODE', 'MAJOR_MINOR_STATUS_FLAG')], by = c('NPDES_ID' = 'EXTERNAL_PERMIT_NMBR')) %>%
  group_by(year_quarter, state_code, violation_type, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG, NPDES_ID) %>%
  summarise(n = n()) %>%
  group_by(year_quarter, state_code, violation_type, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
  summarise(permit_count = n()) %>%
  spread(violation_type, permit_count)

violation.rate <- violation.count %>% 
  right_join(permit.count) %>%
  select(quarter, state_code, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG, active_permit_count, effluent, reporting) %>%
  mutate_all(replace_na, 0) %>%
  mutate(violation_count = effluent + reporting,
         effluent_rate = effluent/active_permit_count,
         reporting_rate = reporting/active_permit_count,
         violation_rate = violation_count/active_permit_count) 

plot.df <- violation.rate %>%
  filter(effluent_rate <= 1) %>%
  filter(state_code == 'CA') %>%
  filter(PERMIT_TYPE_CODE == 'individual')

ggplot(plot.df, aes(x = quarter, y = effluent_rate, color = state_code, group = state_code)) +
  geom_line()
# ==> there is a huge discrepency between the plot produced by Vincent, shown in the "ca_minimum_mandatory_penalty.ipynb" notebook
# ==> check if the ICIS violation data for California is complete

# 2) check data completeness ----------------------------------------------
icis.ca.violation <- violation %>%
  filter(state_code == 'CA') %>%
  mutate(EXTERNAL_PERMIT_NMBR = NPDES_ID) %>%
  left_join(permits)

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
# ==> 5527/(5527+86544) = 6% of records
# ==> without primary key, it is very difficult to match ICIS and CA data 


# 3) compare violation count for post-2000 data ---------------------------
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

ggplot(icis.ca.comparison, aes(x = year_quarter, y = violation_count, group = data_source, color = data_source)) +
  geom_line()+
  facet_wrap(~violation_type+PERMIT_TYPE_CODE, scales = 'free') +
  theme_classic() +
  scale_color_discrete(name = 'Data Source', labels = c('CA Waterboard Erin', 'ICIS Public Violation Data')) +
  theme(legend.position = 'top') +
  labs( x = 'Quarter', y = 'Violation Count', title = 'California NPDES Violation Count By Different Data Sources')
ggsave(file.path(figure.dir,'ca_npdes_violation_count_by_data_sources.png'), width = 7, height = 5)

# ==> there seems to be a huge discrepancy between CA data and ICIS data...
# ==> Effluent violaiton records largely match for different data sources, but only barely
# ==> it is possible that the unit of violation records is different for the California data and ICIS data
# ==> we will plot violation rate instead

# 4) compare violation rate for post-2000 data ----------------------------
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
ggsave(file.path(figure.dir, 'ca_icis_violation_rate_comparison.pdf'), width = 9, height = 5)

# ==> large discrepany remains
# ==> data discrepany analysis

# 5) data discrepancy - check icis data only and see what data is submitted  -----------------
## a) parameters
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
## ==> violation rates match with each other across groups
## ==> unlikely due to parameter groups

## b) major vs minor
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
## ==> majors track each other much better
## ==> but what subgroup of majors?

## c) facility type
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
## ==> there is no clear pattern

## d) other patterns

tmp <- icis.ca.violation %>%
  filter(violation_type == 'Effluent') %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  filter(year_quarter < 2000)
table(tmp$RNC_DETECTION_CODE)
table(tmp$RNC_RESOLUTION_CODE)
table(tmp$STATE_WATER_BODY_NAME)
table(tmp$PRETREATMENT_INDICATOR_CODE)

# 6) data discrepancy - plot gaps of submissions  --------------------------------------------
## a)  major vs minor
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
ggsave(file.path(figure.dir, 'ca_icis_violation_record_comparison_major_or_minor.pdf'),
       width = 7, height = 4)

# the gap ... is it pollutant-based? permit-based? violation_type-based?

# within the invidual majors that have data
# check what pollutants are submitted more consistently

## b) subset to pollutants that had records pre-2000: there are about 80 of them (out of about 250)
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
ggsave(file.path(figure.dir, 'ca_icis_violation_pollutant_subset.pdf'),
       width = 7, height = 4)
## ==> these old pollutants are common pollutants measured consistently
## ==> but there is still a surge of these records around 2015, so consistent reporting is not dependent on pollutant
## ==> is it a subset of permits then?

## c) early permits
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
ggsave(file.path(figure.dir, 'ca_icis_violation_permit_subset.pdf'),
       width = 7, height = 4)
## ==> violation counts for the subset of permits are pretty consistent before and after 2000 
## ==> check with California data! 

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
ggsave(file.path(figure.dir, 'ca_icis_violation_comparison_permit_subset.pdf'),
       width = 7, height = 4)
## ==> There is some discrepency still.. CA has more violation records on 2000 Q2 but ICIS has more from 2001 to about 2012
## ==> check what commonalities early reporters have

tmp <- ca.violation.filt %>%
  filter(`VIOLATION TYPE` == 'Effluent') %>%
  filter(`MAJOR-MINOR` == 'Major') %>%
  filter(individual_general_flag == 'individual') %>%
  filter(EXTERNAL_PERMIT_NMBR %in% old.permits) 
table(tmp$`PROGRAM CATEGORY`) # NPDESWW, but most permits are in this program...
table(tmp$`Violation Source`)

df <- tmp %>%
  filter(year_quarter == 2000.2)
nrow(df)

## ==> The discrepancy still doesn't make sense
## ==> These discrepanies might be due to changing reporting requirements 
## ==> Conditional on those who submitted instead 
## ==> New denominator: Count of DMR submitters

# 7) Use count of DMR reporters as denominator ----------------------------

dmr.count <- dmr %>%
  group_by(monitoring_quarter, facility_size, EXTERNAL_PERMIT_NMBR) %>%
  summarise(n = n()) %>%
  group_by(monitoring_quarter, facility_size) %>%
  summarise(dmr_reporter_count = n())

## a) plot dmr reporter count against active permit count
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
## ==> It is strange that the number of active permits is smaller than DMR reporters recently
## ==> This might be due to the filtering criteria discrepancy 

## b) plot annotated DMR reporter count plot

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
## ==> There is not a sharp jump in DMR reporters around 2000, which is good news
## ==> Though there is a slight drop righat in the start of 2000. We should investigate this further.

## c) Plot violation comparison graph
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
## ==> violation rates from different sources match well when we use DMR reporter counts
## ==> decompose violation rates between 1995-2004 to make sure 
## the drop of violation rates around 2000 is not caused by the additional facilities

# 8) decompose violation rates around 2000 using DMR data -----------------
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
## ==> After 2000, the overall violation rate does drop.
## ==> the result is not driven by facilities that newly reported after 2000
## ==> the two subgroups’ violation rates seem to track each other post-2000

# 9) compare DMR data and CA data violation rate (with pre-2000 data) -----------------------------

dmr.count <- dmr.count %>%
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
## ==> huge discrepancy before 2000
## ==> what cause the discrepancy??


# 10) check sample of discrepancy: permits in DMR but not CA --------------

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

permit <- 'CA0048160'
quarter <- '1998.1'

dmr.tmp <- dmr %>%
  filter(violation_type == 'Effluent') %>%
  filter(EXTERNAL_PERMIT_NMBR == permit) %>%
  filter(monitoring_quarter == quarter)

ca.tmp <- ca.violation.pre2000 %>%
  filter(EXTERNAL_PERMIT_NMBR == permit) 

## ==> tracking down individual records reveals that values that were reported late might not get to CA data
## ==> or the event was marked as a reporting violation
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
## ==> most discrepancies were late DMR values! There were a few cases where the discrepancies weren't late reported. Could ask Erin/Beth about those.
## ==> calculate new violation rates with ca violation count adjusted 

# 11) Adjust CA data accounting for late DMR values -----------------------
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
## ==> discrepancies largely disappear and both pre-2000 and post-2000 data track each other quite well
## ==> puzzle solved!

