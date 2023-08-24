

####################
# CA MMP Analysis
## output motivation tables and figures for paper
####################

# 1. magnitude of effluent violations
# 2. current enforcement strategies

# set up ------------------------------------------------------------------

# OAK_DIR <- Sys.getenv('OAK')
# setwd(file.path(OAK_DIR, 'EPA'))
setwd('~/sherlock_epa/')

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 
       'lubridate', 
       'stringr', 
       'tidyverse', 
       'stargazer', 
       'ggpubr', 
       'ggplot2',
       'ggmap',
       'RPostgres',
       'DBI'
       )

# analysis folder
analysis_dir <- file.path('Analysis', 'ca_mmp')
tab_dir <- file.path(analysis_dir, 'tables')
fig_dir <- file.path(analysis_dir, 'figures')


# connect to the AWS database
# for instructions on how to save credentials as environment variables: https://asconfluence.stanford.edu/confluence/display/REGLAB/PostgreSQL+Database#PostgreSQLDatabase-ManagingUsersandRoles
db_name <- Sys.getenv('EPA_DB_NAME')
db_host <- Sys.getenv('EPA_DB_HOST')
db_port <- Sys.getenv('EPA_DB_PORT')
db_username <- Sys.getenv('EPA_DB_USER')
db_password <- Sys.getenv('EPA_DB_PASSWORD')

con <- dbConnect(RPostgres::Postgres(),
                 dbname=db_name,
                 host=db_host,
                 port=db_port ,
                 user=db_username,
                 password=db_password)

# set colors
colors <- c('#E6945C', '#5C9FE6', '#00C274', '#D35CE6', '#E65C5C')


# 1. magnitude of effluent violations -------------------------------------

# 1) violation type breakdown ---------------------------------------------
## effluent violations consistently rampent

query <- "
with active_permits as(
select 
  extract(year from time_period_begin) as year, 
  npdes_permit_id
from analytics.permit_active_status_hist
  where time_period_desc = 'year'
  and permit_active_flag
),

permit_types as(
select distinct
  npdes_permit_id,
  individual_permit_flag,
  major_permit_flag,
  wastewater_permit_flag,
  stormwater_permit_flag,
  sewage_permit_flag
from icis.permits
), 

locations as(
 select npdes_permit_id, 
    geocode_latitude,
    geocode_longitude
 from icis.facilities
),

dmr_violation as(
select 
  extract(year from monitoring_period_end_date) as year, 
  npdes_id as npdes_permit_id,
  count(CASE WHEN violation_code = 'E90' THEN 1 END) as effluent_violation_count,
  count(CASE WHEN violation_code in ('D80', 'D90') THEN 1 END) as reporting_violation_count
from data_ingest.icis__npdes_eff_violations
group by year, npdes_permit_id
),

cs_violation as(
select 
  extract(year from schedule_date) as year,
  npdes_id as npdes_permit_id,
  count(*) as compliance_schedule_violation_count
from data_ingest.icis__npdes_cs_violations
group by year, npdes_permit_id
),

ps_violation as(
select 
  extract(year from schedule_date) as year,
  npdes_id as npdes_permit_id,
  count(*) as permit_schedule_violation_count
from data_ingest.icis__npdes_ps_violations
group by year, npdes_permit_id
)

select *,
  case when effluent_violation_count > 0 then true else false end as effluent_violation_flag,
  case when reporting_violation_count > 0 then true else false end as reporting_violation_flag,
  case when compliance_schedule_violation_count > 0 then true else false end as compliance_schedule_violation_flag,
  case when permit_schedule_violation_count > 0 then true else false end as permit_schedule_violation_flag
from active_permits
left join permit_types
using (npdes_permit_id)
left join locations
using (npdes_permit_id)
left join dmr_violation
using (year, npdes_permit_id)
left join cs_violation
using (year, npdes_permit_id)
left join ps_violation
using (year, npdes_permit_id)
"

auto_violation <- dbSendQuery(con, query) %>%
  dbFetch()

# # check wether year, permit is unique
# tmp <- auto_violation %>%
#   select(year, npdes_permit_id) %>%
#   distinct()

## get single event violations
query <- "
select 
  extract (year from single_event_violation_date) as year,
  npdes_id as npdes_permit_id,
  violation_code,
  violation_desc,
  single_event_violation_comment
from data_ingest.icis__npdes_se_violations
"

manual_violation <- dbSendQuery(con, query) %>%
  dbFetch()

# distinguish types: effluent, reporting/monitoring, compliance schedule, permit schedule
manual_violation <- manual_violation %>%
  mutate(man_effluent_violation_flag = str_detect(violation_desc, fixed('effluent', ignore_case =  TRUE)) | str_detect(single_event_violation_comment, fixed('effluent', ignore_case =  TRUE))) %>%
  mutate(man_reporting_violation_flag = str_detect(violation_desc, fixed('reporting', ignore_case =  TRUE)) | str_detect(single_event_violation_comment, fixed('reporting', ignore_case =  TRUE))) %>%
  mutate(man_compliance_schedule_flag = str_detect(violation_desc, fixed('compliance schedule', ignore_case = TRUE)) | str_detect(single_event_violation_comment, fixed('compliance schedule', ignore_case = TRUE))) %>%
  mutate(man_permit_schedule_flag = str_detect(violation_desc, fixed('permit schedule', ignore_case = TRUE)) | str_detect(single_event_violation_comment, fixed('permit schedule', ignore_case = TRUE))) 

violation <-   auto_violation %>%
  left_join(manual_violation[, c('year', 'npdes_permit_id', 'man_effluent_violation_flag', 'man_reporting_violation_flag', 'man_compliance_schedule_flag', 'man_permit_schedule_flag')]) 

sample_violation <- violation %>%
  filter(!is.na(geocode_latitude)) %>%
  filter(individual_permit_flag == 1) %>%
  filter(major_permit_flag == 1) %>%
  filter(wastewater_permit_flag == 1) %>%
  filter(sewage_permit_flag == 1) %>%
  filter(between(year, 2000, 2019))

#sum(is.na(sample_violation$geocode_latitude))/nrow(sample_violation) # 0.03% missing geo location data: remove

plot_df <- sample_violation %>%
  group_by(year) %>%
  summarise(active_permit_count = n(),
            effluent_violator_count = sum(effluent_violation_flag | man_effluent_violation_flag, na.rm = TRUE),
            reporting_violator_count = sum(reporting_violation_flag | man_reporting_violation_flag, na.rm = TRUE),
            compliance_schedule_violator_count = sum(compliance_schedule_violation_flag |man_compliance_schedule_flag, na.rm = TRUE),
            permit_schedule_violator_count = sum(permit_schedule_violation_flag | man_permit_schedule_flag, na.rm = TRUE)) %>%
  gather(violation_type, violator_count, -c(year:active_permit_count)) %>%
  mutate(violation_rate = violator_count/active_permit_count)

ggplot() +
  geom_line(data = plot_df, aes(x = year, 
                         y = violation_rate,
                         color = violation_type)) +
  geom_point(data = plot_df[plot_df$violation_type == 'effluent_violator_count',], 
             aes(x = year, 
                 y = violation_rate,
                 size = active_permit_count),
             color = colors[5]) +
  labs(x = 'Monitoring Year (2000-2019)',
       y = 'Proportion of Permits With At Least One Violation') +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = colors[c(1,5,3,2)],
                     guide = FALSE) +
  scale_size_continuous(name = 'Active Permit Count') +
  annotate('text', x = 2013, y = 0.52, color = colors[5], label = paste0('Effluent Violations (20 Years Average: ', round(mean(plot_df[plot_df$violation_type == 'effluent_violator_count',]$violation_rate)*100), '%)')) +
  annotate('text', x = 2013, y = 0.27, color = colors[2], label = paste0('Reporting Violations (20 Years Average: ', round(mean(plot_df[plot_df$violation_type == 'reporting_violator_count',]$violation_rate)*100), '%)')) +
  annotate('text', x = 2013, y = 0.16, color = colors[3], label = paste0('Permit Violations (20 Years Average: ', round(mean(plot_df[plot_df$violation_type == 'permit_schedule_violator_count',]$violation_rate)*100), '%)')) +
  annotate('text', x = 2013, y = 0.02, color = colors[1], label = paste0('Compliance Violations (20 Years Average: ', round(mean(plot_df[plot_df$violation_type == 'compliance_schedule_violator_count',]$violation_rate)*100), '%)')) +
  theme_bw() +
  theme(legend.position = 'top')
ggsave(file.path(fig_dir, 'violation_rate_by_type.pdf'),
       width = 6, height = 5)


# 2) violators map -----------------------------------------------------------

register_google(key = Sys.getenv('SNC_MAP_API'))

us <- c(left = -128, bottom = 25, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

plot_df <- sample_violation %>%
  filter(between(year, 2015, 2019)) %>%
  group_by(npdes_permit_id, geocode_latitude, geocode_longitude) %>%
  summarise(five_years_effluent_violation = sum(effluent_violation_flag, na.rm = TRUE) > 0)

ggmap(map)+
  geom_point(data = plot_df,
             aes(x = geocode_longitude, y = geocode_latitude, color = five_years_effluent_violation),
             size = 1, 
             alpha = 0.4) +
  scale_color_manual(values = c(colors[3], colors[5])) +
  labs(x ='',
       y = '') +
  annotate('text', x= -115.5, y= 26, 
           label = paste0('At Least One Effluent Violations n = ', format(sum(plot_df$five_years_effluent_violation), big.mark = ',')),
           color = colors[5],
           size = 2.5,
           fontface =2) +
  annotate('text', x= -118.6, y= 28.2,
           label = paste0('No Effluent Violations n = ', format(sum(!plot_df$five_years_effluent_violation), big.mark = ',')),
           color = colors[3],
           size = 2.5,
           fontface =2) +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 
ggsave(file.path(fig_dir, 'effluent_violation_map.pdf'),
       width = 6, height = 3)

# 3) common pollutant triggering violations and amount of pollution ----------

query <- "
with active_permits as(
select 
  extract(year from time_period_begin) as year, 
  npdes_permit_id
from analytics.permit_active_status_hist
  where time_period_desc = 'year'
  and permit_active_flag
),

permit_types as(
select distinct
  npdes_permit_id,
  individual_permit_flag,
  major_permit_flag,
  wastewater_permit_flag,
  stormwater_permit_flag,
  sewage_permit_flag
from icis.permits
), 

parameters as(
select 
  distinct
  parameter_code,
  snc_flag
from ontologies.icis__ref_parameter
),

eff_violation as(
select 
  extract (year from monitoring_period_end_date) as year,
  npdes_id as npdes_permit_id,
  parameter_code,
  parameter_desc,
  dmr_value_standard_units,
  unit_code,
  value_qualifier_code,
  exceedence_pct
from data_ingest.icis__npdes_eff_violations
where statistical_base_monthly_avg in ('N', 'A')
and violation_code = 'E90'
)

select *
from active_permits
left join permit_types
using (npdes_permit_id)
inner join eff_violation
using (year, npdes_permit_id)
left join parameters
using (parameter_code)
"

eff_violation <- dbSendQuery(con, query) %>%
  dbFetch()

plot_df <- eff_violation %>%
  group_by(parameter_code, parameter_desc, snc_flag) %>%
  summarise(violation_count = n(),
            pollutant_amount = sum(as.numeric(dmr_value_standard_units))) %>%
  arrange(-violation_count)

# 2. Enforcement Actions for Effluent Violations  --------------------------------------------------

# 1) Breakdown of enforcement action --------------------------------------

query <- "
with eff_violation as(
SELECT
  extract (year from monitoring_period_end_date) as year,
  npdes_id as npdes_permit_id,
  npdes_violation_id,
  parameter_code,
  parameter_desc,
  exceedence_pct,
  statistical_base_monthly_avg
from data_ingest.icis__npdes_eff_violations
where violation_code = 'E90'
),

enforcement_crosswalk as(
select 
  npdes_violation_id,
  violation_desc,
  activity_type_desc,
  enf_identifier
from data_ingest.icis__npdes_violation_enforcements
),

penalty_amount as(
select 
  enf_identifier,
  enf_type_code,
  enf_type_desc,
  agency,
  settlement_entered_date,
  fed_penalty_assessed_amt,
  state_local_penalty_amt
from data_ingest.icis__npdes_formal_enforcement_actions
),

active_permits as(
select 
  extract(year from time_period_begin) as year, 
  npdes_permit_id
from analytics.permit_active_status_hist
  where time_period_desc = 'year'
  and permit_active_flag
),

permit_types as(
select distinct
  npdes_permit_id,
  individual_permit_flag,
  major_permit_flag,
  wastewater_permit_flag,
  stormwater_permit_flag,
  sewage_permit_flag
from icis.permits
), 

parameters as(
select 
  distinct
  parameter_code,
  snc_flag
from ontologies.icis__ref_parameter
),

locations as(
 select npdes_permit_id, 
    geocode_latitude,
    geocode_longitude
 from icis.facilities
)

select *
from active_permits
inner join eff_violation 
using (year, npdes_permit_id)
left join parameters
using (parameter_code)
left join permit_types
using (npdes_permit_id)
left join locations
using (npdes_permit_id)
left join enforcement_crosswalk
using (npdes_violation_id)
left join penalty_amount
using (enf_identifier)
"

eff_enforcement <- dbSendQuery(con, query) %>%
  dbFetch()

# 46% effluent violations got matched to enforcment records
sum(!is.na(eff_enforcement$enf_identifier)) /nrow(eff_enforcement)

## subset to comparable facilities
sample_eff_enforcement <- eff_enforcement %>%
  filter(!is.na(geocode_latitude)) %>%
  filter(individual_permit_flag == 1) %>%
  filter(major_permit_flag == 1) %>%
  filter(wastewater_permit_flag == 1) %>%
  filter(sewage_permit_flag == 1) %>%
  filter(between(year, 2000, 2019))

# 43% effluent violations got matched to enforcment records
sum(!is.na(sample_eff_enforcement$enf_identifier)) /nrow(sample_eff_enforcement)

# breakdown of enforcement types: informal vs formal
table(sample_eff_enforcement$activity_type_desc)

plot_df <- sample_eff_enforcement %>%
  filter(!is.na(activity_type_desc)) %>%
  group_by(activity_type_desc) %>%
  summarise(n = n())

ggplot(plot_df, aes(y = n, x = activity_type_desc, fill = activity_type_desc)) +
  geom_bar(stat = 'identity')



  



