
####################
# CA MMP Analysis
## Preliminary Difference in Differences
####################

# set up ------------------------------------------------------------------

OAK_DIR <- Sys.getenv('OAK')
setwd(file.path(OAK_DIR, 'EPA'))

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 'lubridate', 'stringr', 'tidyverse')

# analysis folder
analysis.dir <- file.path('Analysis', 'ca_mmp')
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

# active permit count: output using output_eff_dmr_submitters.R
permit.count <- read_csv(file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

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
         state_code = substr(NPDES_ID, 1, 2),
         violation_type = ifelse(VIOLATION_CODE %in% c('D80', 'D90'), 'Reporting', 'Effluent')) %>%
  # remove data errors
  filter(MONITORING_PERIOD_END_DATE <= Sys.Date())

# Analysis ----------------------------------------------------------------


# 1) calculate violation rate (using active permit count as denomi --------

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


# 2) run diff-in-diff for state pairs -------------------------------------
## which states have similar trend with California pre-2000 but diverged after 2000? 

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


# 3) select state candidate: significant (at the 0.05 level) negat --------

did.results.present <- did.results %>%
  filter(did_estimate < 0 & p < 0.05) %>%
  arrange(did_estimate) 
did.candidates <- did.results.present$state_code


# 4) plot violation rate comparison  --------------------------------------

## plot candidates
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

## generate comparison graphs for candidate states vs CA
## add on "similar" states: geographically close or EPA with more capacity 
did.candidates <- c(did.candidates, 'NY', 'NV', 'AZ', 'OR')
state.pairs <- list()
for (i in 1:length(did.candidates)){
  state.pairs[[i]] <- c('CA', did.candidates[i])
}

lapply(state.pairs, plot_violation_rate_comparison, df = violation.rate)

# which candidates are more similar with CA? 
tmp <- violation.rate %>%
  filter(state_code %in% c('CA', did.candidates)) %>%
  filter(quarter == 2000.2) %>%
  filter(PERMIT_TYPE_CODE == 'individual') %>%
  left_join(did.results.present) %>%
  arrange(active_permit_count)


