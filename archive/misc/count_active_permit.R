

##################
# CA MMP Analysis
## Count Active Permits by State
##################

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
output.dir <- file.path(analysis.dir, 'output')

# read data ---------------------------------------------------------------

download_date <- '2020-07-07_14-57-43'
data.dir.raw <- file.path('Data', 'raw', download_date)

# permits 
permits <- read_csv(file.path(data.dir.raw, 'npdes', 'ICIS_PERMITS.csv'), 
                    col_types=cols(.default='c')) %>%
  mutate(state_code = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) %>%
  filter(PERMIT_TYPE_CODE %in% c('NPD', 'GPC')) %>%
  mutate(PERMIT_TYPE_CODE = ifelse(PERMIT_TYPE_CODE == 'NPD', 'individual', 'general')) %>%
  mutate(MAJOR_MINOR_STATUS_FLAG = ifelse(MAJOR_MINOR_STATUS_FLAG == 'M', 'Major', 'Minor')) %>%
  mutate(EFFECTIVE_DATE = mdy(EFFECTIVE_DATE),
         EXPIRATION_DATE = mdy(EXPIRATION_DATE),
         RETIREMENT_DATE = mdy(RETIREMENT_DATE),
         TERMINATION_DATE = mdy(TERMINATION_DATE)) 


# output ------------------------------------------------------------------

tmp <- permits %>%
  group_by(state_code, EXTERNAL_PERMIT_NMBR, VERSION_NMBR, EFFECTIVE_DATE, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
  # use the maximum of expiration/ retirement/ termination date because permits can be admin continued even if it is expired
  mutate(effective_end_date = max(EXPIRATION_DATE, RETIREMENT_DATE, TERMINATION_DATE, na.rm = TRUE)) %>%
  mutate(effective_start_year_quarter = quarter(EFFECTIVE_DATE, with_year = TRUE, fiscal_start = 10),
         effective_end_year_quarter = quarter(effective_end_date, with_year = TRUE, fiscal_start = 10))

# build frame
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

# function to output active permit count for each quarter
count_active_permits <- function(df, quarter){
  message(quarter)
  df %>%
    filter(effective_start_year_quarter <= quarter &
             (effective_end_year_quarter >= quarter | is.na(effective_end_year_quarter))) %>%
    mutate(year_quarter = quarter) %>%
    group_by(year_quarter, state_code, PERMIT_TYPE_CODE, MAJOR_MINOR_STATUS_FLAG) %>%
    summarise(active_permit_count = n())
}

# # test
# count_active_permits(df = tmp, quarter = 2010.1)

permit.count <- lapply(quarters, count_active_permits, df = tmp) %>%
  do.call(rbind, .)
# the numbers largely match with the California data

write_csv(permit.count, file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

# plot --------------------------------------------------------------------

permit.count <- read_csv(file.path(output.dir, 'active_permit_count_by_quarter_state.csv'))

plot.df <- permit.count %>%
  filter(state_code == 'CA') 
ggplot(plot.df, aes(x = year_quarter, y = active_permit_count, group = MAJOR_MINOR_STATUS_FLAG, color = MAJOR_MINOR_STATUS_FLAG)) +
  geom_line() +
  facet_wrap(~PERMIT_TYPE_CODE, scales = 'free')

