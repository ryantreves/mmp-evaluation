###############################################################################
# Exploratory script that runs difference-in-differences models on
# California violation data.
#
# Author(s): Nicole Lin
#
# TO DO:
#   -
#
###############################################################################

# set-up ----------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('lubridate', 'stringr', 'tidyverse')

# prepare data ----------------------------------------------------------------

# read data, output from notebook: https://github.com/reglab/ca_mmp/blob/main/notetbooks/%20ca_violation_enforcement_data.ipynb
data_dir <- file.path('~', 'sherlock_oak', 'EPA', 'Data', 'processed', 'california')
violations <- read_csv(file.path(data_dir, 'violations_2022-01-13.csv'))
facilities <- read_csv(file.path(data_dir, 'facilities_2022-01-13.csv'))
wwtp_consistent <- read_csv(file.path(data_dir, 'wwtp_consistent_2022-01-13.csv'))

# prepare violation data
## conditional on WWTP that were active before and after WWTP
## subset to effluent MMP violations
## flag treatment group
## flag treatment time

viol_wwtp <- violations %>%
  filter(WDID_x %in% wwtp_consistent$WDID) %>%
  filter(`VIOLATION TYPE` == 'Effluent') %>%
  filter(mmp_flag)
nrow(viol_wwtp)
nrow(violations)




# descriptive plots  ----------------------------------------------------------


# run models ------------------------------------------------------------------
