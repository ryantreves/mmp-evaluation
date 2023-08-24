##########
# configuration 
## define global variables and directories
##########

# install pacman: a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# set working directory to be $OAK/EPA on sherlock 
OAK_DIR <- Sys.getenv('OAK')
HOME_DIR <- Sys.getenv('HOME')
setwd(file.path(OAK_DIR, 'EPA'))

# analysis folder
PROJECT_DIR <- file.path('Analysis', 'ca_mmp')
DATA_OUTPUT_DIR <- file.path(PROJECT_DIR, 'data')
SYNTH_DIR <- file.path(PROJECT_DIR, 'synthetic_control')
FIGURE_DIR <- file.path(PROJECT_DIR, 'figures')

# data folder
DOWNLOAD_DATE <- '2020-07-07_14-57-43'
DATA_RAW_DIR <- file.path('Data', 'raw', DOWNLOAD_DATE)
DATA_MANUAL_DIR <- file.path('Data', 'manual')

# treatment state
STATE <- 'CA'

