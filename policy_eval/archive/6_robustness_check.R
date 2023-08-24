
####################
# CA MMP Analysis
## Robustness Check
####################

## This script generates robustness check for the synethic control method for MMP 

# set up ------------------------------------------------------------------

# configuration: set working directories and global variables
source('configs.R')

library(pacman)
p_load('here', 'lubridate', 'stringr', 'tidyverse')

synth_dir <- SYNTH_DIR
robustness_dir <- file.path(synth_dir, 'robustness_check')

# run specific robustness check
# if not specified: run all robustness checks
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0){
  args <- c('run', 'donor', 'loot', 'predictor')
}

# 1) Create list of tests -------------------------------------------------

designs <- list(
  'donor' = c(
    'donor_all weighted_permit_yearly 1 manual_1996_2003 complete_data none pretreatment_outcome_with_diffs_and_state_characteristics',
    'donor_consistent_with_territories weighted_permit_yearly 1 manual_1996_2003 consistent_reporting none pretreatment_outcome_with_diffs_and_state_characteristics'
  ),
  'predictor' = c(
    'predictor_outcome_only weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories pretreatment_outcome_with_diffs',
    'predictor_outcome_state_feature weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories pretreatment_outcome_and_state_characteristics'
  ),
  'loot' = c(
    'loot_NH weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories_and_NH pretreatment_outcome_with_diffs_and_state_characteristics',
    'loot_OK weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories_and_OK pretreatment_outcome_with_diffs_and_state_characteristics',
    'loot_RI weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories_and_RI pretreatment_outcome_with_diffs_and_state_characteristics',
    'loot_UT weighted_permit_yearly 1 manual_1996_2003 consistent_reporting remove_territories_and_UT pretreatment_outcome_with_diffs_and_state_characteristics'
  )
  
)


# 2) Create function to run robustness check for each study design --------

output_robustness_check <- function(group, designs, run){
  
  if (run){
    # 1) move existing subfolders to old 
    result_subdirs <- list.dirs(path = synth_dir,
                                full.names = FALSE,
                                recursive = FALSE) 
    old_subdirs <- result_subdirs[grepl(group, result_subdirs, fixed = TRUE)] 
    sapply(old_subdirs, function(x){
      system(paste0('mv ', file.path(synth_dir, x), ' ', file.path(synth_dir, 'old')))
    })
    
    # 2) run synthetic control for each design
    design_names <- paste('Rscript ', here(), '5_synthetic_control.R', designs[[group]], sep = ' ')
    sapply(design_names, system)
  }
  
  
  # 3) output design comparison table
  result_subdirs <- list.dirs(path = synth_dir,
                              full.names = FALSE,
                              recursive = FALSE)
  group_dirs <- result_subdirs[grepl(group, result_subdirs, fixed = TRUE)]
  robustness <- lapply(group_dirs, function(x){
    if (file.exists(file.path(synth_dir, x, 'main_results.csv'))){
      read_csv(file.path(synth_dir, x, 'main_results.csv'))
    } else{
      NULL
    }
  }) %>%
    bind_rows()
  
  write_csv(robustness, file.path(robustness_dir, paste0(group, '.csv')))
  
}

# 3) Run robustness check -------------------------------------------------

run <- args[1] == 'run'
groups <- args[2:length(args)]

sapply(groups, output_robustness_check, designs = designs, run = run)

