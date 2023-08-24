
# Setup
library(tidyverse)
library(yaml)

#  First, download the full list of NPDES permittees in regions 2, 3, 4, and 5
#' from CIWQS:
#' A) Navigate to https://ciwqs.waterboards.ca.gov/ciwqs/readOnly/CiwqsReportServlet?inCommand=reset&reportName=RegulatedFacility
#' B) Select Regions 2, 3, 4, 5F, 5S, and 5R; Program = NPDES; and
#'  Related Permit Status = 'Any'; Click 'Run Report.'
#' C) Click the number under total facilities across all regions (should be 4825).
#' D) Click 'EXPORT THIS REPORT TO EXCEL' at the top.

# Next, load in the full list
data_path <- read_yaml('config.yml', eval.expr=T)$CIWQS_data_path
facilities_list <- read_csv(paste0(data_path, '/region_2-3-4-5_facilities.csv'))

# Filter out general and co-permittees, facilities with no permit number on
# record, and data errors
facilities_list <- facilities_list %>%
  filter(!substr(`NPDES No.`, 3, 3) %in% c('G', 'C', '-'),
         `NPDES No.` != 'null') %>%
  # Select the unique NPDES IDs
  distinct(`NPDES No.`) %>%
  # Save the list of facilities
  write_csv('treatment_regions_facilities.csv')


