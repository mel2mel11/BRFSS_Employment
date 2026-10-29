# 01_IMPORT.R
#
# BRFSS: Import each year's raw file and combine into one master dataset
# Import 2023–2025 BRFSS annual XPT files.
# Every year gets a `year` column
#
# DIRECTIONS FOR DATA DOWNLOAD (repeat per year):
# https://www.cdc.gov/brfss/annual_data/annual_20XX.html
# Save as data/raw/20XXBRFSS.XPT.zip

library(haven)
library(dplyr)
library(purrr)

dir.create("data/processed",  recursive = TRUE, showWarnings = FALSE)

years <- 2023:2025
# check varaible names against annual codebooks.
core <- c('EMPLOY1','INCOME3','EDUCA','GENHLTH','MENTHLTH','PHYSHLTH',
          '_AGE80', '_IMPRACE','SEXVAR','_STATE','_LLCPWT','_STSTR','_PSU',
          'MARITAL','CHILDREN','_METSTAT','_BMI5CAT','CHECKUP1',
          '_SMOKER3','_TOTINDA')

# Year-specific variable names (not needed for imputation itself).
by_year <- list('2023'=c(insurance='_HLTHPL1',alcohol='_RFDRHV8'),
                '2024'=c(insurance='_HLTHPL2',alcohol='_RFDRHV9'),
                '2025'=c(insurance='_HLTHPL2',alcohol='_RFDRHV9'))

read_year <- function(yr) {
  # note: need to change filepath for reproducibility
  raw_dir <- "C:/Users/Melodie Nekoorad/Documents/Research/BRFSS/data/raw"
  path <- file.path(raw_dir, paste0(yr, "BRFSS.XPT.zip"))
  if (!file.exists(path)) stop('Missing raw XPT file: ',path)
  
  raw <- haven::read_xpt(path)
  expected <- c(core, unname(by_year[[as.character(yr)]]))
  absent <- setdiff(expected,names(raw))
  
  # make sure all variables are present
  if(length(absent)) stop(yr,': missing expected variables: ',paste(absent,collapse=', '))
  out <- raw %>% select(all_of(core)) %>% mutate(year=as.integer(yr))
  out$insurance_cdc <- raw[[by_year[[as.character(yr)]][['insurance']]]]
  out$alcohol_cdc <- raw[[by_year[[as.character(yr)]][['alcohol']]]]
  out
}

master <- purrr::map_dfr(years,read_year)
print(master %>% count(year,name='respondents'))
saveRDS(master,'data/processed/01_master.rds')
