# 02_CLEAN.R
#
# Clean BRFSS 2023-2025 variables
# Study population limited to adults ages 18-64


library(dplyr)


# 1. load data
# again, change filepath
d <- readRDS("C:/Users/Melodie Nekoorad/Documents/Research/BRFSS/ReshapeBRFSS/brfss_rebuild/data/processed/01_master.rds")

cat("Original sample size:", nrow(d), "\n")


# 2. save original data
# Useful for checking original missing responses and documentation
saveRDS(d, 'data/processed/02_original_selected.rds')

# 3. Clean BRFSS Responses
# keep valid response codes, convert everything else to NA
# NA includes refused, don't know, and invalid responses

valid_or_na <- function(x, valid) {
  ifelse(x %in% valid, as.numeric(x), NA_real_)
}

d <- d %>%
  mutate(
    
    # _____ EMPLOYMENT _____
    # EMPLOY1:
    # 1 = Employed for wages
    # 2 = Self-employed
    # 3-4 = Unemployed
    # 5-8 = Not in labor force
    EMPLOY1 = valid_or_na(EMPLOY1, 1:8),
    
    
    # _____ HOUSEHOLD INCOME _____
    # keep original income categories,
    # groups will be created in Script 04.
    INCOME3 = valid_or_na(INCOME3, 1:11),
    
    
    # _____ EDUCATION _____
    # EDUCA:
    # 1 = Never attended school or only kindergarten
    # 2 = Elementary
    # 3 = Some high school
    # 4 = High school graduate
    # 5 = Some college or technical school
    # 6 = College graduate
    EDUCA = valid_or_na(EDUCA, 1:6),
    
    
    # _____ GENERAL HEALTH _____
    # This is an outcome and will NOT be imputed.
    # GENHLTH:
    # 1 = Excellent
    # 2 = Very good
    # 3 = Good
    # 4 = Fair
    # 5 = Poor
    GENHLTH = valid_or_na(GENHLTH, 1:5),
    
    
    #_____ MENTAL HEALTH _____
    # This is an outcome and will NOT be imputed.
    # 88 means zero days.
    MENTHLTH = case_when(
      MENTHLTH == 88 ~ 0,
      MENTHLTH %in% 1:30 ~ as.numeric(MENTHLTH),
      TRUE ~ NA_real_
    ),
    
    
    #_____ PHYSICAL HEALTH _____
    # auxiliary imputation predictor.
    # 88 means zero days.
    PHYSHLTH = case_when(
      PHYSHLTH == 88 ~ 0,
      PHYSHLTH %in% 1:30 ~ as.numeric(PHYSHLTH),
      TRUE ~ NA_real_
    ),
    
    
    #_____ MARITAL STATUS _____
    # Script 04 will combine into groups
    # 1 = Married
    # 2 = Divorced
    # 3 = Widowed
    # 4 = Separated
    # 5 = Never married
    # 6 = Unmarried couple
    MARITAL = valid_or_na(MARITAL, 1:6),
    
    
    #_____ CHILDREN _____
    # 88 = No children
    # 99 = Refused
    CHILDREN = case_when(
      CHILDREN == 88 ~ 0,
      CHILDREN %in% 1:87 ~ as.numeric(CHILDREN),
      TRUE ~ NA_real_
    ),
    
    
    #_____ SMOKING _____
    # Script 04 will combine codes 1 and 2 into Current.
    # _SMOKER3:
    # 1 = Current smoker, every day
    # 2 = Current smoker, some days
    # 3 = Former smoker
    # 4 = Never smoked
    #
    smoking_code = valid_or_na(`_SMOKER3`, 1:4),
    
    
    #_____ HEAVY DRINKING _____
    # alcohol_cdc was created in Script 01 using
    # 2023: _RFDRHV8
    # 2024: _RFDRHV9
    # 2025: _RFDRHV9
    #
    # 1 = No heavy drinking
    # 2 = Heavy drinking
    alcohol_code = valid_or_na(alcohol_cdc, 1:2),
    
    
    # DEMOGRAPHIC VARAIBLES
    age = valid_or_na(`_AGE80`, 18:80),
    
    race = valid_or_na(`_IMPRACE`, 1:6),
    
    sex = valid_or_na(SEXVAR, 1:2),
    
    state = as.integer(`_STATE`),
    
    
    # IMPUTATION PREDICTOR
    # Log survey weights are retained as predictors
    # in the MICE imputation model.
    log_weight = if_else(
      is.finite(`_LLCPWT`) & `_LLCPWT` > 0,
      log(`_LLCPWT`),
      NA_real_
    ),
    

    # SURVEY DESIGN VARAIBLES
    # Keep survey strata and PSUs across years.
    strata_year = paste(
      year,
      `_STSTR`,
      sep = '_'
    ),
    
    psu_year = paste(
      year,
      `_STSTR`,
      `_PSU`,
      sep = '_'
    )
  )


# 4. CHECK NON-IMPUTED VARAIBLES
# These variables are not being imputed, check completeness

stopifnot(
  all(!is.na(d$age)),
  all(!is.na(d$race)),
  all(!is.na(d$sex)),
  all(!is.na(d$state)),
  all(!is.na(d$log_weight)),
  all(!is.na(d$`_STSTR`)),
  all(!is.na(d$`_PSU`))
)


# 5. RESTRICT POP'N TO ADULTS 18-64
d <- d %>%
  filter(
    age >= 18,
    age <= 64
  )

cat("Eligible adults ages 18-64:", nrow(d), "\n")


# 6. SURVEY WEIGHTS
# Pooled three-year weight is used for combined 2023-2025 analyses
d$weight_3yr <- d$`_LLCPWT` / 3

# Original annual survey weight is used for models stratified by survey year.
d$weight_annual <- d$`_LLCPWT`


# 7. SUMMARY OF MISSINGNESS
# Check which varaibles have missing values, before running imputation
missing_vars <- c(
  'EMPLOY1',
  'INCOME3',
  'EDUCA',
  'MARITAL',
  'CHILDREN',
  'smoking_code',
  'alcohol_code',
  'GENHLTH',
  'MENTHLTH',
  'PHYSHLTH'
)

missing <- d %>%
  group_by(year) %>%
  summarise(
    n = n(),
    
    across(
      all_of(missing_vars),
      ~ round(mean(is.na(.x)) * 100, 2),
      .names = 'missing_pct_{.col}'
    ),
    
    .groups = 'drop'
  )

print(missing)

# 8. Save outputs
write.csv(missing, 'data/processed/02_missingness.csv', row.names = FALSE)
saveRDS(d, 'data/processed/02_clean.rds')

# 9. Script Complete Message
message("Script 02 complete.")
